# ---- UI ----
mod_log_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    role = "region",
    verbatimTextOutput(ns("log"), placeholder = TRUE)
  )
}

# ---- SERVER ----
# Added `prep_meta` param; kept existing params for backward compatibility.
# You can drop `metrics` (legacy) once all callers switch to `prep_meta`.
mod_log_server <- function(
  id,
  shaped_input,
  raw_file,
  df87,
  init,
  prep_meta = NULL, # NEW: performance/prepare run metrics from mod_prepare_server
  metrics = NULL # LEGACY: old mod_performance metrics block (optional)
) {
  moduleServer(id, function(input, output, session) {
    `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

    output$log <- renderPrint({
      rf <- raw_file()

      # --- Prepare logs & provenance ---
      if (!is.null(rf)) {
        prep_logs <- attr(rf, "prep_log")
        prov <- attr(rf, "provenance")
        if (!is.null(prep_logs) && length(prep_logs)) {
          cat("\n--- Prepare Log ---\n")
          for (ln in prep_logs) cat(ln, "\n")
        }
        if (!is.null(prov) && is.list(prov)) {
          cat("\n--- Prepare Provenance ---\n")
          for (nm in names(prov)) cat(sprintf("%s: %s\n", nm, prov[[nm]]))
        }
        cat("\nRows read:", nrow(rf), "\n")
      } else {
        cat("No prepared data available.\n")
      }

      # --- Prepare run metrics (NEW) ---
      if (!is.null(prep_meta)) {
        pm <- try(prep_meta(), silent = TRUE)
        if (!inherits(pm, "try-error") && is.list(pm)) {
          cat("\n--- Prepare run metrics ---\n")
          cat(sprintf(
            "Run ID: %s | kind=%s | tz=%s | offset_policy=%s | diurnal=%s\n",
            pm$run_id %||% NA_integer_,
            pm$kind %||% "?",
            pm$tz %||% "?",
            pm$offset_policy %||% "?",
            pm$diurnal %||% "?"
          ))
          cat(sprintf(
            "Rows: in=%s out=%s | stations=%s\n",
            pm$n_rows_in %||% NA_integer_,
            pm$n_rows_out %||% NA_integer_,
            pm$station_count %||% NA_integer_
          ))
          # timings in ms
          cat(sprintf(
            "Timing (ms): detect=%0.1f convert=%0.1f total=%0.1f\n",
            pm$t_detect_ms %||% NA_real_,
            pm$t_convert_ms %||% NA_real_,
            pm$t_total_ms %||% NA_real_
          ))
          # stamps
          started <- pm$started_at %||% Sys.time()
          finished <- pm$finished_at %||% Sys.time()
          cat(sprintf(
            "Start:   %s\nFinish:  %s\n",
            format(started, "%Y-%m-%d %H:%M:%S"),
            format(finished, "%Y-%m-%d %H:%M:%S")
          ))

          # If you want to surface logs again here, uncomment:
          # if (!is.null(pm$log) && length(pm$log)) {
          #   cat("\n[Prepare log items]\n")
          #   for (ln in pm$log) cat(ln, "\n")
          # }
        }
      }

      # --- Existing shaped-input summary ---
      si <- shaped_input()
      if (!is.null(si$start_date) && !is.na(si$start_date)) {
        cat("Start-date filter (local):", as.character(si$start_date), "\n")
      } else {
        cat("Start-date filter: (none)\n")
      }
      cat("Rows after filtering:", si$n_rows %||% NA_integer_, "\n")
      cat("Time zone used:", si$tz %||% "?", "\n")
      cat("GMT offset (hours):", si$tz_offset %||% NA_integer_, "\n")
      cat("Standard %z probe:", si$diag_std_z %||% "?", "\n")
      if (!is.null(si$diag_modal_z) && nzchar(si$diag_modal_z)) {
        cat("Modal %z:", si$diag_modal_z, "\n")
      }
      cat("Initial codes: FFMC =", init$ffmc0(), " DMC =", init$dmc0(), " DC =", init$dc0(), "\n")

      # --- Legacy mod_performance block (optional, retained) ---
      if (!is.null(metrics)) {
        m <- metrics()
        cat("\n--- Performance metrics (legacy) ---\n")
        cat(
          "Last event:", m$last_event %||% NA_character_, "@",
          format(m$stamp %||% Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"
        )
        cat(sprintf(
          "Shape: %6.1f ms \nModel: %6.1f ms \nFWI87: %6.1f ms\n",
          m$t_shape_ms %||% NA_real_,
          m$t_model_ms %||% NA_real_,
          m$t_daily_ms %||% NA_real_
        ))
        cat("Rows processed:", m$n_rows %||% NA_integer_, "\n")
      }

      # --- Noon–noon precipitation sanity check (unchanged) ---
      cat("\n--- Sanity check: noon-noon precipitation ---\n")
      tz_use <- if (is.null(si$tz) || !nzchar(si$tz)) "UTC" else si$tz
      wx <- data.table::as.data.table(as.data.frame(si$inputs))
      if (!"datetime" %in% names(wx)) {
        if (all(c("year", "month", "day", "hour") %in% names(wx))) {
          wx[, datetime := lubridate::make_datetime(
            year = as.integer(year), month = as.integer(month),
            day = as.integer(day), hour = as.integer(hour), tz = tz_use
          )]
        } else {
          cat(" (Skipping: couldn't find datetime or year/month/day/hour.)\n")
          return(invisible())
        }
      }
      if (!"hour" %in% names(wx)) wx[, hour := lubridate::hour(datetime)]
      wx[, date := as.Date(datetime, tz = tz_use)]
      if (!"rain" %in% names(wx)) {
        pcol <- find_precip_col(names(wx))
        if (is.null(pcol)) {
          cat(" (Skipping: couldn't find precipitation column.)\n")
          return(invisible())
        }
        data.table::setnames(wx, pcol, "rain")
      }
      wx[, for_date := ifelse(hour <= 12L, date, date + 1L)]
      daily_chk <- wx[, .(
        rain_24   = sum(rain, na.rm = TRUE),
        n_rows    = .N,
        n_non_na  = sum(!is.na(rain)),
        n_prev    = sum(hour > 12L),
        n_am      = sum(hour <= 12L)
      ), by = for_date][order(for_date)]
      data.table::setnames(daily_chk, "for_date", "date")
      daily_chk[, date := as.Date(date)]
      print(daily_chk)

      cat("\nNearest-to-noon selection dHour (count):\n")
      noon_tbl <- data.table::as.data.table(
        nearest_noon_per_day(as.data.frame(wx), dt_col = "datetime", hour_col = "hour", tz = tz_use)
      )
      noon_tbl[, noon_hour := lubridate::hour(datetime)]
      delta_tab <- sort(table(noon_tbl$noon_hour - 12L), decreasing = TRUE)
      print(delta_tab)

      bad <- daily_chk[n_non_na < 24L]
      if (nrow(bad)) {
        cat("\nWARNING: days with < 24 hourly precip values in the noon-noon window:\n")
        print(utils::head(bad, 10))
      } else {
        cat("\nAll noon-noon windows have 24 hourly precip values.\n")
      }
      cat(
        "\nLST sanity: standard offset (h) =", tz_standard_offset_hours(si$tz),
        " current sample %z =", format(head(si$inputs$datetime, 1), "%z"), "\n"
      )

      invisible(NULL)
    })
  })
}
