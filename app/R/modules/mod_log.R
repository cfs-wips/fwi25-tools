# R/modules/mod_log.R
mod_log_ui <- function(id){
  ns <- NS(id)
  tags$div(role = "region", `aria-label` = "Run log", verbatimTextOutput(ns("log"), placeholder = TRUE))
}

mod_log_server <- function(id, shaped_input, raw_file, df87, init){
  moduleServer(id, function(input, output, session){
    output$log <- renderPrint({
      si <- shaped_input()
      cat("Rows read:", nrow(req(raw_file())), "\n")
      if (!is.null(si$start_date) && !is.na(si$start_date)) cat("Start-date filter (local):", as.character(si$start_date), "\n") else cat("Start-date filter: (none)\n")
      cat("Rows after filtering:", si$n_rows, "\n")
      cat("Time zone used:", si$tz, "\n")
      cat("GMT offset (hours) passed to make_inputs():", si$tz_offset, "\n")
      cat("Standard %z probe:", si$diag_std_z, "\n")
      if (!is.null(si$diag_modal_z) && nzchar(si$diag_modal_z)) cat("Modal %z:", si$diag_modal_z, "\n")
      cat("Initial codes: FFMC =", init$ffmc0(), " DMC =", init$dmc0(), " DC =", init$dc0(), "\n")
      fml <- try(formals(hFWI), silent = TRUE); print(fml)
      argn <- if (inherits(fml, "try-error") || is.null(fml)) character(0) else names(fml)
      mapping <- if (all(c("df_wx","timezone") %in% argn)) "df_wx + timezone (NG)" else if ("inputs" %in% argn) "inputs (legacy)" else if ("df" %in% argn) "df (legacy)" else "positional fallback / unknown"
      cat("hFWI() call mapping -> ", mapping, "\n")
      ng_commit <- tryCatch(readLines("ng/_ng_commit.txt", warn = FALSE)[1], error = function(e) NA_character_)
      if (isTRUE(nzchar(ng_commit))) cat("cffdrs-ng commit:", ng_commit, "\n")
      d87 <- df87()
      if (is.null(d87)) cat("FWI87: not requested or not available\n") else cat("FWI87: rows =", nrow(d87), " (daily)\n")
      print(utils::head(d87,10))

      # Sanity check
      cat("\n--- Sanity check: noon→noon precipitation (from hourly inputs) ---\n")
      tz_use <- if (is.null(si$tz) || !nzchar(si$tz)) "UTC" else si$tz
      wx <- data.table::as.data.table(as.data.frame(si$inputs))
      if (!"datetime" %in% names(wx)){
        if (all(c("year","month","day","hour") %in% names(wx))){
          wx[, datetime := lubridate::make_datetime(year = as.integer(year), month = as.integer(month), day = as.integer(day), hour = as.integer(hour), tz = tz_use)]
        } else { cat(" (Skipping: couldn't find datetime or year/month/day/hour.)\n"); return(invisible()) }
      }
      if (!"hour" %in% names(wx)) wx[, hour := lubridate::hour(datetime)]
      wx[, date := as.Date(datetime, tz = tz_use)]
      if (!"rain" %in% names(wx)){
        pcol <- find_precip_col(names(wx)); if (is.null(pcol)){ cat(" (Skipping: couldn't find precipitation column.)\n"); return(invisible()) }
        data.table::setnames(wx, pcol, "rain")
      }
      wx[, for_date := ifelse(hour <= 12L, date, date + 1L)]
      daily_chk <- wx[, .(rain_24 = sum(rain, na.rm = TRUE), n_rows = .N, n_non_na = sum(!is.na(rain)), n_prev = sum(hour > 12L), n_am = sum(hour <= 12L)), by = for_date][order(for_date)]
      data.table::setnames(daily_chk, "for_date", "date"); daily_chk[, date := as.Date(date)]
      print(utils::head(daily_chk, 10))
      cat("\nNearest-to-noon selection Δhour (count):\n")
      noon_tbl <- data.table::as.data.table(nearest_noon_per_day(as.data.frame(wx), dt_col = "datetime", hour_col = "hour", tz = tz_use))
      noon_tbl[, noon_hour := lubridate::hour(datetime)]
      delta_tab <- sort(table(noon_tbl$noon_hour - 12L), decreasing = TRUE); print(delta_tab)
      bad <- daily_chk[n_non_na < 24L]
      if (nrow(bad)) { cat("\nWARNING: days with < 24 hourly precip values in the noon→noon window:\n"); print(utils::head(bad,10)) }
      else { cat("\nAll noon→noon windows have 24 hourly precip values.\n") }
      cat("\nLST sanity: standard offset (h) =", tz_standard_offset_hours(si$tz), " current sample %z =", format(head(si$inputs$datetime, 1), "%z"), "\n")
      invisible(NULL)
    })
  })
}
