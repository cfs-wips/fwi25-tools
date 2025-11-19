# R/modules/mod_log.R
# v2 + Prepare integration (non-breaking)
mod_log_ui <- function(id){
  ns <- NS(id)
  tags$div(role = "region",
           verbatimTextOutput(ns("log"), placeholder = TRUE))
}

mod_log_server <- function(id, shaped_input, raw_file, df87, init, metrics = NULL){
  moduleServer(id, function(input, output, session){
    `%||%` <- function(a,b) if (is.null(a) || is.na(a)) b else a
    
    output$log <- renderPrint({
      si <- shaped_input()
      rf <- req(raw_file())
      
      # -----------------------------------------------------------------------
      # Prepare provenance & logs from mod_prepare_server (optional attributes)
      # -----------------------------------------------------------------------
      prov <- tryCatch(attr(rf, "provenance"), error = function(e) NULL)
      prep <- tryCatch(attr(rf, "prep_log"),   error = function(e) NULL)
      
      # Top line: rows read (from prepared or passthrough raw_file)
      cat("Rows read:", nrow(rf), "\n")
      
      # Status line
      if (!is.null(prov) && is.list(prov)) {
        if (identical(prov$source, "daily")) {
          cat("Status: DAILY upload detected \u2192 HOURLY conversion enacted.\n")
        } else {
          cat("Status: HOURLY upload (no conversion).\n")
        }
      }
      
      # Provenance block
      if (!is.null(prov) && is.list(prov)) {
        cat("\n--- Prepare provenance -----------------------------------------\n")
        if (!is.null(prov$source))       cat("Source:           ", prov$source, "\n", sep = "")
        if (!is.null(prov$conversion))   cat("Conversion:       ", prov$conversion, "\n", sep = "")
        if (!is.null(prov$tz))           cat("Time zone (prep): ", prov$tz, "\n", sep = "")
        if (!is.null(prov$offset_hours)) cat("Std offset (h):   ", prov$offset_hours, "\n", sep = "")
        if (!is.null(prov$prepared_at))  cat("Prepared at:      ", as.character(prov$prepared_at), "\n", sep = "")
      }
      
      # Prepare log block
      if (!is.null(prep) && length(prep)) {
        cat("\n--- Prepare log -------------------------------------------------\n")
        for (ln in prep) cat(ln, "\n")
      }
      
      # -----------------------------------------------------------------------
      # Existing shaped-input summary (unchanged)
      # -----------------------------------------------------------------------
      if (!is.null(si$start_date) && !is.na(si$start_date))
        cat("Start-date filter (local):", as.character(si$start_date), "\n")
      else
        cat("Start-date filter: (none)\n")
      
      cat("Rows after filtering:", si$n_rows, "\n")
      cat("Time zone used:", si$tz, "\n")
      cat("GMT offset (hours) passed to make_inputs():", si$tz_offset, "\n")
      cat("Standard %z probe:", si$diag_std_z, "\n")
      if (!is.null(si$diag_modal_z) && nzchar(si$diag_modal_z)) cat("Modal %z:", si$diag_modal_z, "\n")
      cat("Initial codes: FFMC =", init$ffmc0(), " DMC =", init$dmc0(), " DC =", init$dc0(), "\n")
      
      # -----------------------------------------------------------------------
      # Performance metrics (unchanged)
      # -----------------------------------------------------------------------
      if (!is.null(metrics)){
        m <- metrics()
        cat("\n--- Performance metrics -----------------------------------------\n")
        cat("Last event: ", m$last_event %||% NA_character_, " @ ",
            format(m$stamp %||% Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
        cat(sprintf("Shape: %6.1f ms \nModel: %6.1f ms \nFWI87: %6.1f ms\n",
                    m$t_shape_ms %||% NA_real_,
                    m$t_model_ms %||% NA_real_,
                    m$t_daily_ms %||% NA_real_))
        cat("Rows processed: ", m$n_rows %||% NA_integer_, "\n", sep = "")
        cat("Key (shape): ");  str(m$key_shape,  max.level = 1); cat("\n")
        cat("Key (model): ");  str(m$key_model,  max.level = 1); cat("\n")
        cat("Key (daily87): ");str(m$key_daily,  max.level = 1); cat("\n")
      }
      
      # -----------------------------------------------------------------------
      # Sanity check: noon-noon precipitation (from hourly inputs) (unchanged)
      # -----------------------------------------------------------------------
      cat("\n--- Sanity check: noon-noon precipitation (from hourly inputs) ---\n")
      tz_use <- if (is.null(si$tz) || !nzchar(si$tz)) "UTC" else si$tz
      wx <- data.table::as.data.table(as.data.frame(si$inputs))
      
      if (!"datetime" %in% names(wx)){
        if (all(c("year","month","day","hour") %in% names(wx))){
          wx[, datetime := lubridate::make_datetime(
            year = as.integer(year), month = as.integer(month),
            day  = as.integer(day),  hour  = as.integer(hour), tz = tz_use)]
        } else { cat(" (Skipping: couldn't find datetime or year/month/day/hour.)\n"); return(invisible()) }
      }
      if (!"hour" %in% names(wx)) wx[, hour := lubridate::hour(datetime)]
      wx[, date := as.Date(datetime, tz = tz_use)]
      
      if (!"rain" %in% names(wx)){
        pcol <- find_precip_col(names(wx))
        if (is.null(pcol)){ cat(" (Skipping: couldn't find precipitation column.)\n"); return(invisible()) }
        data.table::setnames(wx, pcol, "rain")
      }
      
      wx[, for_date := ifelse(hour <= 12L, date, date + 1L)]
      daily_chk <- wx[, .(
        rain_24 = sum(rain, na.rm = TRUE),
        n_rows  = .N,
        n_non_na = sum(!is.na(rain)),
        n_prev = sum(hour > 12L),
        n_am   = sum(hour <= 12L)
      ), by = for_date][order(for_date)]
      data.table::setnames(daily_chk, "for_date", "date"); daily_chk[, date := as.Date(date)]
      print(utils::head(daily_chk, 10))
      
      cat("\nNearest-to-noon selection dHour (count):\n")
      noon_tbl <- data.table::as.data.table(
        nearest_noon_per_day(as.data.frame(wx), dt_col = "datetime", hour_col = "hour", tz = tz_use)
      )
      noon_tbl[, noon_hour := lubridate::hour(datetime)]
      delta_tab <- sort(table(noon_tbl$noon_hour - 12L), decreasing = TRUE); print(delta_tab)
      
      bad <- daily_chk[n_non_na < 24L]
      if (nrow(bad)) { cat("\nWARNING: days with < 24 hourly precip values in the noon-noon window:\n"); print(utils::head(bad,10)) }
      else { cat("\nAll noon-noon windows have 24 hourly precip values.\n") }
      
      cat("\nLST sanity: standard offset (h) =", tz_standard_offset_hours(si$tz),
          " current sample %z =", format(head(si$inputs$datetime, 1), "%z"), "\n")
      invisible(NULL)
    })
  })
}
