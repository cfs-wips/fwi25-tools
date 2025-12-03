mod_prepare_server <- function(
  id,
  raw_file,
  mapping,
  tz,
  diurnal_method_reactive = reactive("BT-default"),
  skip_invalid = TRUE,
  notify = TRUE
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    emit_toast <- function(text, type = "message", duration = 5) {
      if (isTRUE(notify)) showNotification(text, type = type, duration = duration)
    }

    # --- Helpers ---
    source("ng/make_minmax.r")
    source("ng/make_hourly.r")
    convert_daily_to_hourly <- function(df, tz_string, diurnal_method) {
      logs <- character()
      if (!all(c("yr", "mon", "day") %in% names(df))) {
        stop("Missing year/month/day columns for daily conversion.")
      }
      tz_off <- as.integer(format(as.POSIXlt(Sys.time(), tz = tz_string), "%z")) / 100
      mm <- try(daily_to_minmax(df[, .(yr, mon, day, temp, rh, ws, prec)]), silent = TRUE)
      if (inherits(mm, "try-error")) stop(paste("daily_to_minmax() failed:", as.character(mm)))
      names(mm) <- tolower(names(mm))
      mm$id <- if ("id" %in% names(df)) df$id[1] else "STN"
      mm$lat <- if ("lat" %in% names(df)) df$lat[1] else NA_real_
      mm$long <- if ("long" %in% names(df)) df$long[1] else NA_real_
      args <- list(mm, timezone = tz_off, skip_invalid = TRUE, verbose = FALSE, round_out = NA)
      fml <- try(formalArgs(minmax_to_hourly), silent = TRUE)
      if (!inherits(fml, "try-error")) {
        if ("method" %in% fml) args$method <- diurnal_method
        if ("diurnal" %in% fml) args$diurnal <- diurnal_method
      }
      hr <- try(do.call(minmax_to_hourly, args), silent = TRUE)
      if (inherits(hr, "try-error")) stop(paste("minmax_to_hourly() failed:", as.character(hr)))
      list(hourly = hr, tz_off = tz_off, logs = logs)
    }

    convert_multi_station_daily_to_hourly <- function(df, tz_string, diurnal_method) {
      if (!"id" %in% names(df)) stop("Daily input must have an 'id' column.")
      stn_list <- split(df, df$id)

      # Gap detection (explicit days)
      ranges <- lapply(stn_list, function(stn) {
        stn$date <- as.Date(sprintf("%04d-%02d-%02d", stn$yr, stn$mon, stn$day))
        gaps <- any(diff(stn$date, units = "days") != 1) # Check day increments
        list(min_date = min(stn$date), max_date = max(stn$date), gaps = gaps)
      })
      gap_stations <- names(Filter(function(x) x$gaps, ranges))
      logs <- character()
      if (length(gap_stations)) {
        emit_toast(sprintf(
          "Daily sequence has gaps for stations: %s",
          paste(gap_stations, collapse = ", ")
        ), "warning", 6)
        logs <- c(logs, sprintf("[Prepare][WARN] Gaps detected for: %s", paste(gap_stations, collapse = ", ")))
      }

      # Conversion
      hourly_list <- lapply(stn_list, function(stn) {
        convert_daily_to_hourly(stn, tz_string, diurnal_method)
      })
      hourly <- data.table::rbindlist(lapply(hourly_list, `[[`, "hourly"), fill = TRUE)
      list(hourly = hourly, tz_off = hourly_list[[1]]$tz_off, logs = logs)
    }

    # --- Robust detection logic (original complexity preserved) ---
    detect_resolution <- function(df, tz_string) {
      dt <- data.table::as.data.table(df)
      cols <- names(dt)
      reasons <- character()

      # Order for checks
      if ("id" %in% cols) {
        data.table::setorder(dt, id, yr, mon, day, hr)
      } else {
        data.table::setorder(dt, yr, mon, day, hr)
      }

      make_ts <- function(y, m, d, h = 0L) {
        suppressWarnings(as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00", y, m, d, h), tz = tz_string))
      }

      # A) If mapped datetime is present
      dt_col <- mapping$col_datetime() %||% ""
      if (nzchar(dt_col) && dt_col %in% cols) {
        data.table::setnames(dt, dt_col, "timestamp", skip_absent = TRUE)
        if (!inherits(dt$timestamp, "POSIXt")) {
          dt[, timestamp := suppressWarnings(as.POSIXct(
            timestamp,
            tz = tz_string,
            tryFormats = c(
              "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
              "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
              "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%SZ"
            )
          ))]
        }
        ok <- FALSE
        try(ok <- is_sequential_hours(dt[, .(timestamp)]), silent = FALSE)
        if (isTRUE(ok)) {
          return(list(kind = "hourly", seq_ok = TRUE, reasons = character()))
        }
        reasons <- c(reasons, "datetime present but hourly sequentiality failed or unparsable")
      }

      # B) If Y/M/D/H present
      ycol <- mapping$col_year() %||% ""
      mcol <- mapping$col_month() %||% ""
      dcol <- mapping$col_day() %||% ""
      hcol <- mapping$col_hour() %||% ""
      have_ymdh <- nzchar(ycol) && nzchar(mcol) && nzchar(dcol) && nzchar(hcol) &&
        all(c(ycol, mcol, dcol, hcol) %in% cols)
      if (have_ymdh) {
        dt[, timestamp := make_ts(get(ycol), get(mcol), get(dcol), get(hcol))]
        ok <- TRUE
        if ("id" %in% names(dt)) {
          seq_check <- dt[, .(seq_ok = is_sequential_hours(.SD)), by = id, .SDcols = "timestamp"]
          if (any(!seq_check$seq_ok)) {
            ok <- FALSE
            showNotification("Some stations have gaps or reversed order", type = "warning", duration = 6)
          }
        } else {
          try(ok <- is_sequential_hours(dt[, .(timestamp)]), silent = FALSE)
        }
        if (isTRUE(ok)) {
          return(list(kind = "hourly", seq_ok = TRUE, reasons = character()))
        }
        reasons <- c(reasons, "Y/M/D/H present but hourly sequentiality failed")
      }

      # C) If date col present (daily)
      date_col <- mapping$col_date() %||% ""
      have_date_col <- nzchar(date_col) && date_col %in% cols
      if (have_date_col) {
        data.table::setnames(dt, date_col, "date", skip_absent = TRUE)
        dt[, date := suppressWarnings(as.Date(date))]
        ok <- TRUE
        if ("id" %in% names(dt)) {
          seq_check <- dt[, .(seq_ok = is_sequential_days(.SD)), by = id, .SDcols = "date"]
          if (any(!seq_check$seq_ok)) {
            ok <- FALSE
            showNotification("Some stations have gaps in daily sequence", type = "warning", duration = 6)
          }
        } else {
          try(ok <- is_sequential_days(dt[, .(date)]), silent = FALSE)
        }
        return(list(
          kind = "daily", seq_ok = isTRUE(ok),
          reasons = if (isTRUE(ok)) character() else "daily not sequential (date column)"
        ))
      }

      # D) If Y/M/D present (daily)
      have_ymd <- nzchar(ycol) && nzchar(mcol) && nzchar(dcol) &&
        all(c(ycol, mcol, dcol) %in% cols)
      if (have_ymd) {
        dt[, date := suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", get(ycol), get(mcol), get(dcol))))]
        ok <- TRUE
        if ("id" %in% names(dt)) {
          seq_check <- dt[, .(seq_ok = is_sequential_days(.SD)), by = id, .SDcols = "date"]
          if (any(!seq_check$seq_ok)) {
            ok <- FALSE
            showNotification("Some stations have gaps in daily sequence", type = "warning", duration = 6)
          }
        } else {
          try(ok <- is_sequential_days(dt[, .(date)]), silent = FALSE)
        }
        return(list(
          kind = "daily", seq_ok = isTRUE(ok),
          reasons = if (isTRUE(ok)) character() else "daily not sequential (Y/M/D)"
        ))
      }

      list(kind = "unknown", seq_ok = NA, reasons = reasons)
    }

    # --- Reactive values ---
    raw_like_rv <- reactiveVal(NULL)
    daily_src_rv <- reactiveVal(NULL)
    meta_rv <- reactiveVal(list(
      kind = NA, converted = FALSE, log = character(),
      t_detect_ms = NA_real_, t_convert_ms = NA_real_, t_total_ms = NA_real_,
      n_rows_in = NA_integer_, n_rows_out = NA_integer_, station_count = NA_integer_,
      run_id = NA_integer_, started_at = NA, finished_at = NA,
      tz = NA_character_, offset_policy = NA_character_, diurnal = NA_character_
    ))
    last_kind_rv <- reactiveVal(NULL)
    run_id_rv <- reactiveVal(0L)

    prep_ready <- eventReactive(
      list(raw_file(), mapping$col_temp(), tz$tz_use(), diurnal_method_reactive()),
      {
        req(raw_file(), nzchar(tz$tz_use()))

        if (is.null(tz$tz_use()) || !nzchar(tz$tz_use())) {
          return(invisible()) # Skip prepare until timezone is resolved
        }
        list(
          src = raw_file(), tz_string = tz$tz_use(),
          offset_policy = tz$tz_offset_policy(), diurnal = diurnal_method_reactive() %||% "BT-default"
        )
      },
      ignoreInit = TRUE
    )


    debounced_ready <- debounce(prep_ready, 500)

    observeEvent(debounced_ready(), {
      args <- debounced_ready()
      src <- as.data.table(args$src)
      tz_string <- args$tz_string
      offset_policy <- args$offset_policy
      dia <- args$diurnal
      started_at <- Sys.time()
      t_total_start <- proc.time()
      logs <- character()
      n_rows_in <- nrow(src)
      station_count <- if ("id" %in% names(src)) length(unique(src$id)) else 1L

      # Detection timing
      t_det_start <- proc.time()
      det <- detect_resolution(src, tz_string)
      t_detect_ms <- as.numeric((proc.time() - t_det_start)[["elapsed"]]) * 1000
      if (length(det$reasons)) logs <- c(logs, paste0("[Prepare][INFO] Detect reasons: ", paste(det$reasons, collapse = "; ")))

      prev <- last_kind_rv()
      if (!identical(prev, det$kind)) last_kind_rv(det$kind)

      run_id <- run_id_rv() + 1L
      run_id_rv(run_id)

      tz_off <- as.integer(format(as.POSIXlt(Sys.time(), tz = tz_string), "%z")) / 100

      if (det$kind == "hourly") {
        emit_toast("Detected HOURLY input. No conversion performed.", "message", 4)
        out <- src
        attr(out, "provenance") <- list(
          source = "hourly", conversion = "passthrough",
          tz = tz_string, offset_policy = offset_policy,
          offset_hours = NA, prepared_at = started_at
        )
        raw_like_rv(out)
        daily_src_rv(NULL)
        finished_at <- Sys.time()
        t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
        meta_rv(list(
          kind = "hourly", converted = FALSE, failed = FALSE,
          tz = tz_string, offset_policy = offset_policy, diurnal = dia,
          log = logs, t_detect_ms = t_detect_ms, t_convert_ms = 0.0,
          t_total_ms = t_total_ms, n_rows_in = n_rows_in,
          n_rows_out = nrow(out), station_count = station_count,
          run_id = run_id, started_at = started_at, finished_at = finished_at
        ))
        return()
      }

      if (det$kind == "daily") {
        emit_toast(sprintf("Detected DAILY input. Converting to HOURLY (%s)…", dia), "message", 5)
        logs <- c(
          logs, "[Prepare] Detected DAILY input.",
          if (isTRUE(det$seq_ok)) "[Prepare] Daily sequence looks continuous." else "[Prepare][WARN] Daily sequence has gaps.",
          "[Prepare] Converting to HOURLY …"
        )
        daily_src_rv(src)
        t_conv_start <- proc.time()
        res <- try(convert_multi_station_daily_to_hourly(src, tz_string, dia), silent = FALSE)
        t_convert_ms <- as.numeric((proc.time() - t_conv_start)[["elapsed"]]) * 1000

        if (inherits(res, "try-error") || is.null(res$hourly) || !nrow(res$hourly)) {
          emit_toast("Daily→Hourly conversion failed.", "error", 6)
          raw_like_rv(data.frame())
          finished_at <- Sys.time()
          t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
          meta_rv(list(
            kind = "daily", converted = TRUE, failed = TRUE,
            tz = tz_string, offset_policy = offset_policy, diurnal = dia,
            log = logs, t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms,
            t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L,
            station_count = station_count, run_id = run_id,
            started_at = started_at, finished_at = finished_at
          ))
          return()
        }

        raw_like <- res$hourly
        attr(raw_like, "provenance") <- list(
          source = "daily",
          conversion = paste0("daily→hourly(", dia, ")"),
          tz = tz_string, offset_policy = offset_policy,
          offset_hours = res$tz_off, prepared_at = started_at
        )
        raw_like_rv(raw_like)
        finished_at <- Sys.time()
        t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
        meta_rv(list(
          kind = "daily", converted = TRUE, failed = FALSE,
          tz = tz_string, offset_policy = offset_policy, diurnal = dia,
          log = c(logs, res$logs), t_detect_ms = t_detect_ms,
          t_convert_ms = t_convert_ms, t_total_ms = t_total_ms,
          n_rows_in = n_rows_in, n_rows_out = nrow(raw_like),
          station_count = station_count, run_id = run_id,
          started_at = started_at, finished_at = finished_at
        ))
        emit_toast(sprintf("Converted %s days to %s hourly rows.", nrow(src), nrow(raw_like)), "message", 5)
        return()
      }

      emit_toast("Prepare: could not classify input — check mapping/data.", "warning", 6)
      raw_like_rv(data.frame())
      daily_src_rv(NULL)
      finished_at <- Sys.time()
      t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
      meta_rv(list(
        kind = "unknown", converted = NA, failed = FALSE,
        tz = tz_string, offset_policy = offset_policy, diurnal = dia,
        log = logs, t_detect_ms = t_detect_ms, t_convert_ms = 0.0,
        t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L,
        station_count = station_count, run_id = run_id,
        started_at = started_at, finished_at = finished_at
      ))
    })

    list(
      raw_uploaded = reactive(raw_file()),
      hourly_file = reactive(raw_like_rv()),
      src_daily = reactive(daily_src_rv()),
      prep_meta = reactive(meta_rv())
    )
  })
}
