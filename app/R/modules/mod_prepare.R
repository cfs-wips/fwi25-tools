# R/modules/mod_prepare.R
# Prepare module: detect resolution, convert daily->hourly when needed,
# and expose both the hourly dataset (raw_file) and the original daily source (src_daily).
# Existing behavior preserved; adds multi-station handling and toast warnings.

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

    source("ng/make_minmax.r")
    source("ng/make_hourly.r")

    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

    emit_toast <- function(text, type = "message", duration = 5) {
      if (isTRUE(notify)) showNotification(text, type = type, duration = duration)
    }

    # ---- local convert wrapper (unchanged) ----
    convert_daily_to_hourly <- function(df, tz_string, diurnal_method, offset_policy, skip_invalid) {
      logs <- character()
      noon_df <- data.table::as.data.table(df)
      if (!all(c("yr", "mon", "day") %in% names(noon_df))) {
        stop("Missing year/month/day columns for daily conversion.")
      }
      tz_off <- as.integer(format(as.POSIXlt(Sys.time(), tz = tz_string), "%z")) / 100
      mm <- try(daily_to_minmax(noon_df[, .(yr, mon, day, temp, rh, ws, prec)]), silent = TRUE)
      if (inherits(mm, "try-error")) stop(paste("daily_to_minmax() failed:", as.character(mm)))
      mm <- as.data.frame(mm)
      names(mm) <- tolower(names(mm))
      mm$id <- if ("id" %in% names(noon_df)) noon_df$id[1] else "STN"
      mm$lat <- if ("lat" %in% names(noon_df)) noon_df$lat[1] else NA_real_
      mm$long <- if ("long" %in% names(noon_df)) noon_df$long[1] else NA_real_
      args <- list(mm, timezone = tz_off, skip_invalid = TRUE, verbose = FALSE, round_out = NA)
      fml <- try(formalArgs(minmax_to_hourly), silent = TRUE)
      if (!inherits(fml, "try-error")) {
        if ("method" %in% fml) args$method <- diurnal_method
        if ("diurnal" %in% fml) args$diurnal <- diurnal_method
      }
      hr <- try(do.call(minmax_to_hourly, args), silent = TRUE)
      if (inherits(hr, "try-error")) stop(paste("minmax_to_hourly() failed:", as.character(hr)))
      hr <- as.data.frame(hr)
      list(hourly = hr, tz_off = tz_off, logs = logs)
    }

    # ---- NEW helper: convert_multi_station_daily_to_hourly ----------------------
    convert_multi_station_daily_to_hourly <- function(df, tz_string, diurnal_method, offset_policy, skip_invalid, notify = TRUE) {
      emit_toast <- function(text, type = "message", duration = 5) {
        if (isTRUE(notify)) showNotification(text, type = type, duration = duration)
      }
      if (!"id" %in% names(df)) stop("Daily input must have an 'id' column for multi-station handling.")
      stn_list <- split(df, df$id)
      ranges <- lapply(stn_list, function(stn) {
        stn$date <- as.Date(sprintf("%04d-%02d-%02d", stn$yr, stn$mon, stn$day))
        list(min_date = min(stn$date), max_date = max(stn$date), gaps = sum(diff(stn$date) != 1))
      })
      gap_stations <- names(Filter(function(x) x$gaps > 0, ranges))
      if (length(gap_stations)) {
        emit_toast(sprintf("Daily sequence has gaps for stations: %s", paste(gap_stations, collapse = ", ")), "warning", 6)
      }
      min_dates <- sapply(ranges, function(x) x$min_date)
      max_dates <- sapply(ranges, function(x) x$max_date)
      if (length(unique(min_dates)) > 1 || length(unique(max_dates)) > 1) {
        emit_toast("Stations have different start/end dates. FWI will run per station.", "warning", 6)
      }
      res_list <- lapply(stn_list, function(stn) {
        convert_daily_to_hourly(stn, tz_string, diurnal_method, offset_policy, skip_invalid)
      })
      hourly <- data.table::rbindlist(lapply(res_list, function(x) x$hourly), fill = TRUE)
      list(hourly = hourly, tz_off = res_list[[1]]$tz_off, logs = unlist(lapply(res_list, function(x) x$logs)))
    }

    # ---- mapping readiness ----
    mapping_ready <- reactive({
      need_core <- c(mapping$col_temp(), mapping$col_rh(), mapping$col_ws(), mapping$col_rain())
      ok_core <- all(nzchar(need_core))
      has_dt <- nzchar(mapping$col_datetime())
      has_ymdh <- all(nzchar(c(mapping$col_year(), mapping$col_month(), mapping$col_day(), mapping$col_hour())))
      has_date <- nzchar(mapping$col_date())
      has_ymd <- all(nzchar(c(mapping$col_year(), mapping$col_month(), mapping$col_day())))
      hourly_ready <- ok_core && (has_dt || has_ymdh)
      daily_ready <- ok_core && (has_date || has_ymd)
      hourly_ready || daily_ready
    })

    detect_resolution <- function(df, tz_string) {
      dt <- data.table::as.data.table(df)
      cols <- names(dt)
      if ("id" %in% names(dt)) {
        data.table::setorder(dt, id, yr, mon, day, hr)
      } else {
        data.table::setorder(dt, yr, mon, day, hr)
      }
      reasons <- character()
      make_ts <- function(y, m, d, h = 0L) suppressWarnings(as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00", y, m, d, h), tz = tz_string))
      dt_col <- mapping$col_datetime() %||% ""
      if (nzchar(dt_col) && dt_col %in% cols) {
        data.table::setnames(dt, dt_col, "timestamp", skip_absent = TRUE)
        if (!inherits(dt$timestamp, "POSIXt")) {
          dt[, timestamp := suppressWarnings(as.POSIXct(timestamp, tz = tz_string, tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%SZ")))]
        }
        ok <- FALSE
        try(ok <- is_sequential_hours(dt[, .(timestamp)]), silent = FALSE)
        if (isTRUE(ok)) {
          return(list(kind = "hourly", seq_ok = TRUE, reasons = character()))
        }
        reasons <- c(reasons, "datetime present but hourly sequentiality failed or unparsable")
      }
      ycol <- mapping$col_year() %||% ""
      mcol <- mapping$col_month() %||% ""
      dcol <- mapping$col_day() %||% ""
      hcol <- mapping$col_hour() %||% ""
      have_ymdh <- nzchar(ycol) && nzchar(mcol) && nzchar(dcol) && nzchar(hcol) && all(c(ycol, mcol, dcol, hcol) %in% cols)
      if (have_ymdh) {
        dt[, timestamp := make_ts(get(ycol), get(mcol), get(dcol), get(hcol))]

        ok <- TRUE
        if ("id" %in% names(dt)) {
          seq_check <- dt[, .(seq_ok = is_sequential_hours(.SD)), by = id, .SDcols = "timestamp"]
          if (any(!seq_check$seq_ok)) {
            ok <- FALSE
            # Optional: warn user
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
      date_col <- mapping$col_date() %||% ""
      have_date_col <- nzchar(date_col) && date_col %in% cols
      if (have_date_col) {
        data.table::setnames(dt, date_col, "date", skip_absent = TRUE)
        dt[, date := suppressWarnings(as.Date(date))]

        if ("id" %in% names(dt)) {
          seq_check <- dt[, .(seq_ok = is_sequential_days(.SD)), by = id, .SDcols = "date"]
          if (any(!seq_check$seq_ok)) {
            ok <- FALSE
            showNotification("Some stations have gaps in daily sequence", type = "warning", duration = 6)
          }
        } else {
          try(ok <- is_sequential_days(dt[, .(date)]), silent = FALSE)
        }

        return(list(kind = "daily", seq_ok = isTRUE(ok), reasons = if (isTRUE(ok)) character() else "daily not sequential (date column)"))
      }
      have_ymd <- nzchar(ycol) && nzchar(mcol) && nzchar(dcol) && all(c(ycol, mcol, dcol) %in% cols)
      if (have_ymd) {
        dt[, date := suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", get(ycol), get(mcol), get(dcol))))]

        if ("id" %in% names(dt)) {
          seq_check <- dt[, .(seq_ok = is_sequential_days(.SD)), by = id, .SDcols = "date"]
          if (any(!seq_check$seq_ok)) {
            ok <- FALSE
            showNotification("Some stations have gaps in daily sequence", type = "warning", duration = 6)
          }
        } else {
          try(ok <- is_sequential_days(dt[, .(date)]), silent = FALSE)
        }

        return(list(kind = "daily", seq_ok = isTRUE(ok), reasons = if (isTRUE(ok)) character() else "daily not sequential (Y/M/D)"))
      }
      list(kind = "unknown", seq_ok = NA, reasons = reasons)
    }

    raw_like_rv <- reactiveVal(NULL)
    daily_src_rv <- reactiveVal(NULL)
    meta_rv <- reactiveVal(list(kind = NA, converted = FALSE, log = character()))
    last_kind_rv <- reactiveVal(NULL)

    observeEvent(list(raw_file(), mapping_ready(), tz$tz_use(), tz$tz_offset_policy(), diurnal_method_reactive()), ignoreInit = FALSE, {
      src <- raw_file()
      if (is.null(src) || !isTRUE(mapping_ready())) {
        return(invisible())
      }

      tz_string <- tz$tz_use()
      if (is.null(tz_string) || !nzchar(tz_string)) {
        return(invisible()) # Skip prepare until timezone is resolved
      }

      tz_string <- tz$tz_use()
      offset_policy <- tz$tz_offset_policy()
      dia <- diurnal_method_reactive() %||% "BT-default"
      tz_off <- as.integer(format(as.POSIXlt(Sys.time(), tz = tz_string), "%z")) / 100
      logs <- character()
      det <- detect_resolution(src, tz_string)
      if (length(det$reasons)) logs <- c(logs, paste0("[Prepare][INFO] Detect reasons: ", paste(det$reasons, collapse = "; ")))
      prev <- last_kind_rv()
      if (!identical(prev, det$kind)) last_kind_rv(det$kind)
      if (det$kind == "hourly") {
        emit_toast("Detected HOURLY input. No conversion performed.", "message", 4)
        out <- src
        attr(out, "provenance") <- list(source = "hourly", conversion = "passthrough", tz = tz_string, offset_policy = offset_policy, offset_hours = tz_off, prepared_at = Sys.time())
        raw_like_rv(out)
        daily_src_rv(NULL)
        meta_rv(list(kind = "hourly", converted = FALSE, tz = tz_string, log = logs))
        return(invisible())
      }
      if (det$kind == "daily") {
        emit_toast(sprintf("Detected DAILY input. Converting to HOURLY (%s)…", dia), "message", 5)
        logs <- c(logs, "[Prepare] Detected DAILY input.", if (isTRUE(det$seq_ok)) "[Prepare] Daily sequence looks continuous." else "[Prepare][WARN] Daily sequence has gaps.", "[Prepare] Converting to HOURLY …")
        res <- try(convert_multi_station_daily_to_hourly(src, tz_string, dia, offset_policy, skip_invalid, notify), silent = FALSE)
        daily_src <- src
        attr(daily_src, "provenance") <- list(source = "daily", conversion = "source_uploaded", tz = tz_string, offset_policy = offset_policy, offset_hours = tz_off, prepared_at = Sys.time())
        daily_src_rv(daily_src)
        if (inherits(res, "try-error") || is.null(res$hourly) || !nrow(res$hourly)) {
          emit_toast("Daily→Hourly conversion failed.", "error", 6)
          raw_like_rv(data.frame())
          meta_rv(list(kind = "daily", converted = TRUE, failed = TRUE, tz = tz_string, log = logs))
          return(invisible())
        }
        raw_like <- res$hourly
        attr(raw_like, "provenance") <- list(source = "daily", conversion = paste0("daily→hourly(", dia, ")"), tz = tz_string, offset_policy = offset_policy, offset_hours = tz_off, prepared_at = Sys.time())
        raw_like_rv(raw_like)
        meta_rv(list(kind = "daily", converted = TRUE, tz = tz_string, log = logs))
        emit_toast(sprintf("Converted %s days to %s hourly rows.", nrow(src), nrow(raw_like)), "message", 5)
        return(invisible())
      }
      emit_toast("Prepare: could not classify input — check mapping/data.", "warning", 6)
      raw_like_rv(data.frame())
      daily_src_rv(NULL)
      meta_rv(list(kind = "unknown", converted = NA, tz = tz_string, log = logs))
    })

    list(raw_uploaded = reactive(raw_file()), hourly_file = reactive(raw_like_rv()), src_daily = reactive(daily_src_rv()), prep_meta = reactive(meta_rv()))
  })
}
