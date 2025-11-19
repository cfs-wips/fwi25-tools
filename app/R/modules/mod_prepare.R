# app/R/modules/mod_prepare.R
# Daily→Hourly preparation module for FWI25
# - Rain is REQUIRED for both hourly passthrough and daily→hourly conversion.
# - Robust resolution detection (hourly via datetime or Y/M/D/H; daily via date or Y/M/D).
# - Uses is_sequential_days() for daily continuity checks.
# - Dimension-safe guards around daily_to_minmax() and minmax_to_hourly().
# - Emits clear logs + toast notifications. On failure, returns 0-row DF with logs so Log tab can show reasons.
# - Always includes a 'timestamp' column for the engine.

mod_prepare_server <- function(
  id,
  raw_file, # reactive: uploaded df (up$raw_file)
  mapping, # mapping module (col_*() + manual_lat()/manual_lon())
  tz, # timezone module (tz$tz_use(), tz$tz_standard_offset_hours())
  diurnal_method_reactive = reactive("BT-default"),
  skip_invalid = TRUE,
  notify = TRUE # show toast messages (Shiny showNotification)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    source("ng/make_minmax.r")
    source("ng/make_hourly.r")
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    to_int <- function(x) suppressWarnings(as.integer(x))
    to_num <- function(x) suppressWarnings(as.numeric(x))

    emit_toast <- function(text, type = "message", duration = 5) {
      if (isTRUE(notify)) showNotification(text, type = type, duration = duration)
    }

    # -----------------------------------------------------------------------
    # Mapping readiness: Rain REQUIRED for both hourly and daily inputs
    # -----------------------------------------------------------------------
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

    # -----------------------------------------------------------------------
    # Resolution detection (tolerant + diagnostic)
    # -----------------------------------------------------------------------
    detect_resolution <- function(df) {
      tz_string <- tz$tz_use() %||% "UTC"
      dt <- data.table::as.data.table(df)
      cols <- names(dt)
      reasons <- character()

      make_ts <- function(y, m, d, h = 0L) {
        suppressWarnings(as.POSIXct(
          sprintf(
            "%04d-%02d-%02d %02d:00:00",
            as.integer(y), as.integer(m), as.integer(d), as.integer(h)
          ),
          tz = tz_string
        ))
      }

      # HOURLY via datetime
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
        colnames(dt) <- tolower(colnames(dt))
        ok <- FALSE
        try(ok <- is_sequential_hours(dt[, .(timestamp)]), silent = FALSE)
        if (isTRUE(ok)) {
          return(list(kind = "hourly", seq_ok = TRUE, reasons = character()))
        }
        reasons <- c(reasons, "datetime present but hourly sequentiality failed or unparsable")
      }

      # HOURLY via Y/M/D/H
      ycol <- mapping$col_year() %||% ""
      mcol <- mapping$col_month() %||% ""
      dcol <- mapping$col_day() %||% ""
      hcol <- mapping$col_hour() %||% ""
      have_ymdh <- nzchar(ycol) && nzchar(mcol) && nzchar(dcol) && nzchar(hcol) &&
        all(c(ycol, mcol, dcol, hcol) %in% cols)
      if (have_ymdh) {
        dt[, timestamp := make_ts(get(ycol), get(mcol), get(dcol), get(hcol))]
        colnames(dt) <- tolower(colnames(dt))
        ok <- FALSE
        try(ok <- is_sequential_hours(dt[, .(timestamp)]), silent = FALSE)
        if (isTRUE(ok)) {
          return(list(kind = "hourly", seq_ok = TRUE, reasons = character()))
        }
        reasons <- c(reasons, "Y/M/D/H present but hourly sequentiality failed")
      }

      # DAILY via date (RELAXED sequentiality)
      date_col <- mapping$col_date() %||% ""
      have_date_col <- nzchar(date_col) && date_col %in% cols
      if (have_date_col) {
        data.table::setnames(dt, date_col, "date", skip_absent = TRUE)
        dt[, date := suppressWarnings(as.Date(date))]
        if (all(is.na(dt$date))) {
          reasons <- c(reasons, "date column present but could not parse to Date")
        } else {
          colnames(dt) <- tolower(colnames(dt))
          ok <- FALSE
          try(ok <- is_sequential_days(dt[, .(date)]), silent = FALSE)
          return(list(
            kind = "daily", seq_ok = isTRUE(ok),
            reasons = if (isTRUE(ok)) character() else "daily not sequential (date column)"
          ))
        }
      }

      # DAILY via Y/M/D (RELAXED sequentiality)
      have_ymd <- nzchar(ycol) && nzchar(mcol) && nzchar(dcol) && all(c(ycol, mcol, dcol) %in% cols)
      if (have_ymd) {
        dt[, date := suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", get(ycol), get(mcol), get(dcol))))]
        if (all(is.na(dt$date))) {
          reasons <- c(reasons, "Y/M/D present but could not construct Date")
        } else {
          colnames(dt) <- tolower(colnames(dt))
          ok <- FALSE
          try(ok <- is_sequential_days(dt[, .(date)]), silent = FALSE)
          return(list(
            kind = "daily", seq_ok = isTRUE(ok),
            reasons = if (isTRUE(ok)) character() else "daily not sequential (Y/M/D)"
          ))
        }
      }

      # Could not classify
      if (!nzchar(dt_col)) reasons <- c(reasons, "no datetime mapped")
      if (!have_ymdh) reasons <- c(reasons, "missing Y/M/D/H for hourly")
      if (!have_date_col) reasons <- c(reasons, "no date mapped")
      if (!have_ymd) reasons <- c(reasons, "missing Y/M/D for daily")
      list(kind = "unknown", seq_ok = NA, reasons = reasons)
    }

    # -----------------------------------------------------------------------
    # Lower-case noon daily frame for daily_to_minmax() (RAIN required)
    # -----------------------------------------------------------------------
    build_noon_df <- function(df) {
      dt <- data.table::as.data.table(df)

      # Date parts
      if (nzchar(mapping$col_date() %||% "")) {
        data.table::setnames(dt, mapping$col_date(), "date", skip_absent = TRUE)
        dt[, date := as.Date(date)]
        dt[, `:=`(
          yr = to_int(format(date, "%Y")),
          mon = to_int(format(date, "%m")),
          day = to_int(format(date, "%d"))
        )]
      } else {
        data.table::setnames(dt, mapping$col_year(), "yr", skip_absent = TRUE)
        data.table::setnames(dt, mapping$col_month(), "mon", skip_absent = TRUE)
        data.table::setnames(dt, mapping$col_day(), "day", skip_absent = TRUE)
      }

      # Weather (RAIN REQUIRED)
      data.table::setnames(dt, mapping$col_temp(), "temp", skip_absent = TRUE)
      data.table::setnames(dt, mapping$col_rh(), "rh", skip_absent = TRUE)
      data.table::setnames(dt, mapping$col_ws(), "ws", skip_absent = TRUE)
      if (nzchar(mapping$col_rain() %||% "") && mapping$col_rain() %in% names(dt)) {
        data.table::setnames(dt, mapping$col_rain(), "prec", skip_absent = TRUE)
      } else {
        stop("Precipitation (RAIN) is required for daily→hourly conversion.")
      }
      # Station id (optional)
      if (nzchar(mapping$col_id() %||% "") && mapping$col_id() %in% names(dt)) {
        data.table::setnames(dt, mapping$col_id(), "id", skip_absent = TRUE)
      } else {
        dt[, id := "STN"]
      }

      # Lat/Lon (per-row preferred; else manual)
      lower_names <- tolower(names(dt))
      has_lat <- any(lower_names %in% c("lat", "latitude"))
      has_lon <- any(lower_names %in% c("long", "lon", "longitude"))
      if (has_lat && has_lon) {
        for (cand in c("LAT", "Latitude", "latitude", "lat")) if (cand %in% names(dt)) data.table::setnames(dt, cand, "lat", skip_absent = TRUE)
        for (cand in c("LONG", "LON", "Longitude", "longitude", "long", "lon")) if (cand %in% names(dt)) data.table::setnames(dt, cand, "long", skip_absent = TRUE)
      } else {
        lat <- to_num(mapping$manual_lat())
        lon <- to_num(mapping$manual_lon())
        if (is.na(lat) || is.na(lon)) stop("Manual LAT/LON must be provided for daily→hourly conversion.")
        dt[, `:=`(lat = lat, long = lon)]
      }

      need <- c("id", "lat", "long", "yr", "mon", "day", "temp", "rh", "ws", "prec")
      miss <- setdiff(need, names(dt))
      print(miss)
      if (length(miss)) stop("Missing mapped columns for daily→minmax: ", paste(miss, collapse = ", "))

      # Return strict data.frame
      as.data.frame(dt[, .(id, lat, long, yr, mon, day, temp, rh, ws, prec)])
    }

    # -----------------------------------------------------------------------
    # daily → minmax → hourly, with dimension-safe guards & diagnostics
    # -----------------------------------------------------------------------
    convert_daily_to_hourly <- function(df, tz_string, diurnal_method, skip_invalid) {
      noon_all <- build_noon_df(df)

      # year-by-year daily continuity
      ys <- data.table::setDT(noon_all)
      ys <- ys[, .(ok = is_sequential_days(data.frame(date = as.Date(sprintf("%04d-%02d-%02d", yr, mon, day))))), by = .(id, yr)]
      print(ys)
      warn <- ys[ok == FALSE]
      good <- ys[ok == TRUE]
      print(paste0("Good: ", good))
      logs <- character()
      if (nrow(warn)) {
        for (i in seq_len(nrow(warn))) {
          logs <- c(logs, sprintf(
            "[Prepare][WARN] %s %d is not sequential daily; %s.",
            warn$id[i], warn$yr[i],
            if (isTRUE(skip_invalid)) "skipping" else "aborting"
          ))
        }
        if (!isTRUE(skip_invalid)) stop("Non-sequential daily station-years present; aborting.")
      }

      # Fixed standard offset (hours) for converter (standard time)
      tz_off <- if (!is.null(tz$tz_standard_offset_hours)) {
        tz$tz_standard_offset_hours(tz_string)
      } else {
        tz_standard_offset_hours(tz_string) # fallback util
      }

      out <- NULL

      for (i in seq_len(nrow(good))) {
        g <- good[i]
        sub <- data.table::as.data.table(noon_all)[id == g$id & yr == g$yr]

        # --- daily → min/max
        mm <- try(daily_to_minmax(sub[, .(yr, mon, day, temp, rh, ws, prec)]), silent = FALSE)
        if (inherits(mm, "try-error")) stop(sprintf("daily_to_minmax() error for %s %d: %s", g$id, g$yr, as.character(mm)))
        if (!is.data.frame(mm)) mm <- try(as.data.frame(mm), silent = FALSE)
        if (inherits(mm, "try-error") || is.null(dim(mm)) || ncol(mm) == 0L) {
          stop(sprintf("daily_to_minmax() returned non-tabular or empty result for %s %d.", g$id, g$yr))
        }

        # Add id/lat/long and uppercase for hourly converter
        mm <- merge(mm, unique(sub[, .(id, lat, long, yr)]), by = "yr")
        if (!is.data.frame(mm)) mm <- as.data.frame(mm)
        names(mm) <- toupper(names(mm))

        need <- c("LAT", "LONG", "YR", "MON", "DAY", "TEMP_MIN", "TEMP_MAX", "RH_MIN", "RH_MAX", "WS_MIN", "WS_MAX", "PREC", "ID")
        miss <- setdiff(need, names(mm))
        if (length(miss)) stop(sprintf("minmax_to_hourly() input missing for %s %d: %s", g$id, g$yr, paste(miss, collapse = ", ")))
        print(head(mm))
        # --- min/max → hourly
        args <- list(mm, timezone = tz_off, skip_invalid = TRUE, verbose = TRUE, round_out = NA)
        fml <- try(formalArgs(minmax_to_hourly), silent = FALSE)
        if (!inherits(fml, "try-error")) {
          if ("method" %in% fml) args$method <- diurnal_method
          if ("diurnal" %in% fml) args$diurnal <- diurnal_method
        }
        hr <- try(do.call(minmax_to_hourly, args), silent = FALSE)
        if (inherits(hr, "try-error")) stop(sprintf("minmax_to_hourly() error for %s %d: %s", g$id, g$yr, as.character(hr)))
        if (!is.data.frame(hr)) hr <- try(as.data.frame(hr), silent = FALSE)
        if (inherits(hr, "try-error") || is.null(dim(hr)) || ncol(hr) == 0L) {
          stop(sprintf("minmax_to_hourly() returned non-tabular or empty result for %s %d.", g$id, g$yr))
        }

        # Continuity check with fixed-offset tz (avoid DST false alarms)
        hr_dt <- data.table::as.data.table(hr)
        etc_sign <- if (tz_off >= 0) "-" else "+"
        etc_tz <- sprintf("Etc/GMT%s%d", etc_sign, abs(as.integer(tz_off))) # reversed sign convention
        hr_dt[, timestamp := as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00", yr, mon, day, hr), tz = etc_tz)]
        if (!is.null(dim(hr_dt))) setnames(hr_dt, names(hr_dt), tolower(names(hr_dt)))

        ok <- FALSE
        try(ok <- is_sequential_hours(hr_dt), silent = FALSE)
        if (!isTRUE(ok)) logs <- c(logs, sprintf("[Prepare][WARN] Post-conversion hourly continuity not perfect for %s %d.", g$id, g$yr))

        # Keep all columns except our temp timestamp; use with=FALSE to keep 2D
        out <- data.table::rbindlist(
          list(out, hr_dt[, !c("timestamp"), with = FALSE]),
          use.names = TRUE, fill = TRUE
        )
      }

      list(hourly = out, logs = logs, tz_off = tz_off)
    }

    # -----------------------------------------------------------------------
    # Recompose an engine-friendly "raw" df; always include `timestamp`
    # -----------------------------------------------------------------------
    make_raw_like <- function(hr) {
      dt <- data.table::as.data.table(hr)
      tz_str <- tz$tz_use() %||% "UTC"

      if (nzchar(mapping$col_datetime() %||% "")) {
        nm <- mapping$col_datetime()
        dt[, (nm) := as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00", yr, mon, day, hr), tz = tz_str)]
        dt[, timestamp := get(nm)]
      } else {
        data.table::setnames(dt, "yr", mapping$col_year(), skip_absent = TRUE)
        data.table::setnames(dt, "mon", mapping$col_month(), skip_absent = TRUE)
        data.table::setnames(dt, "day", mapping$col_day(), skip_absent = TRUE)
        data.table::setnames(dt, "hr", mapping$col_hour(), skip_absent = TRUE)
        dt[, timestamp := as.POSIXct(
          sprintf(
            "%04d-%02d-%02d %02d:00:00",
            get(mapping$col_year()),
            get(mapping$col_month()),
            get(mapping$col_day()),
            get(mapping$col_hour())
          ),
          tz = tz_str
        )]
      }

      data.table::setnames(dt, "temp", mapping$col_temp(), skip_absent = TRUE)
      data.table::setnames(dt, "rh", mapping$col_rh(), skip_absent = TRUE)
      data.table::setnames(dt, "ws", mapping$col_ws(), skip_absent = TRUE)
      data.table::setnames(dt, "prec", mapping$col_rain(), skip_absent = TRUE)

      if (nzchar(mapping$col_id() %||% "") && "id" %in% names(dt)) {
        data.table::setnames(dt, "id", mapping$col_id(), skip_absent = TRUE)
      } else {
        if ("id" %in% names(dt)) dt[, id := NULL]
      }

      as.data.frame(dt)
    }

    # -----------------------------------------------------------------------
    # Reactives
    # -----------------------------------------------------------------------
    raw_like_rv <- reactiveVal(NULL)
    meta_rv <- reactiveVal(list(kind = NA, converted = FALSE, log = character()))
    last_kind_rv <- reactiveVal(NULL)

    observeEvent(
      list(
        raw_file(),
        mapping$col_datetime(), mapping$col_date(),
        mapping$col_year(), mapping$col_month(), mapping$col_day(), mapping$col_hour(),
        mapping$col_temp(), mapping$col_rh(), mapping$col_ws(), mapping$col_rain(),
        mapping$col_id(), mapping$manual_lat(), mapping$manual_lon(),
        tz$tz_use(), diurnal_method_reactive()
      ),
      ignoreInit = FALSE,
      {
        src <- raw_file()
        if (is.null(src)) {
          return(invisible())
        }
        if (!isTRUE(mapping_ready())) {
          return(invisible())
        }

        tz_string <- tz$tz_use() %||% "UTC"
        dia <- diurnal_method_reactive() %||% "BT-default"
        logs <- character()

        det <- detect_resolution(src)
        if (length(det$reasons)) {
          logs <- c(logs, paste0("[Prepare][INFO] Detect reasons: ", paste(det$reasons, collapse = "; ")))
        }

        # Toast classification change
        prev <- last_kind_rv()
        if (!identical(prev, det$kind)) {
          last_kind_rv(det$kind)
          if (identical(det$kind, "hourly")) {
            emit_toast("Detected HOURLY input. No conversion performed.", "message", 4)
          } else if (identical(det$kind, "daily")) {
            emit_toast(sprintf("Detected DAILY input. Converting to HOURLY (%s) …", dia), "message", 5)
          } else {
            emit_toast("Prepare: could not classify input — check mapping/data.", "warning", 6)
          }
        }

        # HOURLY (RAIN required)
        if (det$kind == "hourly") {
          rn <- mapping$col_rain() %||% ""
          if (!nzchar(rn) || !(rn %in% names(src))) {
            logs <- c(logs, "[Prepare][ERROR] Hourly passthrough requires a precipitation column mapped to 'rain'.")
            emit_toast("Hourly passthrough blocked: RAIN column is required.", "error", 6)

            # return 0-row DF so Log tab can display logs
            out <- data.frame()
            attr(out, "provenance") <- list(
              source = "hourly", conversion = "passthrough-blocked", tz = tz_string,
              offset_hours = if (!is.null(tz$tz_standard_offset_hours)) tz$tz_standard_offset_hours(tz_string) else NA_integer_,
              prepared_at = Sys.time()
            )
            attr(out, "prep_log") <- logs
            raw_like_rv(out)
            meta_rv(list(kind = "hourly", converted = FALSE, failed = TRUE, tz = tz_string, log = logs))
            return(invisible())
          }

          logs <- c(logs, "[Prepare] Detected HOURLY input. No conversion performed.")
          out <- as.data.frame(src)
          if (nzchar(mapping$col_datetime() %||% "")) {
            nm <- mapping$col_datetime()
            if (!("timestamp" %in% names(out))) out$timestamp <- out[[nm]]
          }
          attr(out, "provenance") <- list(
            source       = "hourly",
            conversion   = "passthrough",
            tz           = tz_string,
            offset_hours = if (!is.null(tz$tz_standard_offset_hours)) tz$tz_standard_offset_hours(tz_string) else NA_integer_,
            prepared_at  = Sys.time()
          )
          attr(out, "prep_log") <- logs

          raw_like_rv(out)
          meta_rv(list(kind = "hourly", converted = FALSE, tz = tz_string, log = logs))
          return(invisible())
        }

        # DAILY → HOURLY (RAIN required)
        if (det$kind == "daily") {
          rn <- mapping$col_rain() %||% ""
          if (!nzchar(rn) || !(rn %in% names(src))) {
            logs <- c(logs, "[Prepare][ERROR] Daily→Hourly requires a precipitation column mapped to 'rain'.")
            emit_toast("Daily→Hourly blocked: RAIN column is required.", "error", 6)

            out <- data.frame()
            attr(out, "provenance") <- list(
              source = "daily", conversion = "daily→hourly-blocked", tz = tz_string,
              offset_hours = if (!is.null(tz$tz_standard_offset_hours)) tz$tz_standard_offset_hours(tz_string) else NA_integer_,
              prepared_at = Sys.time()
            )
            attr(out, "prep_log") <- logs
            raw_like_rv(out)
            meta_rv(list(kind = "daily", converted = TRUE, failed = TRUE, tz = tz_string, log = logs))
            return(invisible())
          }

          # Conversion
          logs <- c(
            logs,
            "[Prepare] Detected DAILY input.",
            if (isTRUE(det$seq_ok)) {
              "[Prepare] Daily sequence looks continuous."
            } else {
              "[Prepare][WARN] Daily sequence has gaps; converter will skip non-sequential station-years."
            },
            "[Prepare] Converting to HOURLY …"
          )

          res <- try(convert_daily_to_hourly(src, tz_string, diurnal_method = dia, skip_invalid = skip_invalid), silent = FALSE)

          if (inherits(res, "try-error") || is.null(res$hourly) || !nrow(res$hourly)) {
            logs <- c(logs, paste0("[Prepare][ERROR] Daily→Hourly failed: ", as.character(res)))
            emit_toast("Daily→Hourly conversion FAILED. See Log for details.", "error", 6)

            out <- data.frame()
            attr(out, "provenance") <- list(
              source = "daily", conversion = paste0("daily→minmax→hourly(", dia, "): failed"),
              tz = tz_string,
              offset_hours = if (!is.null(tz$tz_standard_offset_hours)) tz$tz_standard_offset_hours(tz_string) else NA_integer_,
              prepared_at = Sys.time()
            )
            attr(out, "prep_log") <- logs
            raw_like_rv(out)
            meta_rv(list(kind = "daily", converted = TRUE, failed = TRUE, tz = tz_string, log = logs))
            return(invisible())
          }

          hr <- res$hourly
          ids <- if ("id" %in% names(hr)) length(unique(hr$id)) else 1L
          rng <- sprintf(
            "%04d-%02d-%02d %02d:00 → %04d-%02d-%02d %02d:00",
            min(hr$yr), min(hr$mon), min(hr$day), min(hr$hr),
            max(hr$yr), max(hr$mon), max(hr$day), max(hr$hr)
          )

          logs <- c(
            logs,
            sprintf("[Prepare] Conversion OK; TZ offset (standard): %d h.", res$tz_off),
            sprintf("[Prepare] Produced %d station(s), %d hourly rows, %s.", ids, nrow(hr), rng)
          )
          emit_toast(sprintf("Daily→Hourly conversion completed: %d hourly rows.", nrow(hr)), "message", 5)

          raw_like <- make_raw_like(hr)
          attr(raw_like, "provenance") <- list(
            source       = "daily",
            conversion   = paste0("daily→minmax→hourly(", dia, ")"),
            tz           = tz_string,
            offset_hours = res$tz_off,
            prepared_at  = Sys.time()
          )
          attr(raw_like, "prep_log") <- logs

          raw_like_rv(raw_like)
          meta_rv(list(kind = "daily", converted = TRUE, tz = tz_string, log = logs))
          return(invisible())
        }

        # UNKNOWN
        if (det$kind == "unknown") {
          if (length(det$reasons)) {
            logs <- c(logs, paste0("[Prepare][ERROR] Could not classify input: ", paste(det$reasons, collapse = "; ")))
          } else {
            logs <- c(logs, "[Prepare][ERROR] Could not classify input as hourly or daily. Check mapping/data.")
          }
          emit_toast("Prepare: could not classify input — check mapping/data.", "warning", 6)

          out <- data.frame()
          attr(out, "provenance") <- list(
            source = "unknown", conversion = "n/a", tz = tz_string,
            offset_hours = if (!is.null(tz$tz_standard_offset_hours)) tz$tz_standard_offset_hours(tz_string) else NA_integer_,
            prepared_at = Sys.time()
          )
          attr(out, "prep_log") <- logs
          raw_like_rv(out)
          meta_rv(list(kind = "unknown", converted = NA, tz = tz_string, log = logs))
          return(invisible())
        }
      }
    )

    # Public API
    list(
      raw_file  = reactive(raw_like_rv()),
      prep_meta = reactive(meta_rv())
    )
  })
}
