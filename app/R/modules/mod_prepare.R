
# mod_prepare.R — robust prepare module with hourly-vs-daily detection, gap handling, and daily-noon replacement

mod_prepare_server <- function(
    id,
    raw_file,
    mapping,
    tz,
    diurnal_method_reactive = reactive("BT-default"),
    notify = TRUE
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # null-coalescing
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    
    emit_toast <- function(text, type = "message", duration = 5) {
      if (isTRUE(notify)) showNotification(text, type = type, duration = duration)
    }
    
    # --- Required helpers / dependencies -------------------------------------
    source("ng/make_minmax.r")  # provides: daily_to_minmax()
    source("ng/make_hourly.r")  # provides: minmax_to_hourly()
    
    # Guess an hour-like column if mapping doesn't provide one or if it isn't present
    guess_hour_col <- function(dt) {
      cols <- names(dt)
      name_hits <- grep("^(hour|hr|hh|heure|hora)$", tolower(cols), value = TRUE)
      check_col <- function(col) {
        x <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(dt[[col]]))))
        ok <- sum(!is.na(x) & x >= 0L & x <= 23L)
        frac <- ok / max(1L, length(x))
        frac >= 0.80  # at least 80% of values look like hours
      }
      for (col in c(name_hits, cols)) {
        if (check_col(col)) return(col)
      }
      NULL
    }
    
    # Resolve standard column names via mapping or best-effort detection
    resolve_cols <- function(dt) {
      h_map   <- mapping$col_hour()
      h_guess <- if (is.null(h_map) || !nzchar(h_map) || !(h_map %in% names(dt))) guess_hour_col(dt) else h_map
      list(
        id       = if ("id" %in% names(dt)) "id" else NULL,
        lat      = if ("lat" %in% names(dt)) "lat" else NULL,
        long     = if ("long" %in% names(dt)) "long" else if ("lon" %in% names(dt)) "lon" else NULL,
        yr       = mapping$col_year()   %||% "yr",
        mon      = mapping$col_month()  %||% "mon",
        day      = mapping$col_day()    %||% "day",
        hr       = h_guess,  # guessed if mapping not available
        temp     = mapping$col_temp()   %||% "temp",
        rh       = mapping$col_rh()     %||% "rh",
        ws       = mapping$col_ws()     %||% "ws",
        prec     = mapping$col_rain()   %||% (if ("prec" %in% names(dt)) "prec" else if ("Rain_24" %in% names(dt)) "Rain_24" else if ("rn_1" %in% names(dt)) "rn_1" else NULL),
        datetime = mapping$col_datetime() %||% NULL,
        date     = mapping$col_date()     %||% NULL
      )
    }
    
    # DST-aware hour checks
    .count_hour_gaps <- function(ts) { d <- as.numeric(diff(ts), units = "hours"); sum(d > 1 & d != 2) }  # ignore spring-forward
    .count_hour_dups <- function(ts) { d <- as.numeric(diff(ts), units = "hours"); sum(d == 0) }          # fall-back duplicates
    .is_seq_days     <- function(d)  { if (!length(d)) return(FALSE); all(diff(d) == 1) }
    
    # Convert daily table (single or multi-station) to hourly via your pipeline
    convert_daily_to_hourly <- function(df, tz_string, diurnal_method) {
      req_cols <- c("yr", "mon", "day", "temp", "rh", "ws", "prec")
      if (!all(req_cols %in% names(df))) stop("Missing daily columns: ", paste(setdiff(req_cols, names(df)), collapse = ", "))
      tz_off <- as.integer(format(as.POSIXlt(Sys.time(), tz = tz_string), "%z")) / 100
      
      mm <- try(daily_to_minmax(df[, .(yr, mon, day, temp, rh, ws, prec)]), silent = TRUE)
      if (inherits(mm, "try-error")) stop(paste("daily_to_minmax() failed:", as.character(mm)))
      names(mm) <- tolower(names(mm))
      
      # carry id/lat/long if present
      mm$id   <- if ("id"   %in% names(df)) df$id[1]   else "STN"
      mm$lat  <- if ("lat"  %in% names(df)) df$lat[1]  else NA_real_
      mm$long <- if ("long" %in% names(df)) df$long[1] else NA_real_
      
      # Build args defensively: add optional params only if supported
      fml  <- try(formalArgs(minmax_to_hourly), silent = TRUE)
      args <- list(mm, timezone = tz_off, verbose = FALSE, round_out = NA)
      if (!inherits(fml, "try-error") && "skip_invalid" %in% fml) args$skip_invalid <- TRUE
      if (!inherits(fml, "try-error")) {
        if ("method"  %in% fml) args$method  <- diurnal_method
        if ("diurnal" %in% fml) args$diurnal <- diurnal_method
      }
      
      hr <- try(do.call(minmax_to_hourly, args), silent = TRUE)
      if (inherits(hr, "try-error")) stop(paste("minmax_to_hourly() failed:", as.character(hr)))
      
      list(hourly = hr, tz_off = tz_off, logs = character())
    }
    
    convert_multi_station_daily_to_hourly <- function(df, tz_string, diurnal_method) {
      if (!"id" %in% names(df)) stop("Daily input must have an 'id' column.")
      stn_list <- split(df, df$id)
      
      # optional gap report on daily input
      gaps <- lapply(stn_list, function(stn) {
        stn$date <- as.Date(sprintf("%04d-%02d-%02d", stn$yr, stn$mon, stn$day))
        any(diff(stn$date) != 1)
      })
      gap_stations <- names(which(unlist(gaps)))
      logs <- character()
      if (length(gap_stations)) {
        emit_toast(sprintf("Daily sequence has gaps for stations: %s", paste(gap_stations, collapse = ", ")), "warning", 6)
        logs <- c(logs, sprintf("[Prepare][WARN] Daily gaps: %s", paste(gap_stations, collapse = ", ")))
      }
      
      hourly_list <- lapply(stn_list, function(stn) convert_daily_to_hourly(stn, tz_string, diurnal_method))
      hourly <- data.table::rbindlist(lapply(hourly_list, `[[`, "hourly"), fill = TRUE)
      list(hourly = hourly, tz_off = hourly_list[[1]]$tz_off, logs = logs)
    }
    
    # Pick representative daily temp/RH (prefer hr==12; otherwise nearest to 12)
    pick_daily_representative <- function(day_dt, col_hr, col_temp, col_rh) {
      hr_vals <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(day_dt[[col_hr]]))))
      idx12 <- which(hr_vals == 12L)
      idx   <- if (length(idx12)) idx12[1] else which.min(abs(hr_vals - 12L))
      list(
        temp = suppressWarnings(as.numeric(day_dt[[col_temp]][idx])),
        rh   = suppressWarnings(as.numeric(day_dt[[col_rh]][idx]))
      )
    }
    
    # Construct one daily row (yr/mon/day/temp/rh/ws/prec + id/lat/long)
    make_daily_row <- function(day_dt, cols) {
      repv    <- pick_daily_representative(day_dt, cols$hr, cols$temp, cols$rh)
      ws_mean <- if (!is.null(cols$ws) && cols$ws %in% names(day_dt)) mean(suppressWarnings(as.numeric(day_dt[[cols$ws]])), na.rm = TRUE) else NA_real_
      prec_sum<- if (!is.null(cols$prec) && cols$prec %in% names(day_dt)) sum(suppressWarnings(as.numeric(day_dt[[cols$prec]])), na.rm = TRUE) else 0
      data.table::data.table(
        id   = if (!is.null(cols$id)) day_dt[[cols$id]][1] else "STN",
        lat  = if (!is.null(cols$lat)) suppressWarnings(as.numeric(day_dt[[cols$lat]][1])) else NA_real_,
        long = if (!is.null(cols$long)) suppressWarnings(as.numeric(day_dt[[cols$long]][1])) else NA_real_,
        yr   = suppressWarnings(as.integer(day_dt[[cols$yr]][1])),
        mon  = suppressWarnings(as.integer(day_dt[[cols$mon]][1])),
        day  = suppressWarnings(as.integer(day_dt[[cols$day]][1])),
        temp = repv$temp, rh = repv$rh, ws = ws_mean, prec = prec_sum
      )
    }
    
    # Synthesize a full 24-hour series for one station/day from a daily row
    synthesize_day_hourly <- function(daily_row, tz_string, diurnal_method) {
      mm <- try(daily_to_minmax(daily_row[, .(yr, mon, day, temp, rh, ws, prec)]), silent = TRUE)
      if (inherits(mm, "try-error")) stop(paste("daily_to_minmax() failed:", as.character(mm)))
      names(mm) <- tolower(names(mm))
      mm$id   <- if ("id"   %in% names(daily_row)) daily_row$id[1]   else "STN"
      mm$lat  <- if ("lat"  %in% names(daily_row)) daily_row$lat[1]  else NA_real_
      mm$long <- if ("long" %in% names(daily_row)) daily_row$long[1] else NA_real_
      
      fml  <- try(formalArgs(minmax_to_hourly), silent = TRUE)
      tz_off <- as.integer(format(as.POSIXlt(Sys.time(), tz = tz_string), "%z")) / 100
      args <- list(mm, timezone = tz_off, verbose = FALSE, round_out = NA)
      if (!inherits(fml, "try-error") && "skip_invalid" %in% fml) args$skip_invalid <- TRUE
      if (!inherits(fml, "try-error")) {
        if ("method"  %in% fml) args$method  <- diurnal_method
        if ("diurnal" %in% fml) args$diurnal <- diurnal_method
      }
      hr <- try(do.call(minmax_to_hourly, args), silent = TRUE)
      if (inherits(hr, "try-error")) stop(paste("minmax_to_hourly() failed:", as.character(hr)))
      if (!"timestamp" %in% names(hr)) {
        hr[, timestamp := as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00", yr, mon, day, hr), tz = tz_string)]
      }
      hr[]
    }
    
    # Fill only missing hours (per station/day) using daily→hourly synthesis
    fill_hourly_gaps_with_diurnal <- function(src, tz_string, diurnal_method) {
      dt   <- data.table::as.data.table(src)
      cols <- resolve_cols(dt)
      stopifnot(all(c(cols$yr, cols$mon, cols$day, cols$hr) %in% names(dt)))
      
      # Canonical timestamp from Y/M/D/H
      dt[, timestamp := as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00",
                                           as.integer(get(cols$yr)),
                                           as.integer(get(cols$mon)),
                                           as.integer(get(cols$day)),
                                           as.integer(gsub("[^0-9-]", "", as.character(get(cols$hr))))),
                                   tz = tz_string)]
      
      stn_list <- if (!is.null(cols$id) && cols$id %in% names(dt)) split(dt, dt[[cols$id]]) else list(STN = dt)
      
      fill_one_station <- function(stn_dt, stn_id) {
        stn_dt[, date := as.Date(timestamp, tz = tz_string)]
        day_stats <- stn_dt[, .(
          n_hours   = data.table::uniqueN(as.integer(format(timestamp, "%H"))),
          gap_count = { d <- as.numeric(diff(sort(timestamp)), units = "hours"); sum(d > 1 & d != 2) }
        ), by = date]
        need <- day_stats[gap_count > 0 | n_hours < 24, date]
        if (!length(need)) return(stn_dt[, date := NULL][])
        
        synth_list <- vector("list", length(need))
        for (i in seq_along(need)) {
          day_i   <- need[i]
          day_dt  <- stn_dt[date == day_i]
          daily_row <- make_daily_row(day_dt, cols)
          syn <- synthesize_day_hourly(daily_row, tz_string, diurnal_method)
          synth_list[[i]] <- syn
        }
        syn_all <- data.table::rbindlist(synth_list, fill = TRUE)
        
        # Merge: add missing timestamps; fill NA fields on existing timestamps
        data.table::setkey(stn_dt, timestamp)
        data.table::setkey(syn_all, timestamp)
        
        # add rows for truly missing timestamps (anti-join; no 'nomatch' with '!')
        missing_ts <- syn_all[!stn_dt, on = "timestamp"]$timestamp
        if (length(missing_ts)) {
          stn_dt <- data.table::rbindlist(list(stn_dt, syn_all[J(missing_ts)]), fill = TRUE)
        }
        
        # fill NA values in overlapping timestamps for core variables
        core_fill <- intersect(c(cols$temp, cols$rh, cols$ws, cols$prec, "temp", "Rh", "Wspd", "prec"), names(stn_dt))
        if (length(core_fill)) {
          idx <- stn_dt[, .I[Reduce(`|`, lapply(.SD, is.na))], .SDcols = core_fill]
          if (length(idx)) {
            stn_dt[idx, (core_fill) := syn_all[.SD$timestamp, on = "timestamp"][, ..core_fill]]
          }
        }
        
        stn_dt[, date := NULL][]
      }
      
      out_list <- Map(fill_one_station, stn_list, names(stn_list))
      out <- data.table::rbindlist(out_list, fill = TRUE)
      if (!is.null(cols$id) && cols$id %in% names(out)) {
        data.table::setorder(out, get(cols$id), timestamp)
      } else {
        data.table::setorder(out, timestamp)
      }
      out[]
    }
    
    # Replace entire stream: require daily NOON completeness and convert to full hourly
    daily_noon_is_complete <- function(src) {
      dt   <- data.table::as.data.table(src)
      cols <- resolve_cols(dt)
      if (!all(c(cols$yr, cols$mon, cols$day, cols$hr) %in% names(dt))) return(FALSE)
      
      dt[, date := as.Date(sprintf("%04d-%02d-%02d", get(cols$yr), get(cols$mon), get(cols$day)))]
      dt[, hr   := as.integer(gsub("[^0-9-]", "", as.character(get(cols$hr))))]
      per_day <- dt[, .(has_noon = any(hr == 12L), noon_count = sum(hr == 12L)),
                    by = .(id = if (!is.null(cols$id)) get(cols$id) else rep("STN", .N), date)]
      all(per_day$has_noon & per_day$noon_count == 1L) &&
        all(dt[, .(.is_seq_days(unique(date))), by = .(id = if (!is.null(cols$id)) get(cols$id) else rep("STN", .N))]$V1)
    }
    
    convert_daily_noon_to_hourly <- function(src, tz_string, diurnal_method) {
      dt   <- data.table::as.data.table(src)
      cols <- resolve_cols(dt)
      stopifnot(all(c(cols$yr, cols$mon, cols$day, cols$hr) %in% names(dt)))
      dt[, hr := as.integer(gsub("[^0-9-]", "", as.character(get(cols$hr))))]
      noon <- dt[hr == 12L]
      
      daily <- noon[, .(
        id   = if (!is.null(cols$id)) get(cols$id) else rep("STN", .N),
        lat  = if (!is.null(cols$lat)) suppressWarnings(as.numeric(get(cols$lat))) else NA_real_,
        long = if (!is.null(cols$long)) suppressWarnings(as.numeric(get(cols$long))) else NA_real_,
        yr   = suppressWarnings(as.integer(get(cols$yr))),
        mon  = suppressWarnings(as.integer(get(cols$mon))),
        day  = suppressWarnings(as.integer(get(cols$day))),
        temp = suppressWarnings(as.numeric(get(cols$temp))),
        rh   = suppressWarnings(as.numeric(get(cols$rh))),
        ws   = if (!is.null(cols$ws)) suppressWarnings(as.numeric(get(cols$ws))) else NA_real_,
        prec = if (!is.null(cols$prec)) suppressWarnings(as.numeric(get(cols$prec))) else 0
      )]
      
      convert_multi_station_daily_to_hourly(daily, tz_string, diurnal_method)$hourly
    }
    
    # Schema-first detection (+ daily-one-hour generalized) and hourly inference
    detect_resolution <- function(df, tz_string) {
      dt <- data.table::as.data.table(df)
      cols <- names(dt)
      reasons <- character()
      
      dt_col  <- mapping$col_datetime() %||% ""
      date_col<- mapping$col_date()     %||% ""
      ycol    <- mapping$col_year()     %||% ""
      mcol    <- mapping$col_month()    %||% ""
      dcol    <- mapping$col_day()      %||% ""
      hcol    <- mapping$col_hour()     %||% ""
      
      # If mapping doesn't give us hour, try to guess it
      if (!nzchar(hcol) || !(hcol %in% cols)) {
        hcol_guess <- guess_hour_col(dt)
        if (!is.null(hcol_guess)) {
          hcol <- hcol_guess
          reasons <- c(reasons, sprintf("guessed hour column: '%s'", hcol))
        }
      }
      
      has_datetime <- nzchar(dt_col)   && dt_col   %in% cols
      has_hour     <- nzchar(hcol)     && hcol     %in% cols
      has_date     <- nzchar(date_col) && date_col %in% cols
      has_ymd      <- all(nzchar(c(ycol,mcol,dcol))) && all(c(ycol,mcol,dcol) %in% cols)
      
      # Build date vector early (used for per-day counts)
      date_vec <- NULL
      if (has_ymd) {
        date_vec <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", dt[[ycol]], dt[[mcol]], dt[[dcol]])))
      } else if (has_date) {
        date_vec <- suppressWarnings(as.Date(dt[[date_col]]))
      }
      
      # Per-day counts by station (if we can form a date)
      more_than_one_row_per_day <- FALSE
      if (!is.null(date_vec)) {
        base_dt <- data.table::data.table(
          id   = if ("id" %in% cols) dt$id else rep("STN", nrow(dt)),
          date = date_vec
        )
        per_day <- base_dt[, .(n_row = .N), by = .(id, date)]
        more_than_one_row_per_day <- any(per_day$n_row > 1L)
        if (more_than_one_row_per_day) reasons <- c(reasons, "inferred hourly: >1 rows per day")
      }
      
      # --- Decide KIND by schema (hourly-first) ---
      if (has_datetime || (has_ymd && (has_hour || more_than_one_row_per_day))) {
        # HOURLY schema
        kind <- "hourly"
        
        # Canonical timestamp for sequential checks (only if we have hour or datetime)
        if (has_datetime) {
          data.table::setnames(dt, dt_col, "timestamp", skip_absent = TRUE)
          if (!inherits(dt$timestamp, "POSIXt")) {
            dt[, timestamp := suppressWarnings(as.POSIXct(
              timestamp, tz = tz_string,
              tryFormats = c(
                "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
                "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
                "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%SZ"
              )
            ))]
          }
        } else if (has_hour && has_ymd) {
          hr_vals <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(dt[[hcol]]))))
          dt[, timestamp := suppressWarnings(as.POSIXct(sprintf(
            "%04d-%02d-%02d %02d:00:00", as.integer(get(ycol)), as.integer(get(mcol)), as.integer(get(dcol)), hr_vals
          ), tz = tz_string))]
        } else {
          # Hour missing: keep kind=hourly (inferred), but no seq check available
          return(list(kind = kind, seq_ok = NA, reasons = c(reasons, "hour column missing; sequentiality not checked")))
        }
        
        # Detect DAILY-ONE-HOUR disguised as hourly (any fixed hour)
        if (has_hour && !is.null(date_vec)) {
          hr_vals <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(dt[[hcol]]))))
          base_dt <- data.table::data.table(
            id   = if ("id" %in% cols) dt$id else rep("STN", nrow(dt)),
            date = date_vec,
            hr   = hr_vals
          )
          per_day <- base_dt[, .(n_row = .N, uniq_hr = data.table::uniqueN(hr)), by = .(id, date)]
          per_stn <- base_dt[, .(uniq_hr_all_days = data.table::uniqueN(hr)), by = id]
          is_daily_one_hour <- (all(per_day$n_row == 1L) &&
                                  all(per_day$uniq_hr == 1L) &&
                                  all(per_stn$uniq_hr_all_days == 1L))
          if (is_daily_one_hour) {
            kind <- "daily_one_hour"
            seq_ok <- base_dt[, .(seq_ok = .is_seq_days(unique(date))), by = id]
            if (any(!seq_ok$seq_ok)) reasons <- c(reasons, "daily_one_hour has gaps in days")
            return(list(kind = kind, seq_ok = all(seq_ok$seq_ok), reasons = reasons))
          }
        }
        
        # Hourly sequentiality (per station)
        if ("id" %in% names(dt)) {
          data.table::setorder(dt, id, timestamp)
          gaps <- dt[, .(gap_count = .count_hour_gaps(timestamp),
                         dup_count = .count_hour_dups(timestamp)), by = id]
          if (any(gaps$gap_count > 0)) {
            reasons <- c(reasons, sprintf("hourly gaps detected for stations: %s",
                                          paste(gaps$id[gaps$gap_count > 0], collapse = ", ")))
          }
          return(list(kind = kind, seq_ok = !any(gaps$gap_count > 0), reasons = reasons))
        } else {
          data.table::setorder(dt, timestamp)
          gc <- .count_hour_gaps(dt$timestamp)
          if (gc > 0) reasons <- c(reasons, "hourly gaps detected (no id)")
          return(list(kind = kind, seq_ok = gc == 0, reasons = reasons))
        }
      }
      
      # --- DAILY schema only if we didn’t match hourly signature ---
      if (has_date || has_ymd) {
        kind <- "daily"
        dts <- if (has_date) suppressWarnings(as.Date(dt[[date_col]])) else
          suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", dt[[ycol]], dt[[mcol]], dt[[dcol]])))
        if ("id" %in% names(dt)) {
          seq_chk <- data.table::data.table(id = dt$id, date = dts)[, .(seq_ok = .is_seq_days(date)), by = id]
          if (any(!seq_chk$seq_ok)) reasons <- c(reasons, sprintf("daily gaps detected for stations: %s",
                                                                  paste(seq_chk$id[!seq_chk$seq_ok], collapse = ", ")))
          return(list(kind = kind, seq_ok = all(seq_chk$seq_ok), reasons = reasons))
        } else {
          return(list(kind = kind, seq_ok = .is_seq_days(dts), reasons = reasons))
        }
      }
      
      list(kind = "unknown", seq_ok = NA, reasons = c(reasons, "unable to classify by schema"))
    }
    
    # Attach provenance attribute
    attach_provenance <- function(out, source, conversion, tz_string, offset_policy, prepared_at, offset_hours = NA) {
      tz_off <- if (is.na(offset_hours)) as.integer(format(as.POSIXlt(Sys.time(), tz = tz_string), "%z")) / 100 else offset_hours
      attr(out, "provenance") <- list(
        source = source,
        conversion = conversion,
        tz = tz_string,
        offset_policy = offset_policy,
        offset_hours = tz_off,
        prepared_at = prepared_at
      )
      out
    }
    
    # --- Reactive state -------------------------------------------------------
    raw_like_rv  <- reactiveVal(NULL)
    daily_src_rv <- reactiveVal(NULL)
    meta_rv      <- reactiveVal(list(
      kind = NA, converted = FALSE, failed = FALSE,
      log = character(),
      t_detect_ms = NA_real_, t_convert_ms = NA_real_, t_total_ms = NA_real_,
      n_rows_in = NA_integer_, n_rows_out = NA_integer_, station_count = NA_integer_,
      run_id = NA_integer_, started_at = NA, finished_at = NA,
      tz = NA_character_, offset_policy = NA_character_, diurnal = NA_character_
    ))
    last_kind_rv <- reactiveVal(NULL)
    run_id_rv    <- reactiveVal(0L)
    gap_observer_rv <- reactiveVal(NULL)  # <-- to destroy & recreate per upload
    
    prep_ready <- eventReactive(
      list(raw_file(), mapping$col_temp(), tz$tz_use(), diurnal_method_reactive()),
      {
        req(raw_file(), nzchar(tz$tz_use()))
        list(
          src = raw_file(),
          tz_string = tz$tz_use(),
          offset_policy = tz$tz_offset_policy(),
          diurnal = diurnal_method_reactive() %||% "BT-default"
        )
      },
      ignoreInit = TRUE
    )
    
    debounced_ready <- debounce(prep_ready, 500)
    
    # --- Main orchestrator ----------------------------------------------------
    observeEvent(debounced_ready(), {
      # destroy any previous gap-policy observer so a fresh selection is required
      prev_obs <- gap_observer_rv()
      if (!is.null(prev_obs)) { prev_obs$destroy(); gap_observer_rv(NULL) }
      
      args <- debounced_ready()
      src <- data.table::as.data.table(args$src)
      tz_string     <- args$tz_string
      offset_policy <- args$offset_policy
      dia           <- args$diurnal
      
      started_at    <- Sys.time()
      t_total_start <- proc.time()
      logs          <- character()
      
      n_rows_in     <- nrow(src)
      station_count <- if ("id" %in% names(src)) length(unique(src$id)) else 1L
      
      # Detection timing
      t_det_start  <- proc.time()
      det          <- detect_resolution(src, tz_string)
      t_detect_ms  <- as.numeric((proc.time() - t_det_start)[["elapsed"]]) * 1000
      if (length(det$reasons)) logs <- c(logs, paste0("[Prepare][INFO] Detect reasons: ", paste(det$reasons, collapse = "; ")))
      
      prev <- last_kind_rv(); if (!identical(prev, det$kind)) last_kind_rv(det$kind)
      run_id <- run_id_rv() + 1L; run_id_rv(run_id)
      
      # 1) HOURLY & sequential OK -> passthrough
      if (det$kind == "hourly" && isTRUE(det$seq_ok)) {
        emit_toast("Detected HOURLY input. No conversion performed.", "message", 4)
        out <- attach_provenance(src, "hourly", "passthrough", tz_string, offset_policy, started_at, offset_hours = NA)
        raw_like_rv(out); daily_src_rv(NULL)
        
        finished_at <- Sys.time()
        t_total_ms  <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
        meta_rv(list(
          kind = "hourly", converted = FALSE, failed = FALSE,
          tz = tz_string, offset_policy = offset_policy, diurnal = dia,
          log = logs, t_detect_ms = t_detect_ms, t_convert_ms = 0.0,
          t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out),
          station_count = station_count, run_id = run_id,
          started_at = started_at, finished_at = finished_at
        ))
        return()
      }
      
      # 2) HOURLY with gaps -> present options (default: fill only the gaps)
      if (det$kind == "hourly" && !isTRUE(det$seq_ok)) {
        # Precompute valid options BEFORE presenting the modal
        can_replace <- daily_noon_is_complete(src)
        
        # Build choices vector dynamically
        choices <- c(
          "Stop and let me fix the source file" = "stop",
          "Fill only the missing hours (daily→hourly for gaps)" = "fill_gaps"
        )
        if (can_replace) {
          choices <- c(choices, "Replace entire stream: use daily NOON (hr=12) and convert to hourly" = "replace_stream")
        }
        
        showModal(modalDialog(
          title = "Hourly sequence has gaps",
          radioButtons(ns("gap_policy"), "Choose an action:", choices = choices, selected = "fill_gaps"),
          footer = tagList(modalButton("Cancel"), actionButton(ns("apply_gap_policy"), "Apply")),
          easyClose = TRUE
        ))
        
        # Create a fresh, single-use observer for this upload
        gap_obs <- observeEvent(input$apply_gap_policy, {
          removeModal()
          choice <- input$gap_policy
          
          if (identical(choice, "stop")) {
            emit_toast("Stopped. Please fix the gaps in your source file.", "warning", 6)
            raw_like_rv(data.frame()); daily_src_rv(NULL)
            
            finished_at <- Sys.time()
            t_total_ms  <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
            meta_rv(list(
              kind = "hourly", converted = FALSE, failed = TRUE,
              tz = tz_string, offset_policy = offset_policy, diurnal = dia,
              log = c(logs, "[Prepare] User chose STOP."), t_detect_ms = t_detect_ms, t_convert_ms = 0.0,
              t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L,
              station_count = station_count, run_id = run_id,
              started_at = started_at, finished_at = finished_at
            ))
            return()
          }
          
          if (identical(choice, "replace_stream")) {
            emit_toast(sprintf("Replacing entire stream using daily NOON and converting to HOURLY (%s)…", dia), "message", 6)
            t_conv_start <- proc.time()
            out <- try(convert_daily_noon_to_hourly(src, tz_string, dia), silent = FALSE)
            t_convert_ms <- as.numeric((proc.time() - t_conv_start)[["elapsed"]]) * 1000
            
            if (inherits(out, "try-error") || is.null(out) || !nrow(out)) {
              emit_toast("Daily NOON → Hourly replacement failed.", "error", 6)
              raw_like_rv(data.frame())
              finished_at <- Sys.time()
              t_total_ms  <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
              meta_rv(list(
                kind = "hourly", converted = TRUE, failed = TRUE,
                tz = tz_string, offset_policy = offset_policy, diurnal = dia,
                log = c(logs, "[Prepare] Replace stream failed."), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms,
                t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L,
                station_count = station_count, run_id = run_id,
                started_at = started_at, finished_at = finished_at
              ))
              return()
            }
            
            out <- attach_provenance(out, "hourly", paste0("replace_stream(dailyNoon→hourly:", dia, ")"),
                                     tz_string, offset_policy, started_at)
            raw_like_rv(out); daily_src_rv(NULL)
            
            finished_at <- Sys.time()
            t_total_ms  <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
            meta_rv(list(
              kind = "hourly", converted = TRUE, failed = FALSE,
              tz = tz_string, offset_policy = offset_policy, diurnal = dia,
              log = c(logs, "[Prepare] Replaced entire stream via daily NOON."), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms,
              t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out),
              station_count = station_count, run_id = run_id,
              started_at = started_at, finished_at = finished_at
            ))
            emit_toast(sprintf("Replaced with %s hourly rows.", nrow(out)), "message", 5)
            return()
          }
          
          # default / fill_gaps
          emit_toast(sprintf("Filling missing hours using daily→hourly (%s)…", dia), "message", 6)
          t_conv_start <- proc.time()
          out <- try(fill_hourly_gaps_with_diurnal(src, tz_string, dia), silent = FALSE)
          t_convert_ms <- as.numeric((proc.time() - t_conv_start)[["elapsed"]]) * 1000
          
          if (inherits(out, "try-error") || is.null(out) || !nrow(out)) {
            emit_toast("Gap filling failed.", "error", 6)
            raw_like_rv(data.frame())
            finished_at <- Sys.time()
            t_total_ms  <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
            meta_rv(list(
              kind = "hourly", converted = TRUE, failed = TRUE,
              tz = tz_string, offset_policy = offset_policy, diurnal = dia,
              log = c(logs, "[Prepare] Gap fill failed."), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms,
              t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L,
              station_count = station_count, run_id = run_id,
              started_at = started_at, finished_at = finished_at
            ))
            return()
          }
          
          out <- attach_provenance(out, "hourly", paste0("gapFill(daily→hourly:", dia, ")"),
                                   tz_string, offset_policy, started_at)
          raw_like_rv(out); daily_src_rv(NULL)
          
          finished_at <- Sys.time()
          t_total_ms  <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
          meta_rv(list(
            kind = "hourly", converted = TRUE, failed = FALSE,
            tz = tz_string, offset_policy = offset_policy, diurnal = dia,
            log = c(logs, "[Prepare] Filled missing hours using daily→hourly."), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms,
            t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out),
            station_count = station_count, run_id = run_id,
            started_at = started_at, finished_at = finished_at
          ))
          emit_toast(sprintf("Filled gaps. Output has %s hourly rows.", nrow(out)), "message", 5)
          return()
        }, once = TRUE, ignoreInit = TRUE)
        
        gap_observer_rv(gap_obs)  # store so we can destroy on next upload
        return()
      }
      
      # 3) DAILY_ONE_HOUR (any fixed hour per day) -> treat as DAILY and convert
      if (det$kind == "daily_one_hour") {
        emit_toast(sprintf("Detected DAILY (fixed-hour) input. Converting to HOURLY (%s)…", dia), "message", 5)
        logs <- c(logs, "[Prepare] Detected DAILY (one-hour-per-day) input.",
                  if (isTRUE(det$seq_ok)) "[Prepare] Daily sequence looks continuous." else "[Prepare][WARN] Daily sequence has gaps.",
                  "[Prepare] Converting to HOURLY …")
        
        # Build daily rows from fixed-hour entries
        dt   <- data.table::as.data.table(src)
        cols <- resolve_cols(dt)
        dt[, date := as.Date(sprintf("%04d-%02d-%02d", get(cols$yr), get(cols$mon), get(cols$day)))]
        daily <- dt[, make_daily_row(.SD, cols),
                    by = .(id_group = if (!is.null(cols$id)) get(cols$id) else rep("STN", .N), date)]
        daily[, id_group := NULL]  # drop grouping helper; id already in rows
        
        t_conv_start <- proc.time()
        res <- try(convert_multi_station_daily_to_hourly(daily, tz_string, dia), silent = FALSE)
        t_convert_ms <- as.numeric((proc.time() - t_conv_start)[["elapsed"]]) * 1000
        
        if (inherits(res, "try-error") || is.null(res$hourly) || !nrow(res$hourly)) {
          emit_toast("Daily→Hourly conversion failed.", "error", 6)
          raw_like_rv(data.frame())
          finished_at <- Sys.time()
          t_total_ms  <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
          meta_rv(list(
            kind = "daily_one_hour", converted = TRUE, failed = TRUE,
            tz = tz_string, offset_policy = offset_policy, diurnal = dia,
            log = logs, t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms,
            t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L,
            station_count = station_count, run_id = run_id,
            started_at = started_at, finished_at = finished_at
          ))
          return()
        }
        
        out <- attach_provenance(res$hourly, "daily_one_hour", paste0("daily→hourly(", dia, ")"),
                                 tz_string, offset_policy, started_at, offset_hours = res$tz_off)
        raw_like_rv(out)
        
        finished_at <- Sys.time()
        t_total_ms  <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
        meta_rv(list(
          kind = "daily_one_hour", converted = TRUE, failed = FALSE,
          tz = tz_string, offset_policy = offset_policy, diurnal = dia,
          log = c(logs, res$logs), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms,
          t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out),
          station_count = station_count, run_id = run_id,
          started_at = started_at, finished_at = finished_at
        ))
        emit_toast(sprintf("Converted %s days to %s hourly rows.", nrow(daily), nrow(out)), "message", 5)
        return()
      }
      
      # 4) DAILY (YMD or date) -> convert to HOURLY
      if (det$kind == "daily") {
        emit_toast(sprintf("Detected DAILY input. Converting to HOURLY (%s)…", dia), "message", 5)
        logs <- c(logs, "[Prepare] Detected DAILY input.",
                  if (isTRUE(det$seq_ok)) "[Prepare] Daily sequence looks continuous." else "[Prepare][WARN] Daily sequence has gaps.",
                  "[Prepare] Converting to HOURLY …")
        
        t_conv_start <- proc.time()
        res <- try(convert_multi_station_daily_to_hourly(src, tz_string, dia), silent = FALSE)
        t_convert_ms <- as.numeric((proc.time() - t_conv_start)[["elapsed"]]) * 1000
        
        if (inherits(res, "try-error") || is.null(res$hourly) || !nrow(res$hourly)) {
          emit_toast("Daily→Hourly conversion failed.", "error", 6)
          raw_like_rv(data.frame())
          finished_at <- Sys.time()
          t_total_ms  <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
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
        
        out <- attach_provenance(res$hourly, "daily", paste0("daily→hourly(", dia, ")"),
                                 tz_string, offset_policy, started_at, offset_hours = res$tz_off)
        raw_like_rv(out)
        
        finished_at <- Sys.time()
        t_total_ms  <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
        meta_rv(list(
          kind = "daily", converted = TRUE, failed = FALSE,
          tz = tz_string, offset_policy = offset_policy, diurnal = dia,
          log = c(logs, res$logs), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms,
          t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out),
          station_count = station_count, run_id = run_id,
          started_at = started_at, finished_at = finished_at
        ))
        emit_toast(sprintf("Converted %s days to %s hourly rows.", nrow(src), nrow(out)), "message", 5)
        return()
      }
      
      # 5) Unknown
      emit_toast("Prepare: could not classify input — check mapping/data.", "warning", 6)
      raw_like_rv(data.frame()); daily_src_rv(NULL)
      
      finished_at <- Sys.time()
      t_total_ms  <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
      meta_rv(list(
        kind = "unknown", converted = NA, failed = FALSE,
        tz = tz_string, offset_policy = offset_policy, diurnal = dia,
        log = logs, t_detect_ms = t_detect_ms, t_convert_ms = 0.0,
        t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L,
        station_count = station_count, run_id = run_id,
        started_at = started_at, finished_at = finished_at
      ))
    })
    
    # Expose outputs
    list(
      raw_uploaded = reactive(raw_file()),
      hourly_file  = reactive(raw_like_rv()),
      src_daily    = reactive(daily_src_rv()),
      prep_meta    = reactive(meta_rv())
    )
  })
}
