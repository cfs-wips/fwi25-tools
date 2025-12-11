
# R/modules/mod_prepare.R
# Robust prepare module:
#  - Hourly-vs-daily detection that prefers Y/M/D/H
#  - Safe datetime handling (strip "(...)" suffixes, never throws)
#  - Gap fill using daily→hourly synthesis for missing hours
#  - Optional replace-stream using daily NOON (hr=12) → hourly
#  - Provenance + timing + lightweight logs
#  - Case-insensitive column resolver across varied schemas

mod_prepare_server <- function(
    id,
    raw_file,
    mapping,
    tz,
    diurnal_method_reactive = shiny::reactive("BT-default"),
    notify = TRUE
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Utilities -----------------------------------------------------------
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    emit_toast <- function(text, type = "message", duration = 5) {
      if (isTRUE(notify)) shiny::showNotification(text, type = type, duration = duration)
    }
    
    # ---- Dependencies (expected to exist) ------------------------------------
    source("ng/make_minmax.r")  # daily_to_minmax()
    source("ng/make_hourly.r")  # minmax_to_hourly()
    
    # Normalize a token (lowercase, remove non-alphanum)
    .norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
    
    # Helper to safely call mapping$col_*() if present
    .map_get <- function(fun) {
      if (is.null(fun)) return(NULL)
      tryCatch(fun(), error = function(e) NULL)
    }
    
    # Find the first column in dt whose normalized name equals any normalized token
    .find_col <- function(dt, tokens) {
      if (is.null(tokens) || !length(tokens)) return(NULL)
      toks <- unique(tokens[!is.na(tokens) & nzchar(tokens)])
      if (!length(toks)) return(NULL)
      nm   <- names(dt)
      nmn  <- vapply(nm, .norm, "", USE.NAMES = FALSE)
      toksn<- vapply(toks, .norm, "", USE.NAMES = FALSE)
      for (t in toksn) {
        i <- which(nmn == t)
        if (length(i)) return(nm[i[1]])
      }
      NULL
    }
    
    # Guess a datetime column name only if needed (safe)
    guess_datetime_col <- function(dt) {
      nm <- names(dt); nmn <- vapply(nm, .norm, "", USE.NAMES = FALSE)
      wanted <- c("datetime","timestamp","timeanddate","dateandtime","date_time","timelocal","localtime","dt")
      for (i in seq_along(nm)) {
        if (nmn[i] %in% wanted) return(nm[i])
      }
      # fallback: try character columns that parse well (strip parentheses like "(-6h)")
      try_formats <- c(
        "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
        "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
        "%H:%M %Y-%m-%d",    "%H:%M %Y/%m/%d",
        "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%SZ"
      )
      safe_parse <- function(x) {
        x2 <- gsub("\\s*\\([^)]*\\)\\s*$", "", x)
        tryCatch(suppressWarnings(as.POSIXct(x2, tz = "UTC", tryFormats = try_formats)),
                 error = function(e) rep(NA_real_, length(x2)))
      }
      for (col in nm) {
        x <- dt[[col]]
        if (!is.character(x)) next
        parsed <- safe_parse(x)
        good_frac <- sum(!is.na(parsed)) / max(1L, length(parsed))
        hour_var  <- if (all(is.na(parsed))) FALSE else (length(unique(format(parsed, "%H"))) > 1)
        if (good_frac >= 0.80 && hour_var) return(col)
      }
      NULL
    }
    
    # ---- Column resolver (HOTFIX: case-insensitive, schema-first) ------------
    resolve_cols <- function(dt) {
      # 1) Prefer schema via common variants, case-insensitive
      yr  <- .find_col(dt, c(.map_get(mapping$col_year),  "yr", "year"))
      mon <- .find_col(dt, c(.map_get(mapping$col_month), "mon","month"))
      day <- .find_col(dt, c(.map_get(mapping$col_day),   "day","date","dom"))
      hr  <- .find_col(dt, c(.map_get(mapping$col_hour),  "hr","hour","hh"))
      
      # 2) Only consider free-form datetime if Y/M/D/H is incomplete
      datetime <- NULL
      if (is.null(yr) || is.null(mon) || is.null(day) || is.null(hr)) {
        datetime <- .find_col(dt, c(.map_get(mapping$col_datetime),
                                    "datetime","timestamp","timeanddate","dateandtime","date_time"))
        if (is.null(datetime)) datetime <- guess_datetime_col(dt)
      }
      
      # 3) id + core met vars (robust, case-insensitive)
      id   <- .find_col(dt, c(.map_get(mapping$col_id), "id","station","stationname","station_name","station name"))
      lat  <- .find_col(dt, c("lat","latitude"))
      long <- .find_col(dt, c("long","lon","longitude"))
      
      temp <- .find_col(dt, c(.map_get(mapping$col_temp), "temp","temperature"))
      rh   <- .find_col(dt, c(.map_get(mapping$col_rh),   "rh","relhum","relativehumidity"))
      ws   <- .find_col(dt, c(.map_get(mapping$col_ws),   "ws","wspd","windspeed","wind"))
      prec <- .find_col(dt, c(.map_get(mapping$col_prec), "prec","rn_1","rain","rain_24","rain24","p24"))
      
      date <- .find_col(dt, c(.map_get(mapping$col_date), "date"))
      
      # Debug to console (helps diagnose misclassification)
      msg <- sprintf("[Prepare][DEBUG] Found cols: id=%s lat=%s long=%s yr=%s mon=%s day=%s hr=%s datetime=%s temp=%s rh=%s ws=%s prec=%s",
                     id, lat, long, yr, mon, day, hr, datetime, temp, rh, ws, prec)
      message(msg)
      
      list(id=id, lat=lat, long=long, yr=yr, mon=mon, day=day, hr=hr,
           temp=temp, rh=rh, ws=ws, prec=prec, datetime=datetime, date=date)
    }
    
    # ---- DST-aware checks ----------------------------------------------------
    .count_hour_gaps <- function(ts) { d <- as.numeric(diff(ts), units = "hours"); sum(d > 1 & d != 2) }  # ignore spring-forward +2
    .count_hour_dups <- function(ts) { d <- as.numeric(diff(ts), units = "hours"); sum(d == 0) }          # fall-back duplicates
    .is_seq_days     <- function(d)  { if (!length(d)) return(FALSE); all(diff(d) == 1) }
    
    # Safe ordering by id + timestamp
    safe_setorder <- function(dt, id_name) {
      if (!is.null(id_name) && id_name %in% names(dt)) {
        data.table::setorderv(dt, c(id_name, "timestamp"))
      } else {
        data.table::setorderv(dt, "timestamp")
      }
    }
    
    # ---- Conversions ---------------------------------------------------------
    convert_daily_to_hourly <- function(df, tz_string, diurnal_method) {
      req_cols <- c("yr", "mon", "day", "temp", "rh", "ws", "prec")
      if (!all(req_cols %in% names(df))) {
        stop("Missing daily columns: ", paste(setdiff(req_cols, names(df)), collapse = ", "))
      }
      tz_off <- as.integer(format(as.POSIXct(Sys.time(), tz = tz_string), "%z")) / 100
      
      mm <- try(daily_to_minmax(df[, .(yr, mon, day, temp, rh, ws, prec)]), silent = TRUE)
      if (inherits(mm, "try-error")) stop(paste("daily_to_minmax() failed:", as.character(mm)))
      names(mm) <- tolower(names(mm))
      
      # carry id/lat/long if present
      mm$id   <- if ("id"   %in% names(df)) df$id[1]   else "STN"
      mm$lat  <- if ("lat"  %in% names(df)) df$lat[1]  else NA_real_
      mm$long <- if ("long" %in% names(df)) df$long[1] else NA_real_
      
      # Build args (defensive to optional formals)
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
    
    # Represent a daily row (prefer hr==12 for temp/RH, else nearest)
    pick_daily_representative <- function(day_dt, col_hr, col_temp, col_rh) {
      hr_vals <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(day_dt[[col_hr]]))))
      idx12 <- which(hr_vals == 12L)
      idx   <- if (length(idx12)) idx12[1] else which.min(abs(hr_vals - 12L))
      list(
        temp = suppressWarnings(as.numeric(day_dt[[col_temp]][idx])),
        rh   = suppressWarnings(as.numeric(day_dt[[col_rh]][idx]))
      )
    }
    
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
    
    synthesize_day_hourly <- function(daily_row, tz_string, diurnal_method) {
      mm <- try(daily_to_minmax(daily_row[, .(yr, mon, day, temp, rh, ws, prec)]), silent = TRUE)
      if (inherits(mm, "try-error")) stop(paste("daily_to_minmax() failed:", as.character(mm)))
      names(mm) <- tolower(names(mm))
      mm$id   <- if ("id"   %in% names(daily_row)) daily_row$id[1]   else "STN"
      mm$lat  <- if ("lat"  %in% names(daily_row)) daily_row$lat[1]  else NA_real_
      mm$long <- if ("long" %in% names(daily_row)) daily_row$long[1] else NA_real_
      
      fml  <- try(formalArgs(minmax_to_hourly), silent = TRUE)
      tz_off <- as.integer(format(as.POSIXct(Sys.time(), tz = tz_string), "%z")) / 100
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
        
        # add rows for truly missing timestamps (anti-join; safe)
        missing_ts <- syn_all[!stn_dt, on = "timestamp"]$timestamp
        if (length(missing_ts)) {
          stn_dt <- data.table::rbindlist(list(stn_dt, syn_all[data.table::J(missing_ts)]), fill = TRUE)
        }
        
        # fill NA values in overlapping timestamps for core variables
        core_fill <- intersect(c(cols$temp, cols$rh, cols$ws, cols$prec, "temp", "Rh", "Wspd", "prec"), names(stn_dt))
        if (length(core_fill)) {
          na_idx <- stn_dt[, .I[Reduce(`|`, lapply(.SD, is.na))], .SDcols = core_fill]
          if (length(na_idx)) {
            na_ts <- stn_dt$timestamp[na_idx]
            fills <- syn_all[.(na_ts), on = "timestamp", nomatch = 0L]
            stn_dt[na_idx, (core_fill) := fills[, ..core_fill]]
          }
        }
        
        stn_dt[, date := NULL][]
      }
      
      out_list <- Map(fill_one_station, stn_list, names(stn_list))
      out <- data.table::rbindlist(out_list, fill = TRUE)
      safe_setorder(out, cols$id)
      out[]
    }
    
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
    
    # ---- Detection (HOTFIX: hourly-first, safe datetime) ---------------------
    detect_resolution <- function(df, tz_string) {
      dt <- data.table::as.data.table(df)
      rc <- resolve_cols(dt)
      reasons <- character()
      
      has_ymd  <- !is.null(rc$yr)  && rc$yr  %in% names(dt) &&
        !is.null(rc$mon) && rc$mon %in% names(dt) &&
        !is.null(rc$day) && rc$day %in% names(dt)
      has_hour <- !is.null(rc$hr)  && rc$hr  %in% names(dt)
      has_dt   <- !is.null(rc$datetime) && rc$datetime %in% names(dt)
      has_date <- !is.null(rc$date) && rc$date %in% names(dt)
      
      # 1) HOURLY via Y/M/D/H (preferred; deterministic timestamp)
      if (has_ymd && has_hour) {
        hr_vals <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(dt[[rc$hr]]))))
        dt[, timestamp := suppressWarnings(as.POSIXct(sprintf(
          "%04d-%02d-%02d %02d:00:00",
          as.integer(get(rc$yr)), as.integer(get(rc$mon)), as.integer(get(rc$day)), hr_vals
        ), tz = tz_string))]
        
        # Detect disguised daily-one-hour (any fixed hr per day)
        date_vec <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", dt[[rc$yr]], dt[[rc$mon]], dt[[rc$day]])))
        base_dt <- data.table::data.table(
          id   = if (!is.null(rc$id)) dt[[rc$id]] else rep("STN", nrow(dt)),
          date = date_vec,
          hr   = hr_vals
        )
        per_day <- base_dt[, .(n_row = .N, uniq_hr = data.table::uniqueN(hr)), by = .(id, date)]
        per_stn <- base_dt[, .(uniq_hr_all_days = data.table::uniqueN(hr)), by = id]
        is_daily_one_hour <- (all(per_day$n_row == 1L) &&
                                all(per_day$uniq_hr == 1L) &&
                                all(per_stn$uniq_hr_all_days == 1L))
        if (is_daily_one_hour) {
          seq_ok <- base_dt[, .(seq_ok = .is_seq_days(unique(date))), by = id]
          if (any(!seq_ok$seq_ok)) reasons <- c(reasons, "daily_one_hour has gaps in days")
          return(list(kind = "daily_one_hour", seq_ok = all(seq_ok$seq_ok), reasons = reasons))
        }
        
        # Hourly sequential check
        id_col <- if (!is.null(rc$id) && rc$id %in% names(dt)) rc$id else NULL
        if (!is.null(id_col)) {
          data.table::setorderv(dt, c(id_col, "timestamp"))
          gaps <- dt[, .(gap_count = .count_hour_gaps(timestamp),
                         dup_count = .count_hour_dups(timestamp)), by = id_col]
          data.table::setnames(gaps, id_col, "id")
          if (any(gaps$gap_count > 0)) {
            reasons <- c(reasons, sprintf("hourly gaps detected: %s", paste(gaps$id[gaps$gap_count > 0], collapse=", ")))
          }
          if (any(gaps$dup_count > 0)) {
            reasons <- c(reasons, sprintf("hourly duplicates detected: %s", paste(gaps$id[gaps$dup_count > 0], collapse=", ")))
          }
          return(list(kind = "hourly", seq_ok = !any(gaps$gap_count > 0), reasons = reasons))
        } else {
          data.table::setorderv(dt, "timestamp")
          d <- as.numeric(diff(dt$timestamp), units="hours")
          if (any(d > 1 & d != 2)) reasons <- c(reasons, "hourly gaps detected (no id)")
          if (any(d == 0))         reasons <- c(reasons, "hourly duplicates detected (no id)")
          return(list(kind = "hourly", seq_ok = !any(d > 1 & d != 2), reasons = reasons))
        }
      }
      
      # 2) HOURLY via datetime (only if Y/M/D/H is missing; safe parsing)
      if (has_dt) {
        x2 <- gsub("\\s*\\([^)]*\\)\\s*$", "", as.character(dt[[rc$datetime]]))
        parsed <- tryCatch(
          suppressWarnings(as.POSIXct(x2, tz = tz_string,
                                      tryFormats = c(
                                        "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
                                        "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
                                        "%H:%M %Y-%m-%d",    "%H:%M %Y/%m/%d",
                                        "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%SZ"
                                      )
          )),
          error = function(e) rep(NA_real_, nrow(dt))
        )
        dt[, timestamp := parsed]
        if (sum(!is.na(parsed)) == 0) {
          return(list(kind = "hourly", seq_ok = NA, reasons = c(reasons, "datetime present but unparsable")))
        }
        data.table::setorderv(dt, "timestamp")
        d <- as.numeric(diff(dt$timestamp), units="hours")
        if (any(d > 1 & d != 2)) reasons <- c(reasons, "hourly gaps detected (datetime)")
        return(list(kind = "hourly", seq_ok = !any(d > 1 & d != 2), reasons = reasons))
      }
      
      # 3) DAILY when hourly signatures absent
      if (!is.null(rc$date) && rc$date %in% names(dt) || has_ymd) {
        dts <- if (!is.null(rc$date) && rc$date %in% names(dt)) {
          suppressWarnings(as.Date(dt[[rc$date]]))
        } else {
          suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", dt[[rc$yr]], dt[[rc$mon]], dt[[rc$day]])))
        }
        seq_ok <- { d <- diff(sort(unique(dts))); all(d == 1) }
        return(list(kind = "daily", seq_ok = seq_ok, reasons = reasons))
      }
      
      # 4) Unknown (verbose names to help diagnose)
      list(kind = "unknown", seq_ok = NA,
           reasons = c(reasons, sprintf("unknown: names=%s", paste(names(dt), collapse=", "))))
    }
    
    # ---- Provenance ----------------------------------------------------------
    attach_provenance <- function(out, source, conversion, tz_string, offset_policy, prepared_at, offset_hours = NA) {
      tz_off <- if (is.na(offset_hours)) as.integer(format(as.POSIXct(Sys.time(), tz = tz_string), "%z")) / 100 else offset_hours
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
    
    # ---- Reactive state ------------------------------------------------------
    raw_like_rv  <- shiny::reactiveVal(NULL)
    daily_src_rv <- shiny::reactiveVal(NULL)
    meta_rv      <- shiny::reactiveVal(list(
      kind = NA, converted = FALSE, failed = FALSE,
      log = character(),
      t_detect_ms = NA_real_, t_convert_ms = NA_real_, t_total_ms = NA_real_,
      n_rows_in = NA_integer_, n_rows_out = NA_integer_, station_count = NA_integer_,
      run_id = NA_integer_, started_at = NA, finished_at = NA,
      tz = NA_character_, offset_policy = NA_character_, diurnal = NA_character_
    ))
    last_kind_rv <- shiny::reactiveVal(NULL)
    run_id_rv    <- shiny::reactiveVal(0L)
    gap_observer_rv <- shiny::reactiveVal(NULL)  # destroy & recreate per upload
    
    prep_ready <- shiny::eventReactive(
      list(raw_file(), mapping$col_temp(), tz$tz_use(), diurnal_method_reactive()),
      {
        shiny::req(raw_file(), nzchar(tz$tz_use()))
        list(
          src = raw_file(),
          tz_string = tz$tz_use(),
          offset_policy = tz$tz_offset_policy(),
          diurnal = diurnal_method_reactive() %||% "BT-default"
        )
      },
      ignoreInit = TRUE
    )
    
    debounced_ready <- shiny::debounce(prep_ready, 500)
    
    # ---- Orchestrator --------------------------------------------------------
    shiny::observeEvent(debounced_ready(), {
      # clear any modal and old observer
      shiny::removeModal()
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
      
      # Detection
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
      
      # 2) HOURLY with gaps -> offer actions (fill gaps or replace stream via daily NOON)
      if (det$kind == "hourly" && !isTRUE(det$seq_ok)) {
        can_replace <- daily_noon_is_complete(src)
        
        choices <- c(
          "Stop and let me fix the source file" = "stop",
          "Fill only the missing hours (daily→hourly for gaps)" = "fill_gaps"
        )
        if (can_replace) {
          choices <- c(choices, "Replace entire stream: use daily NOON (hr=12) and convert to hourly" = "replace_stream")
        }
        
        shiny::showModal(shiny::modalDialog(
          title = "Hourly sequence has gaps",
          shiny::radioButtons(ns("gap_policy"), "Choose an action:", choices = choices, selected = "fill_gaps"),
          footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("apply_gap_policy"), "Apply")),
          easyClose = TRUE
        ))
        
        gap_obs <- shiny::observeEvent(input$apply_gap_policy, {
          shiny::removeModal()
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
        
        gap_observer_rv(gap_obs)
        return()
      }
      
      # 3) DAILY_ONE_HOUR -> treat as DAILY and convert
      if (det$kind == "daily_one_hour") {
        emit_toast(sprintf("Detected DAILY (fixed-hour) input. Converting to HOURLY (%s)…", dia), "message", 5)
        logs <- c(logs, "[Prepare] Detected DAILY (one-hour-per-day) input.",
                  if (isTRUE(det$seq_ok)) "[Prepare] Daily sequence looks continuous." else "[Prepare][WARN] Daily sequence has gaps.",
                  "[Prepare] Converting to HOURLY …")
        
        dt   <- data.table::as.data.table(src)
        rc   <- resolve_cols(dt)
        dt[, date := as.Date(sprintf("%04d-%02d-%02d", get(rc$yr), get(rc$mon), get(rc$day)))]
        daily <- dt[, make_daily_row(.SD, rc),
                    by = .(id_group = if (!is.null(rc$id)) get(rc$id) else rep("STN", .N), date)]
        daily[, id_group := NULL]
        
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
      
      # 4) DAILY -> convert to HOURLY
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
    
    # ---- Expose outputs ------------------------------------------------------
    list(
      raw_uploaded = shiny::reactive(raw_file()),
      hourly_file  = shiny::reactive(raw_like_rv()),
      src_daily    = shiny::reactive(daily_src_rv()),
      prep_meta    = shiny::reactive(meta_rv())
    )
  })
}
