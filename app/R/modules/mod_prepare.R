# R/modules/mod_prepare.R
# Prepare module — robust resolution detection, gap fill (robust Y/M/D/H join),
# type normalization, canonical schema promotion & pruning, and data.table-friendly assignments.
mod_prepare_server <- function(
  id,
  raw_file,
  mapping,
  tz,
  filter,
  diurnal_method_reactive = shiny::reactive("BT-default"),
  notify = TRUE
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    emit_toast <- function(text, type = "message", duration = 5) {
      if (isTRUE(notify)) shiny::showNotification(text, type = type, duration = duration)
    }
    emit_err <- function(where, e) {
      msg <- sprintf("[Prepare][ERROR][%s] %s", where, conditionMessage(e))
      message(msg)
      emit_toast(msg, "error", 6)
    }


    .norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
    .map_get <- function(fun) {
      if (is.null(fun)) {
        return(NULL)
      }
      tryCatch(fun(), error = function(e) NULL)
    }
    dt_prepare_for_add <- function(dt, extra_cols = 7L) {
      data.table::setDT(dt)
      data.table::setalloccol(dt, n = extra_cols)
      dt
    }

    # --- Simple dedup by (id?, year, month, day, hour) ---
    simple_dedup_by_key <- function(dt) {
      data.table::setDT(dt)
      key_cols <- c(if ("id" %in% names(dt)) "id", "year", "month", "day", "hour")
      # If parts missing but timestamp exists, derive them
      if (!all(key_cols %in% names(dt))) {
        if ("timestamp" %in% names(dt) && inherits(dt$timestamp, "POSIXct")) {
          if (!("year" %in% names(dt))) dt[, year := as.integer(format(timestamp, "%Y"))]
          if (!("month" %in% names(dt))) dt[, month := as.integer(format(timestamp, "%m"))]
          if (!("day" %in% names(dt))) dt[, day := as.integer(format(timestamp, "%d"))]
          if (!("hour" %in% names(dt))) dt[, hour := as.integer(format(timestamp, "%H"))]
        }
      }
      keep <- intersect(key_cols, names(dt))
      if (!length(keep)) {
        return(dt[])
      }
      data.table::setorderv(dt, keep)
      dt <- unique(dt, by = keep)
      dt[]
    }
    set_col <- function(dt, name, value) {
      data.table::set(dt, j = name, value = value)
    }
    .find_col <- function(dt, tokens) {
      if (is.null(tokens) || !length(tokens)) {
        return(NULL)
      }
      toks <- unique(tokens[!is.na(tokens) & nzchar(tokens)])
      nm <- names(dt)
      nmn <- vapply(nm, .norm, "", USE.NAMES = FALSE)
      toksn <- vapply(toks, .norm, "", USE.NAMES = FALSE)
      for (t in toksn) {
        i <- which(nmn == t)
        if (length(i)) {
          return(nm[i[1]])
        }
      }
      NULL
    }

    guess_datetime_col <- function(dt) {
      nm <- names(dt)
      nmn <- vapply(nm, .norm, "", USE.NAMES = FALSE)
      wanted <- c("datetime", "timestamp", "timeanddate", "dateandtime", "date_time", "timelocal", "localtime", "dt")
      for (i in seq_along(nm)) {
        if (nmn[i] %in% wanted) {
          return(nm[i])
        }
      }
      try_formats <- c(
        "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
        "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
        "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%S%z"
      )
      safe_parse <- function(x) {
        x2 <- gsub("\\s*\\([^)]*\\)\\s*$", "", x)
        tryCatch(
          suppressWarnings(as.POSIXct(x2, tz = "UTC", tryFormats = try_formats)),
          error = function(e) rep(NA_real_, length(x2))
        )
      }
      for (col in nm) {
        x <- dt[[col]]
        if (!is.character(x)) next
        parsed <- safe_parse(x)
        good_frac <- sum(!is.na(parsed)) / max(1L, length(parsed))
        hour_var <- if (all(is.na(parsed))) FALSE else (length(unique(format(parsed, "%H"))) > 1)
        if (good_frac >= 0.80 && hour_var) {
          return(col)
        }
      }
      NULL
    }

    resolve_cols <- function(dt) {
      yr <- .find_col(dt, c(.map_get(mapping$col_year), "yr", "year"))
      mon <- .find_col(dt, c(.map_get(mapping$col_month), "mon", "month"))
      day <- .find_col(dt, c(.map_get(mapping$col_day), "day", "date", "dom"))
      hr <- .find_col(dt, c(.map_get(mapping$col_hour), "hr", "hour", "hh"))
      datetime <- NULL
      if (is.null(yr) || is.null(mon) || is.null(day) || is.null(hr)) {
        datetime <- .find_col(dt, c(.map_get(mapping$col_datetime), "datetime", "timestamp", "timeanddate", "dateandtime", "date_time"))
        if (is.null(datetime)) datetime <- guess_datetime_col(dt)
      }
      solrad <- .find_col(dt, c(.map_get(mapping$col_solrad), "solar radiation", "solar", "radiation", "shortwave"))
      id <- .find_col(dt, c(.map_get(mapping$col_id), "id", "station", "stationname", "station_name", "station name"))
      lat <- .find_col(dt, c(.map_get(mapping$col_lat), "lat", "latitude"))
      long <- .find_col(dt, c(.map_get(mapping$col_lon), "long", "lon", "longitude"))
      temp <- .find_col(dt, c(.map_get(mapping$col_temp), "temp", "temperature"))
      rh <- .find_col(dt, c(.map_get(mapping$col_rh), "rh", "relhum", "relativehumidity"))
      ws <- .find_col(dt, c(.map_get(mapping$col_ws), "ws", "wspd", "windspeed", "wind"))
      prec <- .find_col(dt, c(.map_get(mapping$col_rain), "prec", "rn_1", "rain", "rain_24", "rain24", "p24"))
      date <- .find_col(dt, c(.map_get(mapping$col_date), "date"))
      message(sprintf(
        "[Prepare][DEBUG] Found cols: id=%s lat=%s long=%s yr=%s mon=%s day=%s hr=%s datetime=%s temp=%s rh=%s ws=%s prec=%s date=%s solrad=%s",
        id, lat, long, yr, mon, day, hr, datetime, temp, rh, ws, prec, date, solrad
      ))
      list(
        id = id, lat = lat, long = long, yr = yr, mon = mon, day = day, hr = hr,
        temp = temp, rh = rh, ws = ws, prec = prec, datetime = datetime, date = date, solrad = solrad
      )
    }

    .count_hour_gaps <- function(ts) {
      d <- as.numeric(diff(ts), units = "hours")
      sum(d > 1 & d != 2)
    }
    .count_hour_dups <- function(ts) {
      d <- as.numeric(diff(ts), units = "hours")
      sum(d == 0)
    }
    .is_seq_days <- function(d) {
      if (!length(d)) {
        return(FALSE)
      }
      all(diff(d) == 1)
    }

    is_blank <- function(x) {
      if (is.null(x)) {
        return(TRUE)
      }
      if (is.character(x)) {
        return(!nzchar(trimws(x)) | is.na(x))
      }
      is.na(x)
    }

    safe_setorder <- function(dt, id_name) {
      if (!is.null(id_name) && id_name %in% names(dt)) data.table::setorderv(dt, c("timestamp", id_name)) else data.table::setorderv(dt, "timestamp")
    }

    fix_single_blank_id <- function(dt) {
      data.table::setDT(dt)
      if (!("id" %in% names(dt))) {
        return(dt[])
      }
      # Normalize to character and trim
      cur <- trimws(as.character(dt[["id"]]))
      ids <- unique(cur)
      # Identify blank & NA
      is_blank_id <- function(x) !nzchar(x) | is.na(x)
      blank_mask <- is_blank_id(ids)

      if (length(ids) == 2 && sum(blank_mask) == 1) {
        # One blank and one real → make all blanks equal to the real id
        real_id <- ids[!blank_mask][1]
        dt[is_blank_id(cur), id := real_id]
      } else if (length(ids) == 2 && "STN" %in% ids) {
        # Only "STN" and a real id → prefer the real id
        real_id <- setdiff(ids, "STN")[1]
        dt[is_blank_id(cur) | cur == "STN", id := real_id]
      }
      dt[]
    }

    normalize_types_hourly <- function(dt, cols) {
      num_candidates <- unique(na.omit(c(
        cols$temp, "temp", "Temp", "temperature", "Temperature",
        cols$rh, "rh", "RH", "Rh", "relhum", "RelativeHumidity", "relativehumidity",
        cols$ws, "ws", "Wspd", "WS", "windspeed", "wind", "wspd",
        cols$prec, "prec", "rn_1", "Rain_24", "rain_24", "rain", "Rain", "p24", "P24",
        "FFMC", "DMC", "DC", "BUI", "ISI", "FWI", cols$lat, cols$long, "lat", "latitude", "long", "lon", "longitude",
        cols$yr, cols$mon, cols$day, cols$hr, "year", "month", "day", "hour"
      )))
      present <- intersect(num_candidates, names(dt))
      for (cn in present) {
        if (is.character(dt[[cn]])) dt[[cn]] <- trimws(dt[[cn]])
        dt[[cn]] <- suppressWarnings(as.numeric(dt[[cn]]))
      }
      for (nm in c("year", "month", "day", "hour")) if (nm %in% names(dt)) dt[[nm]] <- suppressWarnings(as.integer(dt[[nm]]))
      if (!is.null(cols$yr) && cols$yr %in% names(dt)) dt[[cols$yr]] <- suppressWarnings(as.integer(dt[[cols$yr]]))
      if (!is.null(cols$mon) && cols$mon %in% names(dt)) dt[[cols$mon]] <- suppressWarnings(as.integer(dt[[cols$mon]]))
      if (!is.null(cols$day) && cols$day %in% names(dt)) dt[[cols$day]] <- suppressWarnings(as.integer(dt[[cols$day]]))
      if (!is.null(cols$hr) && cols$hr %in% names(dt)) dt[[cols$hr]] <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(dt[[cols$hr]]))))
      dt[]
    }

    # Robust, never-throw parser. Returns POSIXct with NAs where parsing fails.
    parse_char_timestamp <- function(x, tz_string) {
      tz_safe <- if (is.character(tz_string) && nzchar(tz_string)) tz_string else "UTC"
      if (inherits(x, "POSIXt")) {
        res <- as.POSIXct(x, tz = tz_safe)
        data.table::setattr(res, "tzone", tz_safe)
        return(res)
      }
      if (is.numeric(x)) {
        return(suppressWarnings(as.POSIXct(x, origin = "1970-01-01", tz = tz_safe)))
      }
      ch <- tryCatch(as.character(x), error = function(e) rep(NA_character_, length(x)))
      if (!length(ch)) {
        return(as.POSIXct(character(0), tz = tz_safe))
      }
      ch <- gsub("\\s*\\([^)]*\\)\\s*$", "", ch)
      try_formats <- c(
        "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
        "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
        "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%S%z"
      )
      res <- suppressWarnings(tryCatch(as.POSIXct(ch, tz = tz_safe, tryFormats = try_formats), error = function(e) NA))
      if (inherits(res, "POSIXct")) {
        return(res)
      }
      res2 <- suppressWarnings(tryCatch(as.POSIXct(ch, tz = "UTC", tryFormats = try_formats), error = function(e) NA))
      if (inherits(res2, "POSIXct")) {
        return(res2)
      }
      as.POSIXct(rep(NA_character_, length(ch)), tz = tz_safe)
    }

    canonize_time_columns <- function(dt, tz_string, cols) {
      dt <- dt_prepare_for_add(dt)
      if ("long" %in% names(dt) && !("lon" %in% names(dt))) data.table::setnames(dt, "long", "lon")
      if ("timestamp" %in% names(dt)) {
        if (!inherits(dt$timestamp, "POSIXct")) {
          if (is.character(dt$timestamp)) {
            parsed <- parse_char_timestamp(dt$timestamp, tz_string)
            set_col(dt, "timestamp", parsed)
          } else {
            set_col(dt, "timestamp", suppressWarnings(as.POSIXct(dt$timestamp, tz = tz_string)))
          }
        } else {
          data.table::setattr(dt$timestamp, "tzone", tz_string)
        }
        have_ts <- !is.na(dt$timestamp)
        if (any(have_ts)) {
          set_col(dt, "year", as.integer(format(dt$timestamp, "%Y")))
          set_col(dt, "month", as.integer(format(dt$timestamp, "%m")))
          set_col(dt, "day", as.integer(format(dt$timestamp, "%d")))
          set_col(dt, "hour", as.integer(format(dt$timestamp, "%H")))
        }
      }
      need_build <- !("timestamp" %in% names(dt)) || all(is.na(dt$timestamp))
      if (need_build) {
        year_col <- if ("year" %in% names(dt)) "year" else cols$yr
        mon_col <- if ("month" %in% names(dt)) "month" else cols$mon
        day_col <- if ("day" %in% names(dt)) "day" else cols$day
        hr_col <- if ("hour" %in% names(dt)) "hour" else cols$hr
        if (!is.null(year_col) && !is.null(mon_col) && !is.null(day_col) && !is.null(hr_col)) {
          y <- suppressWarnings(as.integer(dt[[year_col]]))
          m <- suppressWarnings(as.integer(dt[[mon_col]]))
          d <- suppressWarnings(as.integer(dt[[day_col]]))
          h <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(dt[[hr_col]]))))
          ok <- !(is.na(y) | is.na(m) | is.na(d) | is.na(h))
          ts_vec <- as.POSIXct(rep(NA_real_, nrow(dt)), tz = tz_string, origin = "1970-01-01")
          set_col(dt, "timestamp", ts_vec)
          if (any(ok)) {
            ts_char <- sprintf("%04d-%02d-%02d %02d:00:00", y[ok], m[ok], d[ok], h[ok])
            ts_new <- suppressWarnings(as.POSIXct(ts_char, tz = tz_string))
            data.table::set(dt, i = which(ok), j = "timestamp", value = ts_new)
          } else {
            emit_toast("[Prepare][WARN] Cannot build timestamp: missing Y/M/D/H parts.", "warning", 5)
          }
        } else {
          emit_toast("[Prepare][WARN] Cannot build timestamp: time parts not found.", "warning", 5)
        }
      }
      if (!("year" %in% names(dt)) && !is.null(cols$yr) && cols$yr %in% names(dt)) data.table::setnames(dt, cols$yr, "year")
      if (!("month" %in% names(dt)) && !is.null(cols$mon) && cols$mon %in% names(dt)) data.table::setnames(dt, cols$mon, "month")
      if (!("day" %in% names(dt)) && !is.null(cols$day) && cols$day %in% names(dt)) data.table::setnames(dt, cols$day, "day")
      if (!("hour" %in% names(dt)) && !is.null(cols$hr) && cols$hr %in% names(dt)) data.table::setnames(dt, cols$hr, "hour")
      for (dup in c(cols$yr, cols$mon, cols$hr)) {
        if (!is.null(dup) && dup %in% names(dt) && !(dup %in% c("year", "month", "hour"))) dt[, (dup) := NULL]
      }
      dt <- normalize_types_hourly(dt, resolve_cols(dt))
      dt[]
    }

    ## Apply explicit mapping to canonical columns (copy; do not rename sources)
    apply_mapping_to_canonical <- function(dt, mapping, tz_string) {
      data.table::setDT(dt)

      # helper to safely get a mapped column name and copy it if present
      map_copy <- function(role_fun, target, cast = identity) {
        nm <- tryCatch(role_fun(), error = function(e) NULL)
        if (is.null(nm) || !nzchar(nm) || !(nm %in% names(dt))) {
          return(invisible(NULL))
        }
        val <- dt[[nm]]
        val <- suppressWarnings(cast(val))
        data.table::set(dt, j = target, value = val)
      }

      nm <- tryCatch(mapping$col_id(), error = function(e) NULL)
      if (!is.null(nm) && nzchar(nm) && nm %in% names(dt)) {
        src <- trimws(as.character(dt[[nm]]))
        if ("id" %in% names(dt)) {
          cur <- as.character(dt[["id"]])
          data.table::set(
            dt,
            j = "id",
            value = ifelse(nzchar(src), src, ifelse(nzchar(cur), cur, "STN"))
          )
        } else {
          data.table::set(
            dt,
            j = "id",
            value = ifelse(nzchar(src), src, "STN")
          )
        }
      }
      # meteorology (numeric)
      map_copy(mapping$col_temp, "temp", as.numeric)
      map_copy(mapping$col_rh, "rh", as.numeric)
      map_copy(mapping$col_ws, "ws", as.numeric)
      map_copy(mapping$col_rain, "prec", as.numeric)

      # date parts (integers)
      map_copy(mapping$col_year, "yr", function(x) suppressWarnings(as.integer(x)))
      map_copy(mapping$col_month, "mon", function(x) suppressWarnings(as.integer(x)))
      map_copy(mapping$col_day, "day", function(x) suppressWarnings(as.integer(x)))
      map_copy(mapping$col_hour, "hr", function(x) suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(x)))))

      # datetime or date+time -> timestamp
      dt_col <- tryCatch(mapping$col_datetime(), error = function(e) NULL)
      date_col <- tryCatch(mapping$col_date(), error = function(e) NULL)
      time_col <- tryCatch(mapping$col_time(), error = function(e) NULL)

      if (!is.null(dt_col) && nzchar(dt_col) && dt_col %in% names(dt)) {
        # If mapped Date-Time is already POSIXct, use it and set tzone; else parse.
        if (inherits(dt[[dt_col]], "POSIXt")) {
          ts <- as.POSIXct(dt[[dt_col]], tz = if (is.character(tz_string) && nzchar(tz_string)) tz_string else "UTC")
          data.table::setattr(ts, "tzone", if (is.character(tz_string) && nzchar(tz_string)) tz_string else "UTC")
          data.table::set(dt, j = "timestamp", value = ts)
        } else {
          parsed <- parse_char_timestamp(dt[[dt_col]], tz_string)
          data.table::set(dt, j = "timestamp", value = parsed)
          # GUARD TOAST: Warn if mapped Date-Time couldn't be parsed.
          if (all(is.na(parsed))) {
            emit_toast("[Prepare][WARN] Mapped Date–Time column could not be parsed; will fall back to Year/Month/Day/Hour later.", "warning", 6)
          }
        }
      } else if (!is.null(date_col) && !is.null(time_col) &&
        date_col %in% names(dt) && time_col %in% names(dt)) {
        date_str <- if (inherits(dt[[date_col]], "Date")) {
          format(dt[[date_col]], "%Y-%m-%d")
        } else {
          as.character(dt[[date_col]])
        }
        time_raw <- dt[[time_col]]
        if (is.numeric(time_raw)) {
          hr <- floor(time_raw)
          mn <- floor((time_raw - hr) * 60)
          sc <- round((time_raw - hr - mn / 60) * 3600)
          time_str <- sprintf(
            "%02d:%02d:%02d",
            pmax(0, pmin(23, hr)),
            pmax(0, pmin(59, mn)),
            pmax(0, pmin(59, sc))
          )
        } else {
          time_str <- as.character(time_raw)
        }
        dt_combined <- paste(date_str, time_str)
        parsed <- parse_char_timestamp(dt_combined, tz_string)
        data.table::set(dt, j = "timestamp", value = parsed)
        # GUARD TOAST: Warn if combined Date+Time couldn't be parsed.
        if (all(is.na(parsed))) {
          emit_toast("[Prepare][WARN] Mapped Date + Time columns could not be parsed; will fall back to Year/Month/Day/Hour later.", "warning", 6)
        }
      }

      # if we have yr/mon/day/hr but no timestamp yet, build it
      needs_ts <- !("timestamp" %in% names(dt)) || all(is.na(dt$timestamp))
      if (needs_ts && all(c("yr", "mon", "day", "hr") %in% names(dt))) {
        ts_new <- safe_posixct_YMDH(dt$yr, dt$mon, dt$day, dt$hr, tz_string)
        data.table::set(dt, j = "timestamp", value = ts_new)
      }

      dt[]
    }

    convert_daily_to_hourly <- function(df, tz_string, diurnal_method) {
      req_cols <- c("yr", "mon", "day", "temp", "rh", "ws", "prec")
      if (!all(req_cols %in% names(df))) stop("Missing daily columns: ", paste(setdiff(req_cols, names(df)), collapse = ", "))
      tz_off <- if ("timezone" %in% names(df)) suppressWarnings(as.integer(na.omit(unique(df$timezone)))[1L]) else as.integer(format(as.POSIXct(Sys.time(), tz = tz_string), "%z")) / 100
      mm <- try(daily_to_minmax(df[, .(yr, mon, day, temp, rh, ws, prec)]), silent = TRUE)
      if (inherits(mm, "try-error")) stop(paste("daily_to_minmax() failed:", as.character(mm)))
      names(mm) <- tolower(names(mm))
      mm$id <- if ("id" %in% names(df)) df$id[1] else "STN"
      mm$lat <- if ("lat" %in% names(df)) df$lat[1] else NA_real_
      mm$long <- if ("long" %in% names(df)) df$long[1] else if ("lon" %in% names(df)) df$lon[1] else NA_real_
      fml <- try(formalArgs(minmax_to_hourly), silent = TRUE)
      args <- list(mm, timezone = tz_off, verbose = FALSE, round_out = NA)
      if (!inherits(fml, "try-error") && "skip_invalid" %in% fml) args$skip_invalid <- TRUE
      if (!inherits(fml, "try-error")) {
        if ("method" %in% fml) args$method <- diurnal_method
        if ("diurnal" %in% fml) args$diurnal <- diurnal_method
      }
      hr <- try(suppressMessages(suppressWarnings(do.call(minmax_to_hourly, args))), silent = TRUE)
      if (inherits(hr, "try-error")) stop(paste("minmax_to_hourly() failed:", as.character(hr)))
      nm_lower <- tolower(names(hr))
      year_col <- if ("yr" %in% nm_lower) "yr" else if ("year" %in% nm_lower) "year" else NA_character_
      month_col <- if ("mon" %in% nm_lower) "mon" else if ("month" %in% nm_lower) "month" else NA_character_
      day_col <- if ("day" %in% nm_lower) "day" else NA_character_
      hour_col <- if ("hr" %in% nm_lower) "hr" else if ("hour" %in% nm_lower) "hour" else NA_character_
      if (!("timestamp" %in% names(hr)) && all(!is.na(c(year_col, month_col, day_col, hour_col)))) {
        hr[, timestamp := safe_posixct_YMDH(get(year_col), get(month_col), get(day_col), get(hour_col), tz_string)]
      }
      if ("long" %in% names(hr) && !("lon" %in% names(hr))) data.table::setnames(hr, "long", "lon")
      list(hourly = hr[], tz_off = tz_off)
    }

    convert_multi_station_daily_to_hourly <- function(df, tz_string, diurnal_method) {
      if (!"id" %in% names(df)) stop("Daily input must have an 'id' column.")
      stn_list <- split(df, df$id)
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
      # Simple dedup
      hourly <- simple_dedup_by_key(hourly)
      list(hourly = hourly, tz_off = hourly_list[[1]]$tz_off, logs = logs)
    }

    pick_daily_representative <- function(day_dt, col_hr, col_temp, col_rh) {
      hr_vals <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(day_dt[[col_hr]]))))
      idx12 <- which(hr_vals == 12L)
      idx <- if (length(idx12)) idx12[1] else which.min(abs(hr_vals - 12L))
      list(
        temp = suppressWarnings(as.numeric(day_dt[[col_temp]][idx])),
        rh = suppressWarnings(as.numeric(day_dt[[col_rh]][idx]))
      )
    }
    make_daily_row <- function(day_dt, cols) {
      repv <- pick_daily_representative(day_dt, cols$hr, cols$temp, cols$rh)
      ws_mean <- if (!is.null(cols$ws) && cols$ws %in% names(day_dt)) mean(suppressWarnings(as.numeric(day_dt[[cols$ws]])), na.rm = TRUE) else NA_real_
      prec_sum <- if (!is.null(cols$prec) && cols$prec %in% names(day_dt)) sum(suppressWarnings(as.numeric(day_dt[[cols$prec]])), na.rm = TRUE) else 0
      data.table::data.table(
        id = if (!is.null(cols$id)) day_dt[[cols$id]][1] else "STN",
        lat = if (!is.null(cols$lat)) suppressWarnings(as.numeric(day_dt[[cols$lat]][1])) else NA_real_,
        long = if (!is.null(cols$long)) suppressWarnings(as.numeric(day_dt[[cols$long]][1])) else NA_real_,
        yr = suppressWarnings(as.integer(day_dt[[cols$yr]][1])),
        mon = suppressWarnings(as.integer(day_dt[[cols$mon]][1])),
        day = suppressWarnings(as.integer(day_dt[[cols$day]][1])),
        temp = repv$temp, rh = repv$rh, ws = ws_mean, prec = prec_sum
      )
    }

    align_syn_to_stn_names <- function(stn_dt, syn_dt, cols) {
      syn_map <- list(
        temp = c(cols$temp, "temp", "Temp", "temperature", "Temperature"),
        rh   = c(cols$rh, "rh", "RH", "Rh", "relhum", "RelativeHumidity", "relativehumidity"),
        ws   = c(cols$ws, "ws", "Wspd", "WS", "wind", "Wind", "windspeed", "wspd"),
        prec = c(cols$prec, "prec", "rn_1", "Rain_24", "rain_24", "rain", "Rain", "p24", "P24")
      )
      for (var in names(syn_map)) {
        stn_target <- syn_map[[var]][syn_map[[var]] %in% names(stn_dt)]
        syn_source <- syn_map[[var]][syn_map[[var]] %in% names(syn_dt)]
        if (length(stn_target) && length(syn_source)) {
          if (syn_source[1] != stn_target[1]) data.table::setnames(syn_dt, syn_source[1], stn_target[1])
        }
      }
      syn_dt
    }

    synthesize_day_hourly <- function(daily_row, tz_string, diurnal_method) {
      mm <- try(daily_to_minmax(daily_row[, .(yr, mon, day, temp, rh, ws, prec)]), silent = TRUE)
      if (inherits(mm, "try-error")) stop(paste("daily_to_minmax() failed:", as.character(mm)))
      names(mm) <- tolower(names(mm))
      # carry station metadata into the min/max structure
      stn_id <- if ("id" %in% names(daily_row)) daily_row$id[1] else "STN"
      stn_lat <- if ("lat" %in% names(daily_row)) suppressWarnings(as.numeric(daily_row$lat[1])) else NA_real_
      stn_lon <- if ("lon" %in% names(daily_row)) {
        suppressWarnings(as.numeric(daily_row$lon[1]))
      } else if ("long" %in% names(daily_row)) suppressWarnings(as.numeric(daily_row$long[1])) else NA_real_
      mm$id <- stn_id
      mm$lat <- stn_lat
      mm$long <- stn_lon
      fml <- try(formalArgs(minmax_to_hourly), silent = TRUE)
      tz_off <- as.integer(format(as.POSIXct(Sys.time(), tz = tz_string), "%z")) / 100
      args <- list(mm, timezone = tz_off, verbose = FALSE, round_out = NA)
      if (!inherits(fml, "try-error") && "skip_invalid" %in% fml) args$skip_invalid <- TRUE
      if (!inherits(fml, "try-error")) {
        if ("method" %in% fml) args$method <- diurnal_method
        if ("diurnal" %in% fml) args$diurnal <- diurnal_method
      }
      hr <- try(suppressMessages(suppressWarnings(do.call(minmax_to_hourly, args))), silent = TRUE)
      if (inherits(hr, "try-error")) stop(paste("minmax_to_hourly() failed:", as.character(hr)))
      # ensure timestamp
      if (!"timestamp" %in% names(hr)) hr[, timestamp := safe_posixct_YMDH(yr, mon, day, hr, tz_string)]
      # **stamp station metadata** onto synthetic rows
      if (!("id" %in% names(hr))) hr[, id := stn_id]
      if (!("lat" %in% names(hr))) hr[, lat := stn_lat]
      if ("long" %in% names(hr) && !("lon" %in% names(hr))) data.table::setnames(hr, "long", "lon")
      if (!("lon" %in% names(hr))) hr[, lon := stn_lon]
      hr[]
    }

    fill_hourly_gaps_with_diurnal <- function(src, tz_string, diurnal_method) {
      dt <- data.table::as.data.table(src)
      dt <- dt_prepare_for_add(dt)
      cols <- resolve_cols(dt)
      stopifnot(all(c(cols$yr, cols$mon, cols$day, cols$hr) %in% names(dt)))

      ts_new <- safe_posixct_YMDH(
        dt[[cols$yr]], dt[[cols$mon]], dt[[cols$day]],
        gsub("[^0-9-]", "", as.character(dt[[cols$hr]])),
        tz_string
      )

      set_col(dt, "timestamp", ts_new)
      set_col(dt, "date", as.Date(dt$timestamp, tz = tz_string))


      if (!is.null(cols$id) && cols$id %in% names(dt)) {
        # Trim and find non-blank IDs
        id_trim <- trimws(as.character(dt[[cols$id]]))
        nonblank <- id_trim[nzchar(id_trim)]
        if (length(nonblank)) {
          # Use the most frequent non-blank id (mode) as canonical label
          canon_id <- names(sort(table(nonblank), decreasing = TRUE))[1]
        } else {
          canon_id <- "STN"
        }
        # Fill blanks/NA with canonical id
        data.table::set(dt,
          j = cols$id,
          value = ifelse(nzchar(id_trim), id_trim, canon_id)
        )
      } else {
        # No id column at all → create one as "STN"
        data.table::set(dt, j = "id", value = rep.int("STN", nrow(dt)))
        cols$id <- "id"
      }
      stn_list <- split(dt, dt[[cols$id]])
      fill_one_station <- function(stn_dt, stn_id) {
        stn_dt <- dt_prepare_for_add(stn_dt)
        day_stats <- stn_dt[, .(
          n_hours = data.table::uniqueN(as.integer(format(timestamp, "%H"))),
          gap_count = {
            d <- as.numeric(diff(sort(timestamp)), units = "hours")
            sum(d > 1 & d != 2)
          }
        ), by = date]
        need <- day_stats[gap_count > 0 | n_hours < 24, date]
        if (!length(need)) {
          return(stn_dt[, date := NULL][])
        }

        stn_id <- {
          if ("id" %in% names(stn_dt)) {
            ids <- trimws(as.character(stn_dt$id))
            nb <- ids[nzchar(ids)]
            if (length(nb)) nb[1] else "STN"
          } else {
            "STN"
          }
        }

        if (!("id" %in% names(stn_dt))) {
          stn_dt[, id := stn_id]
        } else {
          stn_dt[is_blank(id), id := stn_id]
        }

        synth_list <- vector("list", length(need))
        for (i in seq_along(need)) {
          day_i <- need[i]
          day_dt <- stn_dt[date == day_i]
          daily_row <- make_daily_row(day_dt, cols)
          syn <- synthesize_day_hourly(daily_row, tz_string, diurnal_method)
          synth_list[[i]] <- syn
        }
        syn_all <- data.table::rbindlist(synth_list, fill = TRUE)
        syn_all <- align_syn_to_stn_names(stn_dt, syn_all, cols)
        syn_all[, id := stn_id]
        # Fallback: if ID/lat/lon missing in synthetic rows, backfill from station context
        if (!("id" %in% names(syn_all))) syn_all[, id := stn_id]
        if (!("lat" %in% names(syn_all))) {
          stn_lat <- suppressWarnings(as.numeric(na.omit(stn_dt$lat)[1]))
          syn_all[, lat := stn_lat]
        }
        if ("long" %in% names(syn_all) && !("lon" %in% names(syn_all))) data.table::setnames(syn_all, "long", "lon")
        if (!("lon" %in% names(syn_all))) {
          stn_lon <- suppressWarnings(as.numeric(na.omit(if ("lon" %in% names(stn_dt)) stn_dt$lon else stn_dt$long)[1]))
          syn_all[, lon := stn_lon]
        }

        # ---- Robust append: match missing hours by (id, Y/M/D/H), not raw timestamp ----
        stn_dt[, `:=`(
          year  = as.integer(format(timestamp, "%Y")),
          month = as.integer(format(timestamp, "%m")),
          day   = as.integer(format(timestamp, "%d")),
          hour  = as.integer(format(timestamp, "%H"))
        )]
        # derive Y/M/D/H in syn_all even if names differ
        if (!("year" %in% names(syn_all))) syn_all[, year := as.integer(format(timestamp, "%Y"))]
        if (!("month" %in% names(syn_all))) syn_all[, month := as.integer(format(timestamp, "%m"))]
        if (!("day" %in% names(syn_all))) syn_all[, day := as.integer(format(timestamp, "%d"))]
        if (!("hour" %in% names(syn_all))) {
          hcol <- if ("hr" %in% names(syn_all)) "hr" else if ("hour" %in% names(syn_all)) "hour" else NULL
          if (is.null(hcol)) syn_all[, hour := as.integer(format(timestamp, "%H"))] else syn_all[, hour := as.integer(get(hcol))]
        }

        # set keys; include id if present in both
        if (("id" %in% names(stn_dt)) && ("id" %in% names(syn_all))) {
          data.table::setkey(stn_dt, id, year, month, day, hour)
          data.table::setkey(syn_all, id, year, month, day, hour)
          missing_rows <- syn_all[!stn_dt, on = .(id, year, month, day, hour)]
        } else {
          data.table::setkey(stn_dt, year, month, day, hour)
          data.table::setkey(syn_all, year, month, day, hour)
          missing_rows <- syn_all[!stn_dt, on = .(year, month, day, hour)]
        }
        if (nrow(missing_rows)) {
          stn_dt <- data.table::rbindlist(list(stn_dt, missing_rows), fill = TRUE)
        }

        # ensure ID is not NA on newly added rows
        if ("id" %in% names(stn_dt)) stn_dt[is.na(id), id := stn_id]
        if ("id" %in% names(stn_dt)) stn_dt[is_blank(id), id := stn_id]
        # backfill NA core variables from syn_all where possible
        candidate_vars <- unique(na.omit(c(
          cols$temp, cols$rh, cols$ws, cols$prec,
          "temp", "Temp", "temperature", "Temperature", "rh", "RH", "Rh", "relhum", "RelativeHumidity", "relativehumidity",
          "ws", "Wspd", "WS", "wind", "Wind", "windspeed", "wspd", "prec", "rn_1", "Rain_24", "rain_24", "rain", "Rain", "p24", "P24"
        )))
        vars_in_stn <- intersect(candidate_vars, names(stn_dt))
        vars_to_fill <- intersect(vars_in_stn, names(syn_all))
        if (length(vars_to_fill)) {
          na_idx <- stn_dt[, .I[Reduce(`|`, lapply(.SD, function(x) is.na(x) | is.nan(x)))], .SDcols = vars_to_fill]
          if (length(na_idx)) {
            idx_tbl <- stn_dt[na_idx, .(id = if ("id" %in% names(stn_dt)) id else NA_character_, year, month, day, hour)]
            if (("id" %in% names(syn_all)) && ("id" %in% names(idx_tbl))) {
              fills <- syn_all[idx_tbl, on = .(id, year, month, day, hour), nomatch = 0L]
            } else {
              fills <- syn_all[idx_tbl, on = .(year, month, day, hour), nomatch = 0L]
            }
            if (nrow(fills)) stn_dt[na_idx, (vars_to_fill) := fills[, ..vars_to_fill]]
          }
        }
        stn_dt[, date := NULL][]
      }

      out_list <- Map(fill_one_station, stn_list, names(stn_list))
      out <- data.table::rbindlist(out_list, fill = TRUE)
      # Simple dedup on final combined output
      out <- simple_dedup_by_key(out)
      safe_setorder(out, cols$id %||% NULL)
      if ("id" %in% names(out)) out[!nzchar(trimws(as.character(id))) | is.na(id), id := "STN"]
      out[]
    }

    daily_noon_is_complete <- function(src) {
      dt <- data.table::as.data.table(src)
      cols <- resolve_cols(dt)
      if (!all(c(cols$yr, cols$mon, cols$day, cols$hr) %in% names(dt))) {
        return(FALSE)
      }
      dt[, date := as.Date(sprintf("%04d-%02d-%02d", dt[[cols$yr]], dt[[cols$mon]], dt[[cols$day]]))]
      dt[, hr := as.integer(gsub("[^0-9-]", "", as.character(dt[[cols$hr]])))]
      id_vec <- if (!is.null(cols$id) && cols$id %in% names(dt)) dt[[cols$id]] else rep.int("STN", nrow(dt))
      per_day <- dt[, .(has_noon = any(hr == 12L), noon_count = sum(hr == 12L)), by = .(id = id_vec, date)]
      all(per_day$has_noon & per_day$noon_count == 1L) &&
        all(dt[, .(.is_seq_days(unique(date))), by = .(id = id_vec)]$V1)
    }

    convert_daily_noon_to_hourly <- function(src, tz_string, diurnal_method) {
      dt <- data.table::as.data.table(src)
      cols <- resolve_cols(dt)
      stopifnot(all(c(cols$yr, cols$mon, cols$day, cols$hr) %in% names(dt)))
      dt[, hr := as.integer(gsub("[^0-9-]", "", as.character(dt[[cols$hr]])))]
      noon <- dt[hr == 12L]
      id_vec_noon <- if (!is.null(cols$id) && cols$id %in% names(noon)) noon[[cols$id]] else rep.int("STN", nrow(noon))
      daily <- noon[, .(
        id   = id_vec_noon,
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


    promote_canonical_vars <- function(dt, cols, mapping = NULL) {
      data.table::setDT(dt)

      # Normalize "long" -> "lon" if needed
      if ("long" %in% names(dt) && !("lon" %in% names(dt))) {
        data.table::setnames(dt, "long", "lon")
      }

      # --- Canonical renames using detected mapping (no creation yet) -------------
      if (!("id" %in% names(dt)) && !is.null(cols$id) && cols$id %in% names(dt)) data.table::setnames(dt, cols$id, "id")
      if (!("lat" %in% names(dt)) && !is.null(cols$lat) && cols$lat %in% names(dt)) data.table::setnames(dt, cols$lat, "lat")
      if (!("lon" %in% names(dt)) && !is.null(cols$long) && cols$long %in% names(dt)) data.table::setnames(dt, cols$long, "lon")
      if (!("temp" %in% names(dt)) && !is.null(cols$temp) && cols$temp %in% names(dt)) data.table::setnames(dt, cols$temp, "temp")
      if (!("rh" %in% names(dt)) && !is.null(cols$rh) && cols$rh %in% names(dt)) data.table::setnames(dt, cols$rh, "rh")
      if (!("ws" %in% names(dt)) && !is.null(cols$ws) && cols$ws %in% names(dt)) data.table::setnames(dt, cols$ws, "ws")

      # Precip: pick first available candidate and call it "prec"
      if (!("prec" %in% names(dt))) {
        prec_candidates <- intersect(c(cols$prec, "prec", "rn_1", "Rain_24", "rain_24", "rain", "p24", "P24"), names(dt))
        if (length(prec_candidates)) data.table::setnames(dt, prec_candidates[1], "prec")
      }

      # Date parts
      if (!("year" %in% names(dt)) && !is.null(cols$yr) && cols$yr %in% names(dt)) data.table::setnames(dt, cols$yr, "year")
      if (!("month" %in% names(dt)) && !is.null(cols$mon) && cols$mon %in% names(dt)) data.table::setnames(dt, cols$mon, "month")
      if (!("day" %in% names(dt)) && !is.null(cols$day) && cols$day %in% names(dt)) data.table::setnames(dt, cols$day, "day")
      if (!("hour" %in% names(dt)) && !is.null(cols$hr) && cols$hr %in% names(dt)) data.table::setnames(dt, cols$hr, "hour")

      # --- NEW: Promote Solar Radiation to optional canonical 'sr' ----------------
      # If there's a mapping$col_solrad(), use it; otherwise fall back to common names.
      sr_mapped <- NULL
      if (!is.null(mapping)) {
        sr_mapped <- tryCatch(mapping$col_solrad(), error = function(e) NULL)
      }
      if (!("solrad" %in% names(dt))) {
        sr_candidates <- c(
          sr_mapped, "sr", "SR", "solar", "srad", "SolarRadiation",
          "solar_radiation", "solarradiation", "rad", "Rs"
        )
        sr_candidates <- intersect(sr_candidates[!is.na(sr_candidates) & nzchar(sr_candidates)], names(dt))
        if (length(sr_candidates)) data.table::setnames(dt, sr_candidates[1], "solrad")
      }

      if (!("id" %in% names(dt))) {
        data.table::set(dt, j = "id", value = rep.int("STN", nrow(dt)))
        # } else if (all(is.na(dt$id))) {
        #   dt[, id := "STN"]
      } else {
        dt[, id := {
          ch <- trimws(as.character(id))
          ifelse(nzchar(ch), ch, "STN")
        }]
      }

      # --- NEW: Stamp manual lat/lon defaults (only if missing or all NA) --------
      if (!is.null(mapping)) {
        lat_val <- tryCatch(mapping$manual_lat(), error = function(e) NULL)
        lon_val <- tryCatch(mapping$manual_lon(), error = function(e) NULL)

        # Accept only single numerics to avoid accidental vectors
        if (is.numeric(lat_val) && length(lat_val) == 1) {
          if (!("lat" %in% names(dt)) || all(is.na(dt$lat))) {
            data.table::set(dt, j = "lat", value = as.numeric(lat_val))
          }
        }
        if (is.numeric(lon_val) && length(lon_val) == 1) {
          if (!("lon" %in% names(dt)) || all(is.na(dt$lon))) {
            data.table::set(dt, j = "lon", value = as.numeric(lon_val))
          }
        }
      }

      dt[]
    }


    prune_to_canonical <- function(dt) {
      keep <- c("id", "lat", "lon", "year", "month", "day", "hour", "temp", "rh", "ws", "prec", "timestamp", "solrad")
      present <- keep[keep %in% names(dt)]
      dt <- dt[, ..present]
      data.table::setcolorder(dt, present)
      dt[]
    }

    validate_canonical <- function(dt) {
      needed <- c("id", "lat", "lon", "year", "month", "day", "hour", "temp", "rh", "ws", "prec", "timestamp", "solrad")
      missing <- setdiff(needed, names(dt))
      if (length(missing)) stop(sprintf("Canonical schema missing: %s", paste(missing, collapse = ", ")))
      invisible(TRUE)
    }

    safe_posixct_YMDH <- function(y, m, d, h, tz_string) {
      # Build character timestamps safely; coerce parts to integers
      yy <- suppressWarnings(as.integer(y))
      mm <- suppressWarnings(as.integer(m))
      dd <- suppressWarnings(as.integer(d))
      hh <- suppressWarnings(as.integer(h))
      ts_char <- sprintf("%04d-%02d-%02d %02d:00:00", yy, mm, dd, hh)
      # Always catch errors; return NA timestamps on failure
      tryCatch(
        suppressWarnings(as.POSIXct(ts_char, tz = tz_string)),
        error = function(e) as.POSIXct(rep(NA_character_, length(ts_char)), tz = tz_string)
      )
    }

    # --- NEW: Trim source by filter start_date (keep rows on/after the date) ---
    trim_src_by_start_date <- function(df, start_date, tz_string) {
      # If no start_date, return as-is
      if (is.null(start_date)) {
        return(data.table::as.data.table(df))
      }

      dt <- data.table::as.data.table(df)
      dt <- dt_prepare_for_add(dt)
      cols <- resolve_cols(dt)

      # Try to get a usable per-row Date vector
      date_vec <- NULL

      # 1) Prefer timestamp if present (canonicalize if needed)
      if ("timestamp" %in% names(dt)) {
        # ensure POSIXct timestamp (non-throwing)
        dt <- tryCatch(canonize_time_columns(dt, tz_string, cols), error = function(e) dt)
        date_vec <- suppressWarnings(as.Date(dt$timestamp, tz = tz_string))
      }

      # 2) Fallback: build Date from Y/M/D parts
      if (is.null(date_vec) || all(is.na(date_vec))) {
        if (!is.null(cols$yr) && !is.null(cols$mon) && !is.null(cols$day) &&
          cols$yr %in% names(dt) && cols$mon %in% names(dt) && cols$day %in% names(dt)) {
          y <- suppressWarnings(as.integer(dt[[cols$yr]]))
          m <- suppressWarnings(as.integer(dt[[cols$mon]]))
          d <- suppressWarnings(as.integer(dt[[cols$day]]))
          date_vec <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, m, d)))
        }
      }

      # 3) Last resort: mapped or native 'date' column
      if (is.null(date_vec) || all(is.na(date_vec))) {
        if (!is.null(cols$date) && cols$date %in% names(dt)) {
          date_vec <- suppressWarnings(as.Date(dt[[cols$date]]))
        }
      }

      # Could not derive any usable date; warn and return unchanged
      if (is.null(date_vec) || all(is.na(date_vec))) {
        emit_toast(sprintf(
          "[Prepare][WARN] Could not trim: no usable date/timestamp; start_date=%s",
          as.character(start_date)
        ), "warning", 6)
        return(dt[])
      }

      keep_idx <- which(date_vec >= start_date)

      # If nothing remains, warn and return 0-row DT (caller will handle downstream)
      if (!length(keep_idx)) {
        emit_toast(sprintf(
          "[Prepare][WARN] Trimmed away all rows prior to %s; nothing left to prepare.",
          as.character(start_date)
        ), "warning", 7)
        return(dt[0])
      }

      trimmed <- dt[keep_idx]
      trimmed[]
    }


    detect_resolution <- function(df, tz_string) {
      dt <- data.table::as.data.table(df)
      rc <- resolve_cols(dt)
      reasons <- character()
      message(sprintf("[Prepare][DEBUG] Names=%s", paste(names(dt), collapse = ", ")))
      message(sprintf(
        "[Prepare][DEBUG] Detector flags: has_ymd=%s has_hour=%s has_dt=%s has_date=%s",
        !is.null(rc$yr) && rc$yr %in% names(dt), !is.null(rc$hr) && rc$hr %in% names(dt), !is.null(rc$datetime) && rc$datetime %in% names(dt), !is.null(rc$date) && rc$date %in% names(dt)
      ))
      has_ymd <- !is.null(rc$yr) && rc$yr %in% names(dt) && !is.null(rc$mon) && rc$mon %in% names(dt) && !is.null(rc$day) && rc$day %in% names(dt)
      has_hour <- !is.null(rc$hr) && rc$hr %in% names(dt)
      has_dt <- !is.null(rc$datetime) && rc$datetime %in% names(dt)
      has_date <- !is.null(rc$date) && rc$date %in% names(dt)

      if (has_ymd && has_hour) {
        hr_vals <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(dt[[rc$hr]]))))
        dt <- dt_prepare_for_add(dt)
        set_col(dt, "timestamp", safe_posixct_YMDH(dt[[rc$yr]], dt[[rc$mon]], dt[[rc$day]], hr_vals, tz_string))
        date_vec <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", dt[[rc$yr]], dt[[rc$mon]], dt[[rc$day]])))
        base_dt <- data.table::data.table(
          id = if (!is.null(rc$id) && rc$id %in% names(dt)) dt[[rc$id]] else rep.int("STN", nrow(dt)),
          date = date_vec, hr = hr_vals
        )
        per_day <- base_dt[, .(n_row = .N, uniq_hr = data.table::uniqueN(hr)), by = .(id, date)]
        if (mean(per_day$uniq_hr > 1, na.rm = TRUE) >= 0.30 || data.table::uniqueN(hr_vals) >= 8L) {
          id_col <- if (!is.null(rc$id) && rc$id %in% names(dt)) rc$id else NULL
          if (!is.null(id_col)) {
            data.table::setorderv(dt, c(id_col, "timestamp"))
            gaps <- dt[, .(gap_count = .count_hour_gaps(timestamp), dup_count = .count_hour_dups(timestamp)), by = id_col]
            data.table::setnames(gaps, id_col, "id")
            if (any(gaps$gap_count > 0)) reasons <- c(reasons, sprintf("hourly gaps detected: %s", paste(gaps$id[gaps$gap_count > 0], collapse = ", ")))
            if (any(gaps$dup_count > 0)) reasons <- c(reasons, sprintf("hourly duplicates detected: %s", paste(gaps$id[gaps$dup_count > 0], collapse = ", ")))
            return(list(kind = "hourly", seq_ok = !any(gaps$gap_count > 0), reasons = reasons))
          } else {
            data.table::setorderv(dt, "timestamp")
            d <- as.numeric(diff(dt$timestamp), units = "hours")
            if (any(d > 1 & d != 2)) reasons <- c(reasons, "hourly gaps detected (no id)")
            if (any(d == 0)) reasons <- c(reasons, "hourly duplicates detected (no id)")
            return(list(kind = "hourly", seq_ok = !any(d > 1 & d != 2), reasons = reasons))
          }
        }
        per_stn <- base_dt[, .(uniq_hr_all_days = data.table::uniqueN(hr)), by = id]
        is_daily_one_hour <- (mean(per_day$n_row == 1L) >= 0.95) && (mean(per_day$uniq_hr == 1L) >= 0.95) && all(per_stn$uniq_hr_all_days == 1L)
        if (is_daily_one_hour) {
          seq_ok <- base_dt[, .(seq_ok = .is_seq_days(unique(date))), by = id]
          if (any(!seq_ok$seq_ok)) reasons <- c(reasons, "daily_one_hour has gaps in days")
          return(list(kind = "daily_one_hour", seq_ok = all(seq_ok$seq_ok), reasons = reasons))
        }
        id_col <- if (!is.null(rc$id) && rc$id %in% names(dt)) rc$id else NULL
        if (!is.null(id_col)) {
          data.table::setorderv(dt, c(id_col, "timestamp"))
          gaps <- dt[, .(gap_count = .count_hour_gaps(timestamp), dup_count = .count_hour_dups(timestamp)), by = id_col]
          data.table::setnames(gaps, id_col, "id")
          if (any(gaps$gap_count > 0)) reasons <- c(reasons, sprintf("hourly gaps detected: %s", paste(gaps$id[gaps$gap_count > 0], collapse = ", ")))
          if (any(gaps$dup_count > 0)) reasons <- c(reasons, sprintf("hourly duplicates detected: %s", paste(gaps$id[gaps$dup_count > 0], collapse = ", ")))
          return(list(kind = "hourly", seq_ok = !any(gaps$gap_count > 0), reasons = reasons))
        } else {
          data.table::setorderv(dt, "timestamp")
          d <- as.numeric(diff(dt$timestamp), units = "hours")
          if (any(d > 1 & d != 2)) reasons <- c(reasons, "hourly gaps detected (no id)")
          if (any(d == 0)) reasons <- c(reasons, "hourly duplicates detected (no id)")
          return(list(kind = "hourly", seq_ok = !any(d > 1 & d != 2), reasons = reasons))
        }
      }

      if (has_dt) {
        x2 <- gsub("\\s*\\([^)]*\\)\\s*$", "", as.character(dt[[rc$datetime]]))
        parsed <- tryCatch(
          suppressWarnings(as.POSIXct(x2, tz = tz_string, tryFormats = c(
            "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
            "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
            "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%S%z"
          ))),
          error = function(e) rep(NA_real_, nrow(dt))
        )
        set_col(dt, "timestamp", parsed)
        if (sum(!is.na(parsed)) == 0) {
          return(list(kind = "hourly", seq_ok = NA, reasons = c(reasons, "datetime present but unparsable")))
        }
        data.table::setorderv(dt, "timestamp")
        d <- as.numeric(diff(dt$timestamp), units = "hours")
        if (any(d > 1 & d != 2)) reasons <- c(reasons, "hourly gaps detected (datetime)")
        return(list(kind = "hourly", seq_ok = !any(d > 1 & d != 2), reasons = reasons))
      }

      if (!has_hour && !has_dt && has_ymd) {
        dts <- if (!is.null(rc$date) && rc$date %in% names(dt)) suppressWarnings(as.Date(dt[[rc$date]])) else suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", dt[[rc$yr]], dt[[rc$mon]], dt[[rc$day]])))
        seq_ok <- {
          d <- diff(sort(unique(dts)))
          all(d == 1)
        }
        return(list(kind = "daily", seq_ok = seq_ok, reasons = reasons))
      }

      list(kind = "unknown", seq_ok = NA, reasons = c(reasons, sprintf("unknown: names=%s", paste(names(dt), collapse = ", "))))
    }

    attach_provenance <- function(out, source, conversion, tz_string, offset_policy, prepared_at, offset_hours = NA, start_date_used = NULL) {
      tz_off <- if (is.na(offset_hours)) as.integer(format(as.POSIXct(Sys.time(), tz = tz_string), "%z")) / 100 else offset_hours
      # include mapping_signature for cache/telemetry clarity
      sig <- paste(
        .map_get(mapping$col_temp) %||% "",
        .map_get(mapping$col_rh) %||% "",
        .map_get(mapping$col_ws) %||% "",
        .map_get(mapping$col_rain) %||% "",
        .map_get(mapping$col_year) %||% "",
        .map_get(mapping$col_month) %||% "",
        .map_get(mapping$col_day) %||% "",
        .map_get(mapping$col_hour) %||% "",
        .map_get(mapping$col_datetime) %||% "",
        .map_get(mapping$col_date) %||% "",
        .map_get(mapping$col_solrad) %||% "",
        .map_get(mapping$col_id) %||% "",
        sep = "|"
      )
      attr(out, "provenance") <- list(
        source = source, conversion = conversion, tz = tz_string, offset_policy = offset_policy,
        offset_hours = tz_off, prepared_at = prepared_at, mapping_signature = sig,
        start_date_used = if (is.null(start_date_used)) NA_character_ else as.character(start_date_used)
      )
      out
    }

    raw_like_rv <- shiny::reactiveVal(NULL)
    daily_src_rv <- shiny::reactiveVal(NULL)
    meta_rv <- shiny::reactiveVal(list(kind = NA, converted = FALSE, failed = FALSE, log = character(), t_detect_ms = NA_real_, t_convert_ms = NA_real_, t_total_ms = NA_real_, n_rows_in = NA_integer_, n_rows_out = NA_integer_, station_count = NA_integer_, run_id = NA_integer_, started_at = NA, finished_at = NA, tz = NA_character_, offset_policy = NA_character_, diurnal = NA_character_))
    last_kind_rv <- shiny::reactiveVal(NULL)
    run_id_rv <- shiny::reactiveVal(0L)
    gap_observer_rv <- shiny::reactiveVal(NULL)
    busy_rv <- shiny::reactiveVal(FALSE)

    mapping_fp <- shiny::reactive({
      paste0(
        .map_get(mapping$col_year) %||% "",
        .map_get(mapping$col_month) %||% "",
        .map_get(mapping$col_day) %||% "",
        .map_get(mapping$col_hour) %||% "",
        .map_get(mapping$col_datetime) %||% "",
        .map_get(mapping$col_date) %||% "",
        .map_get(mapping$col_id) %||% "",
        .map_get(mapping$col_temp) %||% "",
        .map_get(mapping$col_rh) %||% "",
        .map_get(mapping$col_ws) %||% "",
        .map_get(mapping$col_rain) %||% "",
        .map_get(mapping$manual_lat) %||% "",
        .map_get(mapping$manual_lon) %||% "",
        .map_get(mapping$col_solrad) %||% "",
        .map_get(mapping$col_id) %||% ""
      )
    })

    prep_ready <- shiny::eventReactive(
      list(
        raw_file(), # file
        tz$tz_ready(), # gate: have a zone
        tz$tz_use(), # re-fire when IANA zone changes
        tz$tz_offset_policy(), # std/modal policy changes
        diurnal_method_reactive(), # diurnal method changes
        mapping_fp(), # column mapping fingerprint changes
        mapping$manual_lat(), # add numeric lat trigger
        mapping$manual_lon(), # add numeric lon trigger
        filter$start_date() # added a trigger on start date
      ),
      {
        shiny::req(raw_file(), isTRUE(tz$tz_ready()))
        tz_use_val <- tz$tz_use()
        shiny::req(is.character(tz_use_val), nzchar(tz_use_val))
        offset_policy_val <- tz$tz_offset_policy()
        if (!is.character(offset_policy_val) || !nzchar(offset_policy_val)) offset_policy_val <- "std"
        dm <- diurnal_method_reactive()
        if (!is.character(dm) || !nzchar(dm)) dm <- "BT-default"
        message(sprintf("[Prepare][INFO] Using TZ=%s (policy=%s)", tz_use_val, offset_policy_val))
        start_date_val <- tryCatch(filter$start_date(), error = function(e) NULL)
        list(
          src = raw_file(),
          tz_string = tz_use_val,
          offset_policy = offset_policy_val,
          diurnal = dm,
          start_date = start_date_val
        )
      },
      ignoreInit = TRUE
    )

    # For TEST
    observeEvent(prep_ready(), ignoreInit = TRUE, {
      args <- prep_ready()
      message(sprintf(
        "[Prepare][TRACE] prep_ready fired at %s (tz=%s, diurnal=%s)",
        format(Sys.time(), "%H:%M:%S"), args$tz_string, args$diurnal
      ))
    })
    observeEvent(debounced_ready(), ignoreInit = TRUE, {
      message(sprintf("[Prepare][TRACE] debounced_ready fired at %s", format(Sys.time(), "%H:%M:%S")))
    })

    debounced_ready <- shiny::debounce(prep_ready, 500)
    shiny::observeEvent(debounced_ready(), {
      if (isTRUE(busy_rv())) {
        return()
      }
      busy_rv(TRUE)
      on.exit(busy_rv(FALSE), add = TRUE)
      shiny::removeModal()
      prev_obs <- gap_observer_rv()
      if (!is.null(prev_obs)) {
        prev_obs$destroy()
        gap_observer_rv(NULL)
      }

      args <- debounced_ready()

      if (!isTRUE(mapping$ready())) {
        emit_toast("[Prepare][INFO] Mapping invalid — skipping Prepare until valid.", "warning", 5)
        # Optional: clear current outputs to avoid stale state
        raw_like_rv(data.frame())
        daily_src_rv(NULL)
        return()
      }

      src <- data.table::as.data.table(args$src)
      tz_string <- args$tz_string
      offset_policy <- args$offset_policy
      dia <- args$diurnal
      started_at <- Sys.time()
      t_total_start <- proc.time()
      logs <- character()
      n_rows_in <- nrow(src)
      station_count <- if ("id" %in% names(src)) length(unique(src$id)) else 1L

      # --- NEW: trim the start of the dataset before detection & conversions ---
      if (!is.null(args$start_date)) {
        before <- nrow(src)
        src <- trim_src_by_start_date(src, args$start_date, tz_string)
        after <- nrow(src)
        trimmed_n <- before - after
        if (trimmed_n > 0) {
          logs <- c(logs, sprintf(
            "[Prepare] Trimmed %d row(s) prior to %s.",
            trimmed_n, format(args$start_date)
          ))
        }
        # Recompute counts after trimming (detection should operate on trimmed source)
        n_rows_in <- nrow(src)
        station_count <- if ("id" %in% names(src)) length(unique(src$id)) else 1L

        # If nothing remains, short-circuit with a friendly message
        if (n_rows_in == 0L) {
          emit_toast("[Prepare] No rows remain after applying start date filter.", "warning", 6)
          raw_like_rv(data.frame())
          daily_src_rv(NULL)
          finished_at <- Sys.time()
          t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
          meta_rv(list(
            kind = "unknown", converted = NA, failed = FALSE, tz = tz_string,
            offset_policy = offset_policy, diurnal = dia, log = logs,
            t_detect_ms = NA_real_, t_convert_ms = 0.0, t_total_ms = t_total_ms,
            n_rows_in = 0L, n_rows_out = 0L, station_count = 0L,
            run_id = run_id_rv() + 1L, started_at = started_at, finished_at = finished_at
          ))
          return()
        }
      }

      t_det_start <- proc.time()
      det <- detect_resolution(src, tz_string)
      t_detect_ms <- as.numeric((proc.time() - t_det_start)[["elapsed"]]) * 1000
      if (length(det$reasons)) logs <- c(logs, paste0("[Prepare][INFO] Detect reasons: ", paste(det$reasons, collapse = "; ")))
      prev <- last_kind_rv()
      if (!identical(prev, det$kind)) last_kind_rv(det$kind)
      run_id <- run_id_rv() + 1L
      run_id_rv(run_id)

      if (det$kind == "hourly" && isTRUE(det$seq_ok)) {
        emit_toast(sprintf("Detected HOURLY input — %d rows across %d station(s). No conversion performed.", n_rows_in, station_count), "message", 4)
        out_dt <- data.table::as.data.table(src)
        # Strict mapping: copy mapped roles into canonical columns (when mapping is ready)
        if (isTRUE(mapping$ready())) out_dt <- apply_mapping_to_canonical(out_dt, mapping, tz_string)
        cols_out <- resolve_cols(out_dt)
        out_dt <- normalize_types_hourly(out_dt, cols_out)
        out_dt <- tryCatch(canonize_time_columns(out_dt, tz_string, cols_out), error = function(e) {
          emit_err("canonize_time_columns(passthrough)", e)
          out_dt
        })
        cols_out <- resolve_cols(out_dt)
        out_dt <- promote_canonical_vars(out_dt, cols_out)
        out_dt <- prune_to_canonical(out_dt)
        try(validate_canonical(out_dt), silent = TRUE)
        out <- attach_provenance(out_dt, "hourly", "passthrough", tz_string, offset_policy, started_at, offset_hours = NA, start_date_used = args$start_date)
        setorderv(out, "timestamp")
        # Simple dedup before publishing
        out <- simple_dedup_by_key(out)
        out <- fix_single_blank_id(out)
        raw_like_rv(out)
        daily_src_rv(NULL)
        finished_at <- Sys.time()
        t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
        meta_rv(list(kind = "hourly", converted = FALSE, failed = FALSE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = logs, t_detect_ms = t_detect_ms, t_convert_ms = 0.0, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out), station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
        return()
      }

      if (det$kind == "hourly" && !isTRUE(det$seq_ok)) {
        cols <- resolve_cols(src)
        src <- dt_prepare_for_add(src)
        ts_src <- safe_posixct_YMDH(
          src[[cols$yr]], src[[cols$mon]], src[[cols$day]],
          gsub("[^0-9-]", "", as.character(src[[cols$hr]])),
          tz_string
        )
        set_col(src, "timestamp", ts_src)
        set_col(src, "date", as.Date(src$timestamp, tz = tz_string))
        id_vec_src <- if (!is.null(cols$id) && cols$id %in% names(src)) src[[cols$id]] else rep.int("STN", nrow(src))
        day_stats <- src[, .(
          n_hours = data.table::uniqueN(as.integer(format(timestamp, "%H"))),
          gap_count = {
            d <- as.numeric(diff(sort(timestamp)), units = "hours")
            sum(d > 1 & d != 2)
          },
          dup_count = {
            d <- as.numeric(diff(sort(timestamp)), units = "hours")
            sum(d == 0)
          }
        ), by = .(id = id_vec_src, date)]
        distinct_days <- data.table::uniqueN(day_stats$date)
        days_with_gaps <- sum(day_stats$gap_count > 0 | day_stats$n_hours < 24)
        can_replace <- daily_noon_is_complete(src)

        choices <- c(
          "stop" = "Stop and let me fix the source file",
          "fill_gaps" = "Fill only the missing hours (daily→hourly for gaps)"
        )
        if (can_replace) choices <- c(choices, "replace_stream" = "Replace entire stream: use daily NOON (hr=12) and convert to hourly")

        shiny::showModal(shiny::modalDialog(
          title = sprintf("Hourly sequence has gaps — %d rows, %d distinct day(s) (%d station%s)", n_rows_in, distinct_days, station_count, if (station_count == 1) "" else "s"),
          shiny::div(
            shiny::tags$p(sprintf("Summary: %d of %d day(s) have gaps or <24 hours; duplicates on %d day(s).", days_with_gaps, distinct_days, sum(day_stats$dup_count > 0))),
            shiny::tags$details(
              shiny::tags$summary("Show affected days"),
              shiny::HTML({
                affected <- day_stats[gap_count > 0 | n_hours < 24][order(date)][1:(.N)]
                if (!nrow(affected)) {
                  "<em>No per-day issues detected (unexpected).</em>"
                } else {
                  paste(
                    sprintf(
                      "%s — %s: hours=%d, gaps=%d, dups=%d",
                      affected$id, as.character(affected$date),
                      affected$n_hours, affected$gap_count, affected$dup_count
                    ),
                    collapse = "<br/>"
                  )
                }
              })
            )
          ),
          shiny::radioButtons(ns("gap_policy"), "Choose an action:", choices = choices, selected = "fill_gaps"),
          footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("apply_gap_policy"), "Apply")),
          easyClose = TRUE, size = "l"
        ))

        gap_obs <- shiny::observeEvent(input$apply_gap_policy,
          {
            shiny::removeModal()
            choice <- input$gap_policy
            if (identical(choice, "Stop and let me fix the source file") || identical(choice, "stop")) {
              emit_toast("Stopped. Please fix the gaps in your source file.", "warning", 6)
              raw_like_rv(data.frame())
              daily_src_rv(NULL)
              finished_at <- Sys.time()
              t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
              meta_rv(list(kind = "hourly", converted = FALSE, failed = TRUE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = c(logs, "[Prepare] User chose STOP."), t_detect_ms = t_detect_ms, t_convert_ms = 0.0, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L, station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
              return()
            }

            if (identical(choice, "Replace entire stream: use daily NOON (hr=12) and convert to hourly") || identical(choice, "replace_stream")) {
              emit_toast(sprintf("Replacing entire stream using daily NOON and converting to HOURLY (%s)…", dia), "message", 6)
              t_conv_start <- proc.time()
              out <- try(convert_daily_noon_to_hourly(src, tz_string, dia), silent = FALSE)
              t_convert_ms <- as.numeric((proc.time() - t_conv_start)[["elapsed"]]) * 1000
              if (inherits(out, "try-error") || is.null(out) || !nrow(out)) {
                emit_err("replace_stream", simpleError("Daily NOON → Hourly replacement failed."))
                raw_like_rv(data.frame())
                finished_at <- Sys.time()
                t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
                meta_rv(list(kind = "hourly", converted = TRUE, failed = TRUE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = c(logs, "[Prepare] Replace stream failed."), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L, station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
                return()
              }
              # Strict mapping: copy mapped roles into canonical columns (when mapping is ready)
              if (isTRUE(mapping$ready())) out <- apply_mapping_to_canonical(out, mapping, tz_string)
              cols_out <- resolve_cols(out)
              out <- normalize_types_hourly(out, cols_out)
              out <- tryCatch(canonize_time_columns(out, tz_string, cols_out), error = function(e) {
                emit_err("canonize_time_columns(replace)", e)
                out
              })
              cols_out <- resolve_cols(out)
              out <- promote_canonical_vars(out, cols_out)
              out <- prune_to_canonical(out)
              try(validate_canonical(out), silent = TRUE)
              out <- attach_provenance(out, "hourly", paste0("replace_stream(dailyNoon→hourly:", dia, ")"), tz_string, offset_policy, started_at, start_date_used = args$start_date)
              setorderv(out, "timestamp")
              # Simple dedup before publishing
              out <- simple_dedup_by_key(out)
              out <- fix_single_blank_id(out)
              raw_like_rv(out)

              daily_src_rv(NULL)
              finished_at <- Sys.time()
              t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
              meta_rv(list(kind = "hourly", converted = TRUE, failed = FALSE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = c(logs, "[Prepare] Replaced entire stream via daily NOON."), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out), station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
              emit_toast(sprintf("Replaced with %s hourly rows.", nrow(out)), "message", 5)
              return()
            }

            emit_toast(sprintf("Filling missing hours using daily→hourly (%s)…", dia), "message", 6)
            t_conv_start <- proc.time()
            out <- try(fill_hourly_gaps_with_diurnal(src, tz_string, dia), silent = FALSE)
            t_convert_ms <- as.numeric((proc.time() - t_conv_start)[["elapsed"]]) * 1000
            if (inherits(out, "try-error") || is.null(out) || !nrow(out)) {
              emit_err("gap_fill", simpleError("Gap filling failed."))
              raw_like_rv(data.frame())
              finished_at <- Sys.time()
              t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
              meta_rv(list(kind = "hourly", converted = TRUE, failed = TRUE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = c(logs, "[Prepare] Gap fill failed."), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L, station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
              return()
            }
            # Strict mapping: copy mapped roles into canonical columns (when mapping is ready)
            if (isTRUE(mapping$ready())) out <- apply_mapping_to_canonical(out, mapping, tz_string)
            cols_out <- resolve_cols(out)
            out <- normalize_types_hourly(out, cols_out)
            out <- tryCatch(canonize_time_columns(out, tz_string, cols_out), error = function(e) {
              emit_err("canonize_time_columns(fill_gaps)", e)
              out
            })
            cols_out <- resolve_cols(out)
            out <- promote_canonical_vars(out, cols_out)
            out <- prune_to_canonical(out)
            try(validate_canonical(out), silent = TRUE)
            out <- attach_provenance(out, "hourly", paste0("gapFill(daily→hourly:", dia, ")"), tz_string, offset_policy, started_at, start_date_used = args$start_date)
            setorderv(out, "timestamp")
            # Simple dedup before publishing
            out <- simple_dedup_by_key(out)
            out <- fix_single_blank_id(out)
            raw_like_rv(out)

            daily_src_rv(NULL)
            finished_at <- Sys.time()
            t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
            meta_rv(list(kind = "hourly", converted = TRUE, failed = FALSE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = c(logs, "[Prepare] Filled missing hours using daily→hourly."), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out), station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
            emit_toast(sprintf("Filled gaps. Output has %s hourly rows.", nrow(out)), "message", 5)
            return()
          },
          ignoreInit = TRUE
        )
        gap_observer_rv(gap_obs)
        return()
      }

      if (det$kind == "daily_one_hour") {
        emit_toast(sprintf("Detected DAILY (fixed-hour) input. Converting to HOURLY (%s)…", dia), "message", 5)
        logs <- c(
          logs,
          "[Prepare] Detected DAILY (one-hour-per-day) input.",
          if (isTRUE(det$seq_ok)) "[Prepare] Daily sequence looks continuous." else "[Prepare][WARN] Daily sequence has gaps.",
          "[Prepare] Converting to HOURLY …"
        )
        dt <- data.table::as.data.table(src)
        rc <- resolve_cols(dt)
        dt[, date := as.Date(sprintf("%04d-%02d-%02d", dt[[rc$yr]], dt[[rc$mon]], dt[[rc$day]]))]
        id_vec_dt <- if (!is.null(rc$id) && rc$id %in% names(dt)) dt[[rc$id]] else rep.int("STN", nrow(dt))
        daily <- dt[, make_daily_row(.SD, rc), by = .(id_group = id_vec_dt, date)]
        daily[, id_group := NULL]
        t_conv_start <- proc.time()
        res <- try(convert_multi_station_daily_to_hourly(daily, tz_string, dia), silent = TRUE)
        t_convert_ms <- as.numeric((proc.time() - t_conv_start)[["elapsed"]]) * 1000
        if (inherits(res, "try-error") || is.null(res$hourly) || !nrow(res$hourly)) {
          emit_err("daily_one_hour→hourly", simpleError("Daily→Hourly conversion failed."))
          raw_like_rv(data.frame())
          finished_at <- Sys.time()
          t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
          meta_rv(list(kind = "daily_one_hour", converted = TRUE, failed = TRUE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = logs, t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L, station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
          return()
        }
        out <- data.table::as.data.table(res$hourly)
        # Strict mapping: copy mapped roles into canonical columns (when mapping is ready)
        if (isTRUE(mapping$ready())) out <- apply_mapping_to_canonical(out, mapping, tz_string)
        cols_out <- resolve_cols(out)
        out <- normalize_types_hourly(out, cols_out)
        out <- tryCatch(canonize_time_columns(out, tz_string, cols_out), error = function(e) {
          emit_err("canonize_time_columns(daily_one_hour)", e)
          out
        })
        cols_out <- resolve_cols(out)
        out <- promote_canonical_vars(out, cols_out)
        out <- prune_to_canonical(out)
        try(validate_canonical(out), silent = TRUE)
        out <- attach_provenance(out, "daily_one_hour", paste0("daily→hourly(", dia, ")"), tz_string, offset_policy, started_at, offset_hours = res$tz_off, start_date_used = args$start_date)
        setorderv(out, "timestamp")
        # Simple dedup before publishing
        out <- simple_dedup_by_key(out)
        out <- fix_single_blank_id(out)
        raw_like_rv(out)
        finished_at <- Sys.time()
        t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
        meta_rv(list(kind = "daily_one_hour", converted = TRUE, failed = FALSE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = c(logs, res$logs), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out), station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
        emit_toast(sprintf("Converted %s days to %s hourly rows.", nrow(daily), nrow(out)), "message", 5)
        return()
      }

      if (det$kind == "daily") {
        emit_toast(sprintf("Detected DAILY input. Converting to HOURLY (%s)…", dia), "message", 5)
        logs <- c(
          logs,
          "[Prepare] Detected DAILY input.",
          if (isTRUE(det$seq_ok)) "[Prepare] Daily sequence looks continuous." else "[Prepare][WARN] Daily sequence has gaps.",
          "[Prepare] Converting to HOURLY …"
        )
        t_conv_start <- proc.time()
        res <- try(convert_multi_station_daily_to_hourly(src, tz_string, dia), silent = TRUE)
        t_convert_ms <- as.numeric((proc.time() - t_conv_start)[["elapsed"]]) * 1000
        if (inherits(res, "try-error") || is.null(res$hourly) || !nrow(res$hourly)) {
          emit_err("daily→hourly", simpleError("Daily→Hourly conversion failed."))
          raw_like_rv(data.frame())
          finished_at <- Sys.time()
          t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
          meta_rv(list(kind = "daily", converted = TRUE, failed = TRUE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = logs, t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L, station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
          return()
        }
        out <- data.table::as.data.table(res$hourly)
        # Strict mapping: copy mapped roles into canonical columns (when mapping is ready)
        if (isTRUE(mapping$ready())) out <- apply_mapping_to_canonical(out, mapping, tz_string)
        cols_out <- resolve_cols(out)
        out <- normalize_types_hourly(out, cols_out)
        out <- tryCatch(canonize_time_columns(out, tz_string, cols_out), error = function(e) {
          emit_err("canonize_time_columns(daily)", e)
          out
        })
        cols_out <- resolve_cols(out)
        out <- promote_canonical_vars(out, cols_out)
        out <- prune_to_canonical(out)
        try(validate_canonical(out), silent = TRUE)
        out <- attach_provenance(out, "daily", paste0("daily→hourly(", dia, ")"), tz_string, offset_policy, started_at, offset_hours = res$tz_off, start_date_used = args$start_date)
        setorderv(out, "timestamp")
        # Simple dedup before publishing
        out <- simple_dedup_by_key(out)
        out <- fix_single_blank_id(out)
        raw_like_rv(out)
        finished_at <- Sys.time()
        t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
        meta_rv(list(kind = "daily", converted = TRUE, failed = FALSE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = c(logs, res$logs), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out), station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
        emit_toast(sprintf("Converted %s days to %s hourly rows.", nrow(src), nrow(out)), "message", 5)
        return()
      }

      emit_toast("Prepare: could not classify input — check mapping/data.", "warning", 6)
      raw_like_rv(data.frame())
      daily_src_rv(NULL)
      finished_at <- Sys.time()
      t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
      meta_rv(list(kind = "unknown", converted = NA, failed = FALSE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = logs, t_detect_ms = t_detect_ms, t_convert_ms = 0.0, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = 0L, station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
    })

    list(
      raw_uploaded = shiny::reactive(raw_file()),
      hourly_file  = shiny::reactive(raw_like_rv()),
      src_daily    = shiny::reactive(daily_src_rv()),
      prep_meta    = shiny::reactive(meta_rv()),
      busy         = shiny::reactive(busy_rv())
    )
  })
}
