# R/modules/mod_prepare.R
# Prepare module — robust resolution detection, gap fill (robust Y/M/D/H join),
# type normalization, canonical schema promotion & pruning, and data.table-friendly assignments.

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

    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    emit_toast <- function(text, type = "message", duration = 5) {
      if (isTRUE(notify)) shiny::showNotification(text, type = type, duration = duration)
    }
    emit_err <- function(where, e) {
      msg <- sprintf("[Prepare][ERROR][%s] %s", where, conditionMessage(e))
      message(msg)
      emit_toast(msg, "error", 6)
    }

    # Dependencies
    source("ng/make_minmax.r") # daily_to_minmax()
    source("ng/make_hourly.r") # minmax_to_hourly()

    .norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
    .map_get <- function(fun) {
      if (is.null(fun)) {
        return(NULL)
      }
      tryCatch(fun(), error = function(e) NULL)
    }

    dt_prepare_for_add <- function(dt, extra_cols = 6L) {
      data.table::setDT(dt)
      data.table::setalloccol(dt, n = extra_cols)
      dt
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
      for (i in seq_along(nm)) if (nmn[i] %in% wanted) {
        return(nm[i])
      }
      try_formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%S%z")
      safe_parse <- function(x) {
        x2 <- gsub("\\s*\\([^)]*\\)\\s*$", "", x)
        tryCatch(suppressWarnings(as.POSIXct(x2, tz = "UTC", tryFormats = try_formats)), error = function(e) rep(NA_real_, length(x2)))
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
      id <- .find_col(dt, c(.map_get(mapping$col_id), "id", "station", "stationname", "station_name", "station name"))
      lat <- .find_col(dt, c(.map_get(mapping$col_lat), "lat", "latitude"))
      long <- .find_col(dt, c(.map_get(mapping$col_lon), "long", "lon", "longitude"))
      temp <- .find_col(dt, c(.map_get(mapping$col_temp), "temp", "temperature"))
      rh <- .find_col(dt, c(.map_get(mapping$col_rh), "rh", "relhum", "relativehumidity"))
      ws <- .find_col(dt, c(.map_get(mapping$col_ws), "ws", "wspd", "windspeed", "wind"))
      prec <- .find_col(dt, c(.map_get(mapping$col_prec), "prec", "rn_1", "rain", "rain_24", "rain24", "p24"))
      date <- .find_col(dt, c(.map_get(mapping$col_date), "date"))
      message(sprintf(
        "[Prepare][DEBUG] Found cols: id=%s lat=%s long=%s yr=%s mon=%s day=%s hr=%s datetime=%s temp=%s rh=%s ws=%s prec=%s date=%s",
        id, lat, long, yr, mon, day, hr, datetime, temp, rh, ws, prec, date
      ))
      list(
        id = id, lat = lat, long = long, yr = yr, mon = mon, day = day, hr = hr,
        temp = temp, rh = rh, ws = ws, prec = prec, datetime = datetime, date = date
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
    safe_setorder <- function(dt, id_name) {
      if (!is.null(id_name) && id_name %in% names(dt)) data.table::setorderv(dt, c(id_name, "timestamp")) else data.table::setorderv(dt, "timestamp")
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

    parse_char_timestamp <- function(x, tz_string) {
      x <- gsub("\\s*\\([^)]*\\)\\s*$", "", x)
      try_formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%S%z")
      tryCatch(suppressWarnings(as.POSIXct(x, tz = tz_string, tryFormats = try_formats)), error = function(e) suppressWarnings(as.POSIXct(x, tz = "UTC", tryFormats = try_formats)))
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
        hr[, timestamp := as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00", as.integer(get(year_col)), as.integer(get(month_col)), as.integer(get(day_col)), as.integer(get(hour_col))), tz = tz_string)]
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
      list(hourly = hourly, tz_off = hourly_list[[1]]$tz_off, logs = logs)
    }

    pick_daily_representative <- function(day_dt, col_hr, col_temp, col_rh) {
      hr_vals <- suppressWarnings(as.integer(gsub("[^0-9-]", "", as.character(day_dt[[col_hr]]))))
      idx12 <- which(hr_vals == 12L)
      idx <- if (length(idx12)) idx12[1] else which.min(abs(hr_vals - 12L))
      list(temp = suppressWarnings(as.numeric(day_dt[[col_temp]][idx])), rh = suppressWarnings(as.numeric(day_dt[[col_rh]][idx])))
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
      if (!"timestamp" %in% names(hr)) hr[, timestamp := as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00", yr, mon, day, hr), tz = tz_string)]
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
      ts_new <- as.POSIXct(sprintf(
        "%04d-%02d-%02d %02d:00:00",
        as.integer(dt[[cols$yr]]), as.integer(dt[[cols$mon]]), as.integer(dt[[cols$day]]),
        as.integer(gsub("[^0-9-]", "", as.character(dt[[cols$hr]])))
      ), tz = tz_string)
      set_col(dt, "timestamp", ts_new)
      set_col(dt, "date", as.Date(dt$timestamp, tz = tz_string))
      stn_list <- if (!is.null(cols$id) && cols$id %in% names(dt)) split(dt, dt[[cols$id]]) else list(STN = dt)
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
          year = as.integer(format(timestamp, "%Y")),
          month = as.integer(format(timestamp, "%m")),
          day = as.integer(format(timestamp, "%d")),
          hour = as.integer(format(timestamp, "%H"))
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
        if ("id" %in% names(stn_dt)) {
          stn_dt[is.na(id), id := stn_id]
        }
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
            # build an index table to join by keys for NA rows
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
      safe_setorder(out, cols$id %||% NULL)
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
      all(per_day$has_noon & per_day$noon_count == 1L) && all(dt[, .(.is_seq_days(unique(date))), by = .(id = id_vec)]$V1)
    }

    convert_daily_noon_to_hourly <- function(src, tz_string, diurnal_method) {
      dt <- data.table::as.data.table(src)
      cols <- resolve_cols(dt)
      stopifnot(all(c(cols$yr, cols$mon, cols$day, cols$hr) %in% names(dt)))
      dt[, hr := as.integer(gsub("[^0-9-]", "", as.character(dt[[cols$hr]])))]
      noon <- dt[hr == 12L]
      id_vec_noon <- if (!is.null(cols$id) && cols$id %in% names(noon)) noon[[cols$id]] else rep.int("STN", nrow(noon))
      daily <- noon[, .(
        id = id_vec_noon,
        lat = if (!is.null(cols$lat)) suppressWarnings(as.numeric(get(cols$lat))) else NA_real_,
        long = if (!is.null(cols$long)) suppressWarnings(as.numeric(get(cols$long))) else NA_real_,
        yr = suppressWarnings(as.integer(get(cols$yr))),
        mon = suppressWarnings(as.integer(get(cols$mon))),
        day = suppressWarnings(as.integer(get(cols$day))),
        temp = suppressWarnings(as.numeric(get(cols$temp))),
        rh = suppressWarnings(as.numeric(get(cols$rh))),
        ws = if (!is.null(cols$ws)) suppressWarnings(as.numeric(get(cols$ws))) else NA_real_,
        prec = if (!is.null(cols$prec)) suppressWarnings(as.numeric(get(cols$prec))) else 0
      )]
      convert_multi_station_daily_to_hourly(daily, tz_string, diurnal_method)$hourly
    }

    promote_canonical_vars <- function(dt, cols) {
      if (!("id" %in% names(dt)) && !is.null(cols$id) && cols$id %in% names(dt)) data.table::setnames(dt, cols$id, "id")
      if (!("lat" %in% names(dt)) && !is.null(cols$lat) && cols$lat %in% names(dt)) data.table::setnames(dt, cols$lat, "lat")
      if ("long" %in% names(dt) && !("lon" %in% names(dt))) data.table::setnames(dt, "long", "lon")
      if (!("lon" %in% names(dt)) && !is.null(cols$long) && cols$long %in% names(dt)) data.table::setnames(dt, cols$long, "lon")
      if (!("temp" %in% names(dt)) && !is.null(cols$temp) && cols$temp %in% names(dt)) data.table::setnames(dt, cols$temp, "temp")
      if (!("rh" %in% names(dt)) && !is.null(cols$rh) && cols$rh %in% names(dt)) data.table::setnames(dt, cols$rh, "rh")
      if (!("ws" %in% names(dt)) && !is.null(cols$ws) && cols$ws %in% names(dt)) data.table::setnames(dt, cols$ws, "ws")
      if (!("prec" %in% names(dt))) {
        prec_candidates <- intersect(c(cols$prec, "prec", "rn_1", "Rain_24", "rain_24", "rain", "p24", "P24"), names(dt))
        if (length(prec_candidates)) data.table::setnames(dt, prec_candidates[1], "prec")
      }
      if (!("year" %in% names(dt)) && !is.null(cols$yr) && cols$yr %in% names(dt)) data.table::setnames(dt, cols$yr, "year")
      if (!("month" %in% names(dt)) && !is.null(cols$mon) && cols$mon %in% names(dt)) data.table::setnames(dt, cols$mon, "month")
      if (!("day" %in% names(dt)) && !is.null(cols$day) && cols$day %in% names(dt)) data.table::setnames(dt, cols$day, "day")
      if (!("hour" %in% names(dt)) && !is.null(cols$hr) && cols$hr %in% names(dt)) data.table::setnames(dt, cols$hr, "hour")
      dt[]
    }

    prune_to_canonical <- function(dt) {
      keep <- c("id", "lat", "lon", "year", "month", "day", "hour", "temp", "rh", "ws", "prec", "timestamp")
      present <- keep[keep %in% names(dt)]
      dt <- dt[, ..present]
      data.table::setcolorder(dt, present)
      dt[]
    }

    validate_canonical <- function(dt) {
      needed <- c("id", "lat", "lon", "year", "month", "day", "hour", "temp", "rh", "ws", "prec", "timestamp")
      missing <- setdiff(needed, names(dt))
      if (length(missing)) stop(sprintf("Canonical schema missing: %s", paste(missing, collapse = ", ")))
      invisible(TRUE)
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
        set_col(dt, "timestamp", suppressWarnings(as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00", as.integer(dt[[rc$yr]]), as.integer(dt[[rc$mon]]), as.integer(dt[[rc$day]]), hr_vals), tz = tz_string)))
        date_vec <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", dt[[rc$yr]], dt[[rc$mon]], dt[[rc$day]])))
        base_dt <- data.table::data.table(id = if (!is.null(rc$id) && rc$id %in% names(dt)) dt[[rc$id]] else rep.int("STN", nrow(dt)), date = date_vec, hr = hr_vals)
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
        parsed <- tryCatch(suppressWarnings(as.POSIXct(x2, tz = tz_string, tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%S%z"))), error = function(e) rep(NA_real_, nrow(dt)))
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

    attach_provenance <- function(out, source, conversion, tz_string, offset_policy, prepared_at, offset_hours = NA) {
      tz_off <- if (is.na(offset_hours)) as.integer(format(as.POSIXct(Sys.time(), tz = tz_string), "%z")) / 100 else offset_hours
      attr(out, "provenance") <- list(source = source, conversion = conversion, tz = tz_string, offset_policy = offset_policy, offset_hours = tz_off, prepared_at = prepared_at)
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
        .map_get(mapping$col_prec) %||% "",
        .map_get(mapping$manual_lat) %||% "",
        .map_get(mapping$manual_lon) %||% "")
    })

    
    
    
    prep_ready <- shiny::eventReactive(
      list(
        raw_file(),                 # file
        tz$tz_ready(),              # gate: have a zone
        tz$tz_use(),                # re-fire when IANA zone changes
        tz$tz_offset_policy(),      # std/modal policy changes
        diurnal_method_reactive(),  # diurnal method changes
        mapping_fp(),               # column mapping fingerprint changes
        mapping$manual_lat(),       # <-- add numeric lat trigger
        mapping$manual_lon()        # <-- add numeric lon trigger
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
        
        list(
          src           = raw_file(),
          tz_string     = tz_use_val,
          offset_policy = offset_policy_val,
          diurnal       = dm
        )
      },
      ignoreInit = TRUE
    )
    
    
      
    
    #ForTEST----
    
    
    observeEvent(prep_ready(), ignoreInit = TRUE, {
      args <- prep_ready()
      message(sprintf("[Prepare][TRACE] prep_ready fired at %s (tz=%s, diurnal=%s)",
                      format(Sys.time(), "%H:%M:%S"), args$tz_string, args$diurnal))
    })
    
    observeEvent(debounced_ready(), ignoreInit = TRUE, {
      message(sprintf("[Prepare][TRACE] debounced_ready fired at %s", format(Sys.time(), "%H:%M:%S")))
    })
    
    
    #ENDFORTEST----
    
    
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
      src <- data.table::as.data.table(args$src)
      tz_string <- args$tz_string
      offset_policy <- args$offset_policy
      dia <- args$diurnal
      started_at <- Sys.time()
      t_total_start <- proc.time()
      logs <- character()
      n_rows_in <- nrow(src)
      station_count <- if ("id" %in% names(src)) length(unique(src$id)) else 1L

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
        out <- attach_provenance(out_dt, "hourly", "passthrough", tz_string, offset_policy, started_at, offset_hours = NA)
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
        ts_src <- as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00", as.integer(src[[cols$yr]]), as.integer(src[[cols$mon]]), as.integer(src[[cols$day]]), as.integer(gsub("[^0-9-]", "", as.character(src[[cols$hr]])))), tz = tz_string)
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
        choices <- c("stop" = "Stop and let me fix the source file", "fill_gaps" = "Fill only the missing hours (daily→hourly for gaps)")
        if (can_replace) choices <- c(choices, "replace_stream" = "Replace entire stream: use daily NOON (hr=12) and convert to hourly")
        shiny::showModal(shiny::modalDialog(
          title = sprintf("Hourly sequence has gaps — %d rows, %d distinct day(s) (%d station%s)", n_rows_in, distinct_days, station_count, if (station_count == 1) "" else "s"),
          shiny::div(shiny::tags$p(sprintf("Summary: %d of %d day(s) have gaps or <24 hours; duplicates on %d day(s).", days_with_gaps, distinct_days, sum(day_stats$dup_count > 0))), shiny::tags$details(shiny::tags$summary("Show affected days"), shiny::HTML({
            affected <- day_stats[gap_count > 0 | n_hours < 24][order(date)][1:(.N)]
            if (!nrow(affected)) "<em>No per-day issues detected (unexpected).</em>" else paste(sprintf("%s — %s: hours=%d, gaps=%d, dups=%d", affected$id, as.character(affected$date), affected$n_hours, affected$gap_count, affected$dup_count), collapse = "<br/>")
          }))),
          shiny::radioButtons(ns("gap_policy"), "Choose an action:", choices = choices, selected = "fill_gaps"),
          footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("apply_gap_policy"), "Apply")),
          easyClose = TRUE,
          size="l"
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
              out <- attach_provenance(out, "hourly", paste0("replace_stream(dailyNoon→hourly:", dia, ")"), tz_string, offset_policy, started_at)
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
            out <- attach_provenance(out, "hourly", paste0("gapFill(daily→hourly:", dia, ")"), tz_string, offset_policy, started_at)
            raw_like_rv(out)
            daily_src_rv(NULL)
            finished_at <- Sys.time()
            t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
            meta_rv(list(kind = "hourly", converted = TRUE, failed = FALSE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = c(logs, "[Prepare] Filled missing hours using daily→hourly."), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out), station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
            emit_toast(sprintf("Filled gaps. Output has %s hourly rows.", nrow(out)), "message", 5)
            return()
          },
          # once = TRUE,
          ignoreInit = TRUE
        )
        gap_observer_rv(gap_obs)
        return()
      }

      if (det$kind == "daily_one_hour") {
        emit_toast(sprintf("Detected DAILY (fixed-hour) input. Converting to HOURLY (%s)…", dia), "message", 5)
        logs <- c(logs, "[Prepare] Detected DAILY (one-hour-per-day) input.", if (isTRUE(det$seq_ok)) "[Prepare] Daily sequence looks continuous." else "[Prepare][WARN] Daily sequence has gaps.", "[Prepare] Converting to HOURLY …")
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
        out <- attach_provenance(out, "daily_one_hour", paste0("daily→hourly(", dia, ")"), tz_string, offset_policy, started_at, offset_hours = res$tz_off)
        raw_like_rv(out)
        finished_at <- Sys.time()
        t_total_ms <- as.numeric((proc.time() - t_total_start)[["elapsed"]]) * 1000
        meta_rv(list(kind = "daily_one_hour", converted = TRUE, failed = FALSE, tz = tz_string, offset_policy = offset_policy, diurnal = dia, log = c(logs, res$logs), t_detect_ms = t_detect_ms, t_convert_ms = t_convert_ms, t_total_ms = t_total_ms, n_rows_in = n_rows_in, n_rows_out = nrow(out), station_count = station_count, run_id = run_id, started_at = started_at, finished_at = finished_at))
        emit_toast(sprintf("Converted %s days to %s hourly rows.", nrow(daily), nrow(out)), "message", 5)
        return()
      }

      if (det$kind == "daily") {
        emit_toast(sprintf("Detected DAILY input. Converting to HOURLY (%s)…", dia), "message", 5)
        logs <- c(logs, "[Prepare] Detected DAILY input.", if (isTRUE(det$seq_ok)) "[Prepare] Daily sequence looks continuous." else "[Prepare][WARN] Daily sequence has gaps.", "[Prepare] Converting to HOURLY …")
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
        out <- attach_provenance(out, "daily", paste0("daily→hourly(", dia, ")"), tz_string, offset_policy, started_at, offset_hours = res$tz_off)
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
      prep_meta    = shiny::reactive(meta_rv())
    )
  })
}
