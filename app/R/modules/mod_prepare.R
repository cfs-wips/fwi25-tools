# app/R/modules/mod_prepare.R
mod_prepare_server <- function(
    id,
    raw_file,                 # reactive: uploaded df (up$raw_file)
    mapping,                  # mod_mapping_server outputs (your list of reactives)
    tz,                       # mod_timezone_server (tz$tz_use(), tz_standard_offset_hours())
    diurnal_method_reactive = reactive("BT-default"),
    skip_invalid = TRUE
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    `%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
    to_int <- function(x) suppressWarnings(as.integer(x))
    to_num <- function(x) suppressWarnings(as.numeric(x))
    
    # --- helpers -------------------------------------------------------------
    detect_resolution <- function(df){
      # Hourly?
      if (nzchar(mapping$col_datetime() %||% "")) {
        dt <- data.table::as.data.table(df)
        data.table::setnames(dt, mapping$col_datetime(), "timestamp", skip_absent = TRUE)
        colnames(dt) <- tolower(colnames(dt))
        ok <- FALSE; try(ok <- is_sequential_hours(dt), silent = TRUE)  # ng/util.r
        if (isTRUE(ok)) return(list(kind="hourly"))
      }
      # Daily?
      has_date <- nzchar(mapping$col_date() %||% "") ||
        (nzchar(mapping$col_year() %||% "") && nzchar(mapping$col_month() %||% "") && nzchar(mapping$col_day() %||% ""))
      if (has_date){
        dt <- data.table::as.data.table(df)
        if (nzchar(mapping$col_date() %||% "")) {
          data.table::setnames(dt, mapping$col_date(), "date", skip_absent = TRUE)
          dt[, date := as.Date(date)]
        } else {
          y <- mapping$col_year(); m <- mapping$col_month(); d <- mapping$col_day()
          dt[, date := as.Date(sprintf("%04d-%02d-%02d", get(y), get(m), get(d)))]
        }
        colnames(dt) <- tolower(colnames(dt))
        ok <- FALSE; try(ok <- is_sequential_days(dt), silent = TRUE)    # ng/util.r
        if (isTRUE(ok)) return(list(kind="daily"))
      }
      list(kind="unknown")
    }
    
    # Build lower-case noon daily frame for daily_to_minmax()
    build_noon_df <- function(df){
      dt <- data.table::as.data.table(df)
      
      if (nzchar(mapping$col_date() %||% "")) {
        data.table::setnames(dt, mapping$col_date(), "date", skip_absent = TRUE)
        dt[, date := as.Date(date)]
        dt[, `:=`(yr = to_int(format(date, "%Y")),
                  mon = to_int(format(date, "%m")),
                  day = to_int(format(date, "%d")))]
      } else {
        data.table::setnames(dt, mapping$col_year(), "yr", skip_absent = TRUE)
        data.table::setnames(dt, mapping$col_month(), "mon", skip_absent = TRUE)
        data.table::setnames(dt, mapping$col_day(), "day", skip_absent = TRUE)
      }
      
      data.table::setnames(dt, mapping$col_temp(), "temp", skip_absent = TRUE)
      data.table::setnames(dt, mapping$col_rh(),   "rh",   skip_absent = TRUE)
      data.table::setnames(dt, mapping$col_ws(),   "ws",   skip_absent = TRUE)
      data.table::setnames(dt, mapping$col_rain(), "prec", skip_absent = TRUE)
      
      need <- c("yr","mon","day","temp","rh","ws","prec")
      miss <- setdiff(need, names(dt))
      if (length(miss)) stop("Missing mapped columns for daily→minmax: ", paste(miss, collapse = ", "))
      
      if (nzchar(mapping$col_id() %||% "") && mapping$col_id() %in% names(dt)) {
        data.table::setnames(dt, mapping$col_id(), "id", skip_absent = TRUE)
      } else {
        dt[, id := "STN"]
      }
      
      # Your hourly converter requires LAT/LONG — use manual values
      lat <- to_num(mapping$manual_lat()); lon <- to_num(mapping$manual_lon())
      if (is.na(lat) || is.na(lon)) stop("Manual LAT/LON must be provided for daily→hourly conversion.")
      
      dt[, `:=`(lat = lat, long = lon)]
      dt[, .(id, lat, long, yr, mon, day, temp, rh, ws, prec)]
    }
    
    # daily → minmax → hourly (returns hourly lower-case columns + optional id)
    convert_daily_to_hourly <- function(df, tz_string, diurnal_method, skip_invalid){
      noon_all <- build_noon_df(df)
      
      ys <- noon_all[, .(ok = is_sequential(as.Date(sprintf("%04d-%02d-%02d", yr, mon, day), "days"))),
                     by = .(id, yr)]  # ng/util.r is_sequential
      warn <- ys[!ok]; good <- ys[ ok]
      
      logs <- character()
      if (nrow(warn)) {
        for (i in seq_len(nrow(warn))) {
          logs <- c(logs, sprintf("[Prepare][WARN] %s %d is not sequential daily; %s.",
                                  warn$id[i], warn$yr[i],
                                  if (isTRUE(skip_invalid)) "skipping" else "aborting"))
        }
        if (!isTRUE(skip_invalid)) stop("Non-sequential daily station-years present; aborting.")
      }
      
      # Single, stable UTC offset (hours) for converter — standard time
      tz_off <- tz_standard_offset_hours(tz_string)  # same helper used in mod_log.R [2](https://041gc-my.sharepoint.com/personal/justin_beckers_nrcan-rncan_gc_ca/Documents/Microsoft%20Copilot%20Chat%20Files/server.txt)
      
      out <- NULL
      for (i in seq_len(nrow(good))) {
        g <- good[i]
        sub <- noon_all[id == g$id & yr == g$yr]
        
        # daily → min/max (lower-case)                                 [3](https://041gc-my.sharepoint.com/personal/justin_beckers_nrcan-rncan_gc_ca/Documents/Microsoft%20Copilot%20Chat%20Files/make_hourly.txt)
        mm  <- daily_to_minmax(sub[, .(yr, mon, day, temp, rh, ws, prec)])
        
        # merge id/lat/long; promote to upper-case for minmax_to_hourly
        mm  <- merge(mm, unique(sub[, .(id, lat, long, yr)]), by = "yr")
        data.table::setnames(mm, names(mm), toupper(names(mm)))
        
        need <- c("LAT","LONG","YR","MON","DAY","TEMP_MIN","TEMP_MAX","RH_MIN","RH_MAX","WS_MIN","WS_MAX","PREC","ID")
        miss <- setdiff(need, names(mm))
        if (length(miss)) stop("minmax_to_hourly input missing: ", paste(miss, collapse = ", "))
        
        # minmax → hourly (Beck & Trevitt) — returns lower-case hourly      [1](https://041gc-my.sharepoint.com/personal/justin_beckers_nrcan-rncan_gc_ca/Documents/Microsoft%20Copilot%20Chat%20Files/mod_engine.txt)
        hr <- minmax_to_hourly(mm, timezone = tz_off, skip_invalid = TRUE, verbose = FALSE)
        
        # continuity check (temporary timestamp)
        hr_dt <- data.table::as.data.table(hr)
        hr_dt[, timestamp := as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00", yr, mon, day, hr), tz = tz_string)]
        colnames(hr_dt) <- tolower(colnames(hr_dt))
        ok <- FALSE; try(ok <- is_sequential_hours(hr_dt), silent = TRUE)
        if (!isTRUE(ok)) logs <- c(logs, sprintf("[Prepare][WARN] Post-conversion hourly continuity not perfect for %s %d.", g$id, g$yr))
        
        out <- data.table::rbindlist(list(out, hr), use.names = TRUE, fill = TRUE)
      }
      
      list(hourly = out, logs = logs, tz_off = tz_off)
    }
    
    # Recompose an engine-friendly "raw" df using the user's mapped names
    make_raw_like <- function(hr){
      # hr has: yr, mon, day, hr, temp, rh, ws, prec, (optional) id, lat, long
      dt <- data.table::as.data.table(hr)
      
      # 1) datetime *or* Y/M/D/H, using the *mapped* names
      if (nzchar(mapping$col_datetime() %||% "")) {
        nm <- mapping$col_datetime()
        dt[, (nm) := as.POSIXct(sprintf("%04d-%02d-%02d %02d:00:00", yr, mon, day, hr), tz = tz$tz_use())]
      } else {
        data.table::setnames(dt, "yr",  mapping$col_year(),  skip_absent = TRUE)
        data.table::setnames(dt, "mon", mapping$col_month(), skip_absent = TRUE)
        data.table::setnames(dt, "day", mapping$col_day(),   skip_absent = TRUE)
        data.table::setnames(dt, "hr",  mapping$col_hour(),  skip_absent = TRUE)
      }
      
      # 2) weather variables using mapped names
      data.table::setnames(dt, "temp", mapping$col_temp(),  skip_absent = TRUE)
      data.table::setnames(dt, "rh",   mapping$col_rh(),    skip_absent = TRUE)
      data.table::setnames(dt, "ws",   mapping$col_ws(),    skip_absent = TRUE)
      data.table::setnames(dt, "prec", mapping$col_rain(),  skip_absent = TRUE)
      
      # 3) carry id if mapped
      if (nzchar(mapping$col_id() %||% "") && "id" %in% names(dt)) {
        data.table::setnames(dt, "id", mapping$col_id(), skip_absent = TRUE)
      } else {
        # if engine expects id optional, we can drop it
        if ("id" %in% names(dt)) dt[, id := NULL]
      }
      
      # Optional: keep lat/long columns if the upload had them — engine uses manual lat/lon anyway
      as.data.frame(dt)
    }
    
    # --- reactives -----------------------------------------------------------
    raw_like_rv <- reactiveVal(NULL)
    meta_rv     <- reactiveVal(list(kind = NA, converted = FALSE, log = character()))
    
    observeEvent(list(raw_file(),
                      mapping$col_datetime(), mapping$col_date(),
                      mapping$col_year(), mapping$col_month(), mapping$col_day(), mapping$col_hour(),
                      mapping$col_temp(), mapping$col_rh(), mapping$col_ws(), mapping$col_rain(),
                      mapping$col_id(), mapping$manual_lat(), mapping$manual_lon(),
                      tz$tz_use(), diurnal_method_reactive()),
                 ignoreInit = TRUE, {
                   src <- raw_file(); req(src)
                   tz_string <- tz$tz_use() %||% "UTC"
                   dia <- diurnal_method_reactive() %||% "BT-default"
                   
                   logs <- character()
                   
                   det <- detect_resolution(src)
                   if (det$kind == "hourly") {
                     logs <- c(logs, "[Prepare] Detected HOURLY (sequential). No conversion performed.")
                     # Pass-through raw as-is
                     raw_like_rv(src)
                     meta_rv(list(kind = "hourly", converted = FALSE, tz = tz_string, log = logs))
                     return(invisible())
                   }
                   
                   if (det$kind == "daily") {
                     logs <- c(logs, "[Prepare] Detected DAILY (sequential). Converting to HOURLY …")
                     res <- try(convert_daily_to_hourly(src, tz_string, diurnal_method = dia, skip_invalid = skip_invalid), silent = TRUE)
                     if (inherits(res, "try-error") || is.null(res$hourly) || !nrow(res$hourly)) {
                       logs <- c(logs, paste0("[Prepare][ERROR] Daily→Hourly failed: ", as.character(res)))
                       raw_like_rv(NULL)
                       meta_rv(list(kind = "daily", converted = TRUE, failed = TRUE, tz = tz_string, log = logs))
                     } else {
                       hr <- res$hourly
                       ids <- if ("id" %in% names(hr)) length(unique(hr$id)) else 1L
                       rng <- sprintf("%04d-%02d-%02d %02d:00 → %04d-%02d-%02d %02d:00",
                                      min(hr$yr), min(hr$mon), min(hr$day), min(hr$hr),
                                      max(hr$yr), max(hr$mon), max(hr$day), max(hr$hr))
                       logs <- c(logs,
                                 sprintf("[Prepare] Conversion OK; TZ offset (standard): %d h.", res$tz_off),
                                 sprintf("[Prepare] Produced %d station(s), %d hourly rows, %s.",
                                         ids, nrow(hr), rng))
                       
                       # Recompose a raw-like frame matching the mapping names so the engine can shape it normally
                       raw_like <- make_raw_like(hr)
                       
                       # Provenance
                       attr(raw_like, "provenance") <- list(
                         source = "daily", conversion = paste0("daily→minmax→hourly(", dia, ")"),
                         tz = tz_string, offset_hours = res$tz_off, prepared_at = Sys.time()
                       )
                       attr(raw_like, "prep_log") <- logs
                       
                       raw_like_rv(raw_like)
                       meta_rv(list(kind = "daily", converted = TRUE, tz = tz_string, log = logs))
                     }
                   } else {
                     logs <- c(logs, "[Prepare][ERROR] Could not classify input as hourly or daily. Check mapping/data.")
                     raw_like_rv(NULL)
                     meta_rv(list(kind = "unknown", converted = NA, tz = tz_string, log = logs))
                   }
                 })
    
    list(
      raw_file  = reactive(raw_like_rv()),
      prep_meta = reactive(meta_rv())
    )
  })
}
