# R/modules/mod_engine.R
# -----------------------------------------------------------------------------
# Engine module for FWI25 tools (multi-station ready)
# - PREVIEW-friendly: shaped_input_core depends on mapping/tz -> plots can
#   populate Input variables pre-run.
# - Run-bound computations (run_model = FWI25 hourly; daily_fwi_df = FWI87 daily).
# - FWI87 computed PER STATION id (nearest-to-noon & precip window by id).
# - Caching and debounced triggers preserved.
# -----------------------------------------------------------------------------

mod_engine_server <- function(
    id, raw_file, mapping, tz, filt, init, tr, run_click,
    debounce_ms = 400,
    cache = "app",
    enable_cache = TRUE
){
  moduleServer(id, function(input, output, session){
    

    source("ng/util.r",        local = TRUE)
    source("ng/make_inputs.r", local = TRUE)
    source("ng/NG_FWI.r",      local = TRUE)
    
    # ----------------------------- Rounding policy -----------------------------
    digits_for_temp <- 2L
    digits_for_rh   <- 0L
    digits_for_ws   <- 1L
    digits_for_rain <- 1L
    digits_for_ffmc <- 1L
    digits_for_dmc  <- 1L
    digits_for_dc   <- 0L
    digits_for_isi  <- 1L
    digits_for_bui  <- 1L
    digits_for_fwi  <- 1L
    digits_for_dsr  <- 1L
    
    default_numeric_digits <- 2L
    never_round_names <- c("year","month","day","hour")
    
    # ----------------------------- Helpers ------------------------------------
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
    
    count_decimals <- function(x){
      if (is.null(x) || length(x) == 0) return(0L)
      s <- trimws(format(x, scientific = FALSE, trim = TRUE, digits = 22))
      m <- regexpr("(?<=\\.)\\d+", s, perl = TRUE)
      ifelse(m > 0, attr(m, "match.length"), 0L)
    }
    
    round_by_spec <- function(
    df,
    spec,
    default_digits = NULL,
    skip_classes = c("POSIXt", "Date", "difftime"),
    default_integers = FALSE,
    never_round = NULL
    ){
      if (is.null(df)) return(df)
      
      # explicit spec
      if (!is.null(spec) && length(spec)){
        for (nm in intersect(names(spec), names(df))){
          col <- df[[nm]]
          if (is.numeric(col) && !inherits(col, skip_classes)){
            df[[nm]] <- round(col, spec[[nm]])
          }
        }
      }
      
      # defaults for the rest
      if (length(default_digits)){
        others <- setdiff(names(df), names(spec %||% character()))
        if (!is.null(never_round) && length(never_round)){
          others <- setdiff(others, never_round)
        }
        for (nm in others){
          col <- df[[nm]]
          if (!is.numeric(col) || is.factor(col) || inherits(col, skip_classes)) next
          if (is.integer(col) && !default_integers) next
          df[[nm]] <- round(col, default_digits)
        }
      }
      df
    }
    
    # rx-safe column getter
    get_col <- function(df, nm){
      if (isTruthy(nm) && nzchar(nm) && nm %in% names(df)) df[[nm]] else NULL
    }
    
    # detect strings with explicit zone (Z or ±HH(:)MM)
    has_explicit_zone <- function(x){
      if (is.null(x)) return(FALSE)
      x <- as.character(x)
      any(grepl("Z$|[+-]\\d{2}:?\\d{2}$", x, perl = TRUE), na.rm = TRUE)
    }
    
    # tiny fingerprint for cache/telemetry
    fast_fingerprint <- function(wx){
      if (is.null(wx) || nrow(wx) == 0) return(list(n=0))
      rng <- range(wx$datetime)
      list(
        n = nrow(wx),
        t0 = as.numeric(rng[1]), t1 = as.numeric(rng[2]),
        sumT = round(sum(wx$temp,  na.rm = TRUE), 2),
        sumR = round(sum(wx$rain,  na.rm = TRUE), 3),
        lat  = suppressWarnings(as.numeric(wx$lat[1])),
        lon  = suppressWarnings(as.numeric(wx$long[1]))
      )
    }
    
    # ----------------------------- Telemetry store -----------------------------
    .metrics <- reactiveVal(list(
      last_event = NA_character_,
      t_shape_ms = NA_real_,
      t_model_ms = NA_real_,
      t_daily_ms = NA_real_,
      n_rows     = NA_integer_,
      key_shape  = NULL,
      key_model  = NULL,
      key_daily  = NULL,
      stamp      = Sys.time()
    ))
    set_metric <- function(upd) .metrics(modifyList(.metrics(), upd, keep.null = TRUE))
    
    # ----------------------------- Run triggers --------------------------------
    has_run <- reactiveVal(FALSE)
    trigger <- reactiveVal(0L)
    
    observeEvent(run_click(), {
      has_run(TRUE)
      set_metric(list(last_event = "run_click", stamp = Sys.time()))
    }, ignoreInit = TRUE, priority = 100)
    
    observeEvent(list(
      mapping$manual_lat(), mapping$manual_lon(),
      tz$tz_offset_policy(), filt$start_date(), tz$tz_mode(),
      init$ffmc0(), init$dmc0(), init$dc0(), init$calc_fwi87()
    ), {
      if (isTRUE(isolate(has_run()))){
        trigger(isolate(trigger()) + 1L)
        set_metric(list(last_event = "auto_change(debounced)", stamp = Sys.time()))
      }
    }, ignoreInit = TRUE)
    
    debounced_trigger <- shiny::debounce(reactive(trigger()), millis = debounce_ms)
    
    # ----------------------------- shaped_input (PREVIEW) ----------------------
    shaped_input_core <- reactive({
      t0 <- proc.time()[['elapsed']]
      on.exit({
        t1 <- proc.time()[['elapsed']]
        key <- try(if (exists("wx", inherits = FALSE) && !is.null(wx)) fast_fingerprint(wx) else NULL, silent = TRUE)
        set_metric(list(
          t_shape_ms = (t1 - t0) * 1000,
          n_rows     = if (exists("wx", inherits = FALSE)) nrow(wx) else NA_integer_,
          key_shape  = if (!inherits(key, "try-error")) key else NULL
        ))
      }, add = TRUE)
      
      validate(need(!is.null(raw_file()), 'Upload a CSV first.'))
      df <- tibble::as_tibble(raw_file())
      
      # required numeric mappings
      needed <- c(mapping$col_temp(), mapping$col_rh(), mapping$col_ws(), mapping$col_rain())
      validate(need(all(nzchar(needed)), 'Please map temperature, RH, wind, and rain columns.'))
      bad <- Filter(function(nm) !is.numeric(df[[nm]]), needed)
      validate(need(length(bad) == 0, sprintf(tr('err_non_numeric_cols'), paste(bad, collapse = ', '))))
      
      # initial conditions & location validations
      validate(
        need(is.finite(init$ffmc0()) && data.table::between(init$ffmc0(), 0, 101), tr('err_ffmc_range')),
        need(is.finite(init$dmc0()) && init$dmc0() >= 0,                           tr('err_dmc_range')),
        need(is.finite(init$dc0())  && init$dc0()  >= 0,                           tr('err_dc_range')),
        need(is.finite(mapping$manual_lat()) && data.table::between(mapping$manual_lat(), -90, 90), 'Latitude must be between -90 and 90.'),
        need(is.finite(mapping$manual_lon()) && data.table::between(mapping$manual_lon(), -180, 180),'Longitude must be between -180 and 180.')
      )
      
      tz_use <- tz$tz_use()
      validate(need(tz_use %in% OlsonNames(), tr('err_tz_invalid')))
      
      # ----- build datetime from any mapping path
      dt_col   <- get_col(df, mapping$col_datetime())
      date_col <- get_col(df, mapping$col_date())
      time_col <- get_col(df, mapping$col_time())
      
      make_combined_dt <- function(date_col, time_col, tz_use){
        if (inherits(date_col, "Date")) {
          date_str <- format(date_col, "%Y-%m-%d")
        } else {
          date_str <- as.character(date_col)
        }
        if (is.numeric(time_col)){
          hr <- floor(time_col)
          mn <- floor((time_col - hr) * 60)
          sc <- round((time_col - hr - mn/60) * 3600)
          time_str <- sprintf("%02d:%02d:%02d",
                              pmax(0, pmin(23, hr)),
                              pmax(0, pmin(59, mn)),
                              pmax(0, pmin(59, sc)))
        } else {
          time_str <- as.character(time_col)
        }
        combined <- paste(date_str, time_str)
        if (has_explicit_zone(combined)){
          dt_utc <- suppressWarnings(lubridate::parse_date_time(
            combined,
            orders = c('Y-m-d H:M:S','Y-m-d H:M','Y/m/d H:M:S','Y/m/d H:M',
                       'd-m-Y H:M:S','d/m/Y H:M:S','m/d/Y H:M','m/d/Y H:M:S',
                       'Ymd HMS','Ymd HM','Ymd H',
                       'Y-m-d I:M p','m/d/Y I:M p','d/m/Y I:M p'),
            tz = 'UTC'
          ))
          if (all(is.na(dt_utc))) return(NULL)
          lubridate::with_tz(dt_utc, tz = tz_use)
        } else {
          dt_local <- suppressWarnings(lubridate::parse_date_time(
            combined,
            orders = c('Y-m-d H:M:S','Y-m-d H:M','Y/m/d H:M:S','Y/m/d H:M',
                       'd-m-Y H:M:S','d/m/Y H:M:S','m/d/Y H:M:S','m/d/Y H:M',
                       'Ymd HMS','Ymd HM','Ymd H',
                       'Y-m-d I:M p','m/d/Y I:M p','d/m/Y I:M p'),
            tz = tz_use
          ))
          if (all(is.na(dt_local))) return(NULL)
          dt_local
        }
      }
      
      if (!is.null(dt_col)){
        if (has_explicit_zone(dt_col)){
          dt_utc <- lubridate::parse_date_time(
            dt_col,
            orders = c('Y-m-d H:M:S','Y-m-d H:M','Y/m/d H:M:S','Y/m/d H:M',
                       'd-m-Y H:M:S','d/m/Y H:M:S','m/d/Y H:M:S','m/d/Y H:M',
                       'Ymd HMS','Ymd HM','Ymd H'),
            tz = 'UTC'
          )
          validate(need(!all(is.na(dt_utc)), 'Could not parse your Date–Time column (UTC/offset).'))
          dt_local <- lubridate::with_tz(dt_utc, tz = tz_use)
        } else {
          dt_local <- lubridate::parse_date_time(
            dt_col,
            orders = c('Y-m-d H:M:S','Y-m-d H:M','Y/m/d H:M:S','Y/m/d H:M',
                       'd-m-Y H:M:S','d/m/Y H:M:S','m/d/Y H:M:S','m/d/Y H:M',
                       'Ymd HMS','Ymd HM','Ymd H'),
            tz = tz_use
          )
          validate(need(!all(is.na(dt_local)), 'Could not parse your Date–Time column.'))
        }
      } else if (!is.null(date_col) && !is.null(time_col)){
        dt_local <- make_combined_dt(date_col, time_col, tz_use)
        validate(need(!is.null(dt_local) && !all(is.na(dt_local)),
                      'Could not parse your Date + Time columns.'))
      } else {
        y <- get_col(df, mapping$col_year())
        m <- get_col(df, mapping$col_month())
        d <- get_col(df, mapping$col_day())
        h <- get_col(df, mapping$col_hour())
        validate(need(all(!is.null(c(y,m,d,h))),
                      'Provide Date–Time or Date + Time or Year/Month/Day/Hour.'))
        dt_local <- lubridate::make_datetime(
          year  = as.integer(y),
          month = as.integer(m),
          day   = as.integer(d),
          hour  = as.integer(h),
          tz    = tz_use
        )
      }
      
      # Optional start-date filter
      if (!is.null(filt$start_date()) && !is.na(filt$start_date())){
        keep <- as.Date(dt_local, tz = tz_use) >= as.Date(filt$start_date())
        validate(need(any(keep), 'All rows were filtered out by the start date.'))
        df <- df[keep, , drop = FALSE]
        dt_local <- dt_local[keep]
        validate(need(nrow(df) > 0, 'No rows remain after filtering; check your date filter or input data.'))
      }
      
      # Time-zone offset computations
      std_probe <- as.POSIXct('2025-01-15 12:00:00', tz = tz_use)
      std_z     <- format(std_probe, '%z')
      std_h     <- tz_standard_offset_hours(tz_use)
      mmodal    <- tryCatch(tz_modal_offset_hours(dt_local),
                            error = function(e) list(offset = NA_real_, z_mode = NA_character_))
      modal_h   <- mmodal$offset
      z_mode    <- mmodal$z_mode
      offset_hours <- if (identical(tz$tz_offset_policy(), 'std')) std_h else modal_h
      if (!isTRUE(all.equal(offset_hours, round(offset_hours)))){
        warning(sprintf('Time–zone offset has minutes (%.2f h). Rounding to nearest hour for make_inputs().', offset_hours))
        offset_hours <- round(offset_hours)
      }
      if (is.na(offset_hours) || abs(offset_hours) > 14)
        stop(sprintf('Computed GMT offset (%.2f) seems invalid.', offset_hours))
      
      # Optional solar radiation
      sol_col <- get_col(df, mapping$col_solrad())
      if (!is.null(sol_col)){
        validate(need(is.numeric(sol_col), "Solar radiation must be numeric if provided."))
      }
      
      # Build shaped inputs tibble (carry id if mapped)
      lat_val  <- suppressWarnings(as.numeric(mapping$manual_lat()))
      long_val <- suppressWarnings(as.numeric(mapping$manual_lon()))
      id_col   <- get_col(df, mapping$col_id())
      
      if (!is.null(sol_col)){
        wx <- tibble::tibble(
          datetime = dt_local,
          year = lubridate::year(dt_local),
          month = lubridate::month(dt_local),
          day = lubridate::day(dt_local),
          hour = lubridate::hour(dt_local),
          temp = as.numeric(get_col(df, mapping$col_temp())),
          rh   = as.numeric(get_col(df, mapping$col_rh())),
          ws   = as.numeric(get_col(df, mapping$col_ws())),
          rain = as.numeric(get_col(df, mapping$col_rain())),
          solrad = as.numeric(sol_col),
          lat  = rep(lat_val,  nrow(df)),
          long = rep(long_val, nrow(df)),
          tz   = tz_use,
          id   = as.character(id_col)
        )
      } else {
        wx <- tibble::tibble(
          datetime = dt_local,
          year = lubridate::year(dt_local),
          month = lubridate::month(dt_local),
          day = lubridate::day(dt_local),
          hour = lubridate::hour(dt_local),
          temp = as.numeric(get_col(df, mapping$col_temp())),
          rh   = as.numeric(get_col(df, mapping$col_rh())),
          ws   = as.numeric(get_col(df, mapping$col_ws())),
          rain = as.numeric(get_col(df, mapping$col_rain())),
          lat  = rep(lat_val,  nrow(df)),
          long = rep(long_val, nrow(df)),
          tz   = tz_use,
          id   = as.character(id_col)
        )
      }
      
      if (nrow(wx) >= 2 && wx$datetime[1] > wx$datetime[2]) wx <- wx[order(wx$datetime), ]
      
      validate(need(all(!is.na(wx$temp)), 'Temperature has NA after parsing.'))
      validate(need(all(!is.na(wx$rh)),   'RH has NA after parsing.'))
      validate(need(all(!is.na(wx$ws)),   'Wind has NA after parsing.'))
      validate(need(all(!is.na(wx$rain)), 'Rain has NA after parsing.'))
      # solrad optional
      
      list(
        inputs      = wx,
        tz          = tz_use,
        tz_offset   = offset_hours,
        start_date  = filt$start_date(),
        n_rows      = nrow(wx),
        diag_std_z  = std_z,
        diag_modal_z= z_mode
      )
    })
    
    # Cache PREVIEW
    shaped_input_tmp <- shaped_input_core
    if (isTRUE(enable_cache)){
      shaped_input_tmp <- shiny::bindCache(
        shaped_input_tmp,
        reactive({
          rf <- raw_file()
          list(
            rf = if (is.null(rf)) NULL else list(rows = nrow(rf), cols = ncol(rf)),
            map = list(
              dt   = mapping$col_datetime(), yr = mapping$col_year(),  mon = mapping$col_month(),
              day  = mapping$col_day(),      hr = mapping$col_hour(),
              date = mapping$col_date(),     time = mapping$col_time(), sol = mapping$col_solrad(),
              t    = mapping$col_temp(),     rh  = mapping$col_rh(),    ws  = mapping$col_ws(),
              rain = mapping$col_rain(),     id  = mapping$col_id(),
              lat  = mapping$manual_lat(),   lon = mapping$manual_lon()
            ),
            tz   = list(use = tz$tz_use(), policy = tz$tz_offset_policy(), mode = tz$tz_mode()),
            start= filt$start_date()
          )
        }),
        cache = cache
      )
    }
    
    # Run-bound (fires on Run and on debounced trigger)
    shaped_input <- shiny::bindEvent(shaped_input_tmp, run_click(), debounced_trigger(), ignoreInit = TRUE)
    
    # Expose PREVIEW for plots (inputs before Run)
    shaped_input_preview <- shaped_input_core
    
    # ----------------------------- run_model (FWI25 hourly) --------------------
    run_model_core <- reactive({
      t0 <- proc.time()[['elapsed']]
      on.exit({
        t1 <- proc.time()[['elapsed']]
        si <- isolate(shaped_input())
        set_metric(list(
          t_model_ms = (t1 - t0) * 1000,
          key_model  = list(
            si   = if (!is.null(si)) fast_fingerprint(si$inputs) else NULL,
            ffmc = init$ffmc0(), dmc = init$dmc0(), dc = init$dc0()
          )
        ))
      }, add = TRUE)
      
      si <- shaped_input(); req(si); req(nrow(si$inputs) > 0)
      inputs <- si$inputs
      
      validate(need(exists('hFWI'), 'hFWI() not found after sourcing NG_FWI.r'))
      fml <- tryCatch(formals(hFWI), error = function(e) NULL)
      out <- NULL
      
      try({
        if (!is.null(fml)){
          argn <- names(fml)
          if (all(c('df_wx','timezone') %in% argn)){
            out <- hFWI(df_wx = inputs, timezone = si$tz_offset,
                        ffmc_old = init$ffmc0(), dmc_old = init$dmc0(), dc_old = init$dc0())
          } else if ('inputs' %in% argn) {
            out <- hFWI(inputs = inputs, ffmc0 = init$ffmc0(), dmc0 = init$dmc0(), dc0 = init$dc0())
          } else if ('df' %in% argn) {
            out <- hFWI(df = inputs, ffmc0 = init$ffmc0(), dmc0 = init$dmc0(), dc0 = init$dc0())
          } else {
            if (length(argn) >= 5)
              out <- hFWI(inputs, si$tz_offset, init$ffmc0(), init$dmc0(), init$dc0())
            else
              out <- hFWI(inputs, init$ffmc0(), init$dmc0(), init$dc0())
          }
        } else {
          out <- try(hFWI(df_wx = inputs, timezone = si$tz_offset,
                          ffmc_old = init$ffmc0(), dmc_old = init$dmc0(), dc_old = init$dc0()),
                     silent = TRUE)
          if (inherits(out, 'try-error'))
            out <- hFWI(inputs = inputs, ffmc0 = init$ffmc0(), dmc0 = init$dmc0(), dc0 = init$dc0())
        }
      }, silent = TRUE)
      
      validate(need(!is.null(out), 'hFWI() call failed; check the Log tab for details.'))
      
      dt <- data.table::as.data.table(as.data.frame(out))
      print(head(out))
      # Rounding spec for hourly model output
      lat_digits  <- count_decimals(mapping$manual_lat())
      long_digits <- count_decimals(mapping$manual_lon())
      if (!is.finite(lat_digits)  || lat_digits  < 0) lat_digits  <- 4L
      if (!is.finite(long_digits) || long_digits < 0) long_digits <- 4L
      
      spec_model <- c(
        temp = digits_for_temp,
        rh   = digits_for_rh,
        ws   = digits_for_ws,
        rain = digits_for_rain,
        lat  = lat_digits,
        long = long_digits,
        ffmc = digits_for_ffmc,
        dmc  = digits_for_dmc,
        dc   = digits_for_dc,
        isi  = digits_for_isi,
        bui  = digits_for_bui,
        fwi  = digits_for_fwi,
        dsr  = digits_for_dsr
      )
      
      dt <- data.table::as.data.table(
        round_by_spec(
          as.data.frame(dt), spec_model,
          default_digits  = default_numeric_digits,
          default_integers = FALSE,
          never_round     = never_round_names
        )
      )
      dt
    })
    
    run_model_tmp <- run_model_core
    if (isTRUE(enable_cache)){
      run_model_tmp <- shiny::bindCache(
        run_model_tmp,
        reactive({
          si <- shaped_input()
          list(
            si     = if (!is.null(si)) fast_fingerprint(si$inputs) else NULL,
            tz_off = if (!is.null(si)) si$tz_offset else NULL,
            ffmc   = init$ffmc0(), dmc = init$dmc0(), dc = init$dc0()
          )
        }),
        cache = cache
      )
    }
    run_model <- shiny::bindEvent(run_model_tmp, run_click(), debounced_trigger(), ignoreInit = TRUE)
    
    # ----------------------------- daily_fwi_df (FWI87 daily; per station) ----
    daily_fwi_core <- reactive({
      t0 <- proc.time()[['elapsed']]
      on.exit({
        t1 <- proc.time()[['elapsed']]
        si <- isolate(shaped_input())
        set_metric(list(
          t_daily_ms = (t1 - t0) * 1000,
          key_daily  = list(
            si    = if (!is.null(si)) fast_fingerprint(si$inputs) else NULL,
            ffmc  = init$ffmc0(), dmc = init$dmc0(), dc = init$dc0(),
            policy = tz$tz_offset_policy(), do87 = init$calc_fwi87()
          )
        ))
      }, add = TRUE)
      
      if (!isTRUE(init$calc_fwi87())) return(NULL)
      
      si <- shaped_input(); req(si, si$inputs); req(nrow(si$inputs) > 0)
      
      tz_use <- if (is.null(si$tz) || !nzchar(si$tz)) 'UTC' else si$tz
      wx <- data.table::as.data.table(as.data.frame(si$inputs))
      req('datetime' %in% names(wx) && inherits(wx$datetime, 'POSIXt'))
      for (nm in c('temp','rh','ws'))
        if (!(nm %in% names(wx))) stop(sprintf("daily_fwi_df(): '%s' not found.", nm))
      
      pcol <- find_precip_col(names(wx))
      if (is.null(pcol)) stop("daily_fwi_df(): couldn't find precipitation ('rain'/'precip'/'prec'/'prcp'/'rf').")
      
      # Local base time columns (preserve id)
      use_std <- identical(tz$tz_offset_policy(), 'std')
      if (use_std){
        cur_off_h <- parse_z_to_hours(format(wx$datetime, '%z'))
        std_off_h <- tz_standard_offset_hours(tz_use)
        delta_h   <- cur_off_h - std_off_h
        wx[, datetime_LST := datetime - lubridate::dhours(delta_h)]
        wx[, dt_base := datetime_LST]
      } else {
        wx[, dt_base := datetime]
      }
      wx[, hour := lubridate::hour(dt_base)]
      wx[, date := as.Date(dt_base, tz = tz_use)]
      
      # ONE closest-to-noon per (id, date) using your id-aware utility
      noon_tbl <- data.table::as.data.table(
        nearest_noon_per_day(
          as.data.frame(wx),
          dt_col   = 'dt_base',
          hour_col = 'hour',
          tz       = tz_use,
          id_col   = 'id'   # <<< key: respect station id
        )
      )
      noon_tbl[, date := as.Date(dt_base, tz = tz_use)]
      
      # Daily window 13:00(prev) -> 12:00(current) per (id, date)
      noon_tbl[, `:=`(
        start = as.POSIXct(paste0(format(date - 1L, '%Y-%m-%d'), ' 13:00:00'), tz = tz_use),
        end   = as.POSIXct(paste0(format(date,     '%Y-%m-%d'), ' 12:00:00'), tz = tz_use)
      )]
      if (use_std){
        end_delta_h <- parse_z_to_hours(format(noon_tbl$end, '%z')) - tz_standard_offset_hours(tz_use)
        noon_tbl[, `:=`(
          start = start - lubridate::dhours(end_delta_h),
          end   = end   - lubridate::dhours(end_delta_h)
        )]
      }
      
      # Sum precipitation per (id, window) with non-equi join keyed by id
      data.table::setkey(wx, id, dt_base)
      acc <- wx[
        noon_tbl,
        on = .(id, dt_base >= start, dt_base <= end),
        allow.cartesian = TRUE,
        .(prec_24 = sum(get(pcol), na.rm = TRUE),
          n_hours = sum(!is.na(get(pcol)))),
        by = .EACHI
      ]
      noon_tbl[, `:=`(prec_24 = acc$prec_24, n_hours = acc$n_hours)]
      if (!(pcol %in% names(noon_tbl))) noon_tbl[, (pcol) := NA_real_]
      
      # Per-row lat/long from the noon record; fall back to global if missing
      lat_fallback  <- if (is.finite(si$inputs$lat[1]))  si$inputs$lat[1]  else 55
      long_fallback <- if (is.finite(si$inputs$long[1])) si$inputs$long[1] else -120
      lat_vec  <- if ("lat"  %in% names(noon_tbl)) noon_tbl$lat  else lat_fallback
      long_vec <- if ("long" %in% names(noon_tbl)) noon_tbl$long else long_fallback
      
      # Build cffdrs::fwi input; include id (string)
      daily_in <- data.table::data.table(
        yr   = lubridate::year(noon_tbl$dt_base),
        mon  = lubridate::month(noon_tbl$dt_base),
        day  = lubridate::day(noon_tbl$dt_base),
        temp = as.numeric(noon_tbl$temp),
        rh   = as.numeric(noon_tbl$rh),
        ws   = as.numeric(noon_tbl$ws),
        prec = as.numeric(noon_tbl$prec_24),
        lat  = as.numeric(lat_vec),
        long = as.numeric(long_vec),
        id   = as.character(noon_tbl$id)
      )
      
      out <- tryCatch({
        cffdrs::fwi(
          input = as.data.frame(daily_in),
          init  = data.frame(ffmc = init$ffmc0(), dmc = init$dmc0(), dc = init$dc0(), lat = lat_fallback),
          batch = TRUE, out = 'all', lat.adjust = TRUE, uppercase = FALSE
        )
      }, error = function(e){
        message('cffdrs::fwi() failed: ', conditionMessage(e)); NULL
      })
      if (is.null(out)) return(NULL)
      
      df87 <- as.data.frame(out)
      names(df87) <- tolower(names(df87))
      
      # Ensure datetime/date exist (noon-based)
      if (!('datetime' %in% names(df87))){
        mon_col <- if ('mon' %in% names(df87)) 'mon' else if ('month' %in% names(df87)) 'month' else NULL
        if (!is.null(mon_col) && all(c('yr','day') %in% names(df87))){
          df87$datetime <- lubridate::make_datetime(
            year  = as.integer(df87$yr),
            month = as.integer(df87[[mon_col]]),
            day   = as.integer(df87$day),
            hour  = 12L,
            tz    = tz_use
          )
        }
      }
      if ('datetime' %in% names(df87) && !('date' %in% names(df87))){
        df87$date <- as.Date(df87$datetime, tz = tz_use)
      } else if (!('date' %in% names(df87)) && all(c('yr','day') %in% names(df87))){
        mon_col <- if ('mon' %in% names(df87)) 'mon' else if ('month' %in% names(df87)) 'month' else NULL
        if (!is.null(mon_col)){
          df87$date <- as.Date(sprintf('%04d-%02d-%02d',
                                       as.integer(df87$yr),
                                       as.integer(df87[[mon_col]]),
                                       as.integer(df87$day)))
        }
      }
      
      d87 <- data.table::as.data.table(df87)
      if ('date' %in% names(d87)) d87[, date := as.Date(date)]
      
      # Attach 12->12 precip info back by (id, date)
      add_cols <- data.table::as.data.table(noon_tbl[, .(id = as.character(id), date, precip_12to12 = prec_24, n_hours)])
      data.table::setkey(add_cols, id, date)
      if (!('id' %in% names(d87))) {
        # If cffdrs::fwi didn't echo id, try safe recovery by date (only if unambiguous)
        map <- unique(add_cols[, .(date, id)])
        ambig <- map[, .N, by = date][N > 1L, unique(date)]
        if (length(ambig) == 0L && 'date' %in% names(d87)) {
          d87 <- merge(d87, map, by = 'date', all.x = TRUE, sort = FALSE)
        }
      }
      data.table::setkey(d87, id, date)
      d87 <- d87[add_cols, on = .(id, date)]
      
      ord <- try(order(d87$datetime), silent = TRUE)
      if (!inherits(ord, 'try-error')) d87 <- d87[ord]
      
      # Rounding for daily output
      lat_digits  <- count_decimals(mapping$manual_lat())
      long_digits <- count_decimals(mapping$manual_lon())
      if (!is.finite(lat_digits)  || lat_digits  < 0) lat_digits  <- 4L
      if (!is.finite(long_digits) || long_digits < 0) long_digits <- 4L
      
      spec_daily <- c(
        temp = digits_for_temp,
        rh   = digits_for_rh,
        ws   = digits_for_ws,
        prec = digits_for_rain,
        precip_12to12 = digits_for_rain,
        lat  = lat_digits,
        long = long_digits,
        ffmc = digits_for_ffmc,
        dmc  = digits_for_dmc,
        dc   = digits_for_dc,
        isi  = digits_for_isi,
        bui  = digits_for_bui,
        fwi  = digits_for_fwi,
        dsr  = digits_for_dsr
      )
      
      d87 <- data.table::as.data.table(
        round_by_spec(
          as.data.frame(d87), spec_daily,
          default_digits  = default_numeric_digits,
          default_integers = FALSE,
          never_round     = never_round_names
        )
      )
      as.data.frame(d87)
    })
    
    daily_fwi_tmp <- daily_fwi_core
    if (isTRUE(enable_cache)){
      daily_fwi_tmp <- shiny::bindCache(
        daily_fwi_tmp,
        reactive({
          si <- shaped_input()
          list(
            si     = if (!is.null(si)) fast_fingerprint(si$inputs) else NULL,
            ffmc   = init$ffmc0(), dmc = init$dmc0(), dc = init$dc0(),
            policy = tz$tz_offset_policy(), do87 = init$calc_fwi87()
          )
        }),
        cache = cache
      )
    }
    daily_fwi_df <- shiny::bindEvent(daily_fwi_tmp, run_click(), debounced_trigger(), ignoreInit = TRUE)
    
    # ----------------------------- Expose API ----------------------------------
    return(list(
      shaped_input         = shaped_input,          # run-bound (Log tab)
      shaped_input_preview = shaped_input_preview,  # preview (Plot inputs)
      run_model            = run_model,             # hourly FWI25
      daily_fwi_df         = daily_fwi_df,          # daily FWI87 (per station)
      metrics              = reactive(.metrics())
    ))
  })
}