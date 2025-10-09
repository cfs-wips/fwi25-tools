# R/modules/mod_engine.R
mod_engine_server <- function(id, raw_file, mapping, tz, filt, init, tr, run_click){
  moduleServer(id, function(input, output, session){
    source("R/modules/mod_utils.R",local=TRUE)
    source("ng/util.r", local = TRUE)
    source("ng/make_inputs.r", local = TRUE)
    source("ng/NG_FWI.r", local = TRUE)
    
    has_explicit_zone <- function(x){
      if (is.null(x)) return(FALSE)
      x <- as.character(x)
      any(grepl("(Z$)|([\\+\\-]\\d{2}:?\\d{2}$)", x, perl = TRUE), na.rm = TRUE)
    }

    get_col <- function(df, nm){ if (isTruthy(nm) && nzchar(nm) && nm %in% names(df)) df[[nm]] else NULL }

    shaped_input <- reactive({
      validate(need(!is.null(raw_file()), "Upload a CSV first."))
      df <- tibble::as_tibble(raw_file())
      # print(head(df))
      needed <- c(mapping$col_temp(), mapping$col_rh(), mapping$col_ws(), mapping$col_rain())
      validate(need(all(nzchar(needed)), "Please map temperature, RH, wind, and rain columns."))
      must_be_numeric <- needed
      bad <- Filter(function(nm) !is.numeric(df[[nm]]), must_be_numeric)
      validate(need(length(bad) == 0, sprintf(tr("err_non_numeric_cols"), paste(bad, collapse=", "))))
      validate(
        need(is.finite(init$ffmc0()) && init$ffmc0() >= 0 && init$ffmc0() <= 101, tr("err_ffmc_range")),
        need(is.finite(init$dmc0()) && init$dmc0() >= 0, tr("err_dmc_range")),
        need(is.finite(init$dc0()) && init$dc0() >= 0, tr("err_dc_range")),
        need(is.finite(mapping$manual_lat()) && mapping$manual_lat() >= -90 && mapping$manual_lat() <= 90, "Latitude must be between -90 and 90."),
        need(is.finite(mapping$manual_lon()) && mapping$manual_lon() >= -180 && mapping$manual_lon() <= 180, "Longitude must be between -180 and 180.")
      )
      # print(init$ffmc0())
      tz_use <- tz$tz_use()
      validate(need(tz_use %in% OlsonNames(), tr("err_tz_invalid")))

      dt_col <- get_col(df, mapping$col_datetime())
      if (!is.null(dt_col)){
        if (has_explicit_zone(dt_col)){
          dt_utc <- lubridate::parse_date_time(dt_col,
            orders = c("Y-m-d H:M:S","Y-m-d H:M","Y/m/d H:M:S","Y/m/d H:M",
                       "d-m-Y H:M:S","d/m/Y H:M:S","m/d/Y H:M:S","m/d/Y H:M",
                       "Ymd HMS","Ymd HM","Ymd H"), tz = "UTC")
          validate(need(!all(is.na(dt_utc)), "Could not parse your Date–Time column (UTC/offset)."))
          dt_local <- lubridate::with_tz(dt_utc, tz = tz_use)
        } else {
          dt_local <- lubridate::parse_date_time(dt_col,
            orders = c("Y-m-d H:M:S","Y-m-d H:M","Y/m/d H:M:S","Y/m/d H:M",
                       "d-m-Y H:M:S","d/m/Y H:M:S","m/d/Y H:M:S","m/d/Y H:M",
                       "Ymd HMS","Ymd HM","Ymd H"), tz = tz_use)
          validate(need(!all(is.na(dt_local)), "Could not parse your Date–Time column."))
        }
      } else {
        y <- get_col(df, mapping$col_year())
        m <- get_col(df, mapping$col_month())
        d <- get_col(df, mapping$col_day())
        h <- get_col(df, mapping$col_hour())
        validate(need(all(!is.null(c(y,m,d,h))), "Provide Date–Time or Year/Month/Day/Hour."))
        dt_local <- lubridate::make_datetime(year = as.integer(y), month = as.integer(m), day = as.integer(d), hour = as.integer(h), tz = tz_use)
      }

      if (!is.null(filt$start_date()) && !is.na(filt$start_date())){
        keep <- as.Date(dt_local, tz = tz_use) >= as.Date(filt$start_date())
        validate(need(any(keep), "All rows were filtered out by the start date."))
        df <- df[keep, , drop = FALSE]
        dt_local <- dt_local[keep]
        validate(need(nrow(df) > 0, "No rows remain after filtering; check your date filter or input data."))
      }

      std_probe <- as.POSIXct("2025-01-15 12:00:00", tz = tz_use)
      std_z <- format(std_probe, "%z")
      std_h <- tz_standard_offset_hours(tz_use)
      mmodal <- tryCatch(tz_modal_offset_hours(dt_local), error = function(e) list(offset = NA_real_, z_mode = NA_character_))
      modal_h <- mmodal$offset
      z_mode  <- mmodal$z_mode
      offset_hours <- if (identical(tz$tz_offset_policy(), "std")) std_h else modal_h
      if (!isTRUE(all.equal(offset_hours, round(offset_hours)))){
        warning(sprintf("Time–zone offset has minutes (%.2f h). Rounding to nearest hour for make_inputs().", offset_hours))
        offset_hours <- round(offset_hours)
      }
      if (is.na(offset_hours) || abs(offset_hours) > 14) stop(sprintf("Computed GMT offset (%.2f) seems invalid.", offset_hours))

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
        lat  = rep(suppressWarnings(as.numeric(mapping$manual_lat())), nrow(df)),
        long = rep(suppressWarnings(as.numeric(mapping$manual_lon())), nrow(df)),
        tz   = tz_use,
        id   = as.character(get_col(df,mapping$col_id()))
      )
      validate(need(all(!is.na(wx$temp)), "Temperature has NA after parsing."))
      validate(need(all(!is.na(wx$rh)),   "RH has NA after parsing."))
      validate(need(all(!is.na(wx$ws)),   "Wind has NA after parsing."))
      validate(need(all(!is.na(wx$rain)), "Rain has NA after parsing."))

      list(inputs = wx, tz = tz_use, tz_offset = offset_hours, start_date = filt$start_date(), n_rows = nrow(wx), diag_std_z = std_z, diag_modal_z = z_mode)
    }) |>
      bindEvent(run_click(),
                mapping$manual_lat(),
                mapping$manual_lon(),
                tz$tz_offset_policy(),
                filt$start_date(),
                tz$tz_mode()
                )

    run_model <- reactive({
      req(shaped_input())
      si <- shaped_input(); inputs <- si$inputs
      validate(need(exists("hFWI"), "hFWI() not found after sourcing NG_FWI.r"))
      fml <- tryCatch(formals(hFWI), error = function(e) NULL)
      out <- NULL
      try({
        if (!is.null(fml)){
          argn <- names(fml)
          if (all(c("df_wx","timezone") %in% argn)){
            out <- hFWI(df_wx = inputs, timezone = si$tz_offset, ffmc_old = init$ffmc0(), dmc_old = init$dmc0(), dc_old = init$dc0())
          } else if ("inputs" %in% argn){
            out <- hFWI(inputs = inputs, ffmc0 = init$ffmc0(), dmc0 = init$dmc0(), dc0 = init$dc0())
          } else if ("df" %in% argn){
            out <- hFWI(df = inputs, ffmc0 = init$ffmc0(), dmc0 = init$dmc0(), dc0 = init$dc0())
          } else {
            if (length(argn) >= 5) out <- hFWI(inputs, si$tz_offset, init$ffmc0(), init$dmc0(), init$dc0()) else out <- hFWI(inputs, init$ffmc0(), init$dmc0(), init$dc0())
          }
        } else {
          out <- try(hFWI(df_wx = inputs, timezone = si$tz_offset, ffmc_old = init$ffmc0(), dmc_old = init$dmc0(), dc_old = init$dc0()), silent = TRUE)
          if (inherits(out, "try-error")) out <- hFWI(inputs = inputs, ffmc0 = init$ffmc0(), dmc0 = init$dmc0(), dc0 = init$dc0())
        }
      }, silent = TRUE)
      validate(need(!is.null(out), "hFWI() call failed; check the Log tab for details."))
      data.table::as.data.table(as.data.frame(out))
    })

    daily_fwi_df <- reactive({
      if (!isTRUE(init$calc_fwi87())) return(NULL)
      si <- shaped_input()
      req(si, si$inputs)
      tz_use <- if (is.null(si$tz) || !nzchar(si$tz)) "UTC" else si$tz
      wx <- data.table::as.data.table(as.data.frame(si$inputs))
      req("datetime" %in% names(wx) && inherits(wx$datetime, "POSIXt"))
      for (nm in c("temp","rh","ws")) if (!(nm %in% names(wx))) stop(sprintf("daily_fwi_df(): '%s' not found.", nm))
      pcol <- find_precip_col(names(wx)); if (is.null(pcol)) stop("daily_fwi_df(): couldn't find precipitation ('rain'/'precip'/'prec'/'prcp'/'rf').")
      use_std <- identical(tz$tz_offset_policy(), "std")
      if (use_std){
        cur_off_h <- parse_z_to_hours(format(wx$datetime, "%z"))
        std_off_h <- tz_standard_offset_hours(tz_use)
        delta_h <- cur_off_h - std_off_h
        wx[, datetime_LST := datetime - lubridate::dhours(delta_h)]
        wx[, dt_base := datetime_LST]
      } else {
        wx[, dt_base := datetime]
      }
      wx[, hour := lubridate::hour(dt_base)]
      wx[, date := as.Date(dt_base, tz = tz_use)]
      noon_tbl <- data.table::as.data.table(nearest_noon_per_day(as.data.frame(wx), dt_col = "dt_base", hour_col = "hour", tz = tz_use))
      noon_tbl[, date := as.Date(dt_base, tz = tz_use)]
      noon_tbl[, `:=`(
        start = as.POSIXct(paste0(format(date - 1L, "%Y-%m-%d"), " 13:00:00"), tz = tz_use),
        end   = as.POSIXct(paste0(format(date, "%Y-%m-%d"),   " 12:00:00"), tz = tz_use)
      )]
      if (use_std){
        end_delta_h <- parse_z_to_hours(format(noon_tbl$end, "%z")) - tz_standard_offset_hours(tz_use)
        noon_tbl[, `:=`(start = start - lubridate::dhours(end_delta_h), end = end - lubridate::dhours(end_delta_h))]
      }
      data.table::setkey(wx, dt_base)
      acc <- wx[
        noon_tbl,
        on = .(dt_base >= start, dt_base <= end),
        allow.cartesian = TRUE,
        .(prec_24 = sum(get(pcol), na.rm = TRUE), n_hours = sum(!is.na(get(pcol)))),
        by = .EACHI
      ]
      noon_tbl[, `:=`(prec_24 = acc$prec_24, n_hours = acc$n_hours)]
      if (!(pcol %in% names(noon_tbl))) noon_tbl[, (pcol) := NA_real_]
      lat_val <- if (is.finite(si$inputs$lat[1])) si$inputs$lat[1] else 55
      long_val <- if (is.finite(si$inputs$long[1])) si$inputs$long[1] else -120
      daily_in <- data.table::data.table(
        yr = lubridate::year(noon_tbl$dt_base),
        mon = lubridate::month(noon_tbl$dt_base),
        day = lubridate::day(noon_tbl$dt_base),
        temp = as.numeric(noon_tbl$temp),
        rh   = as.numeric(noon_tbl$rh),
        ws   = as.numeric(noon_tbl$ws),
        prec = as.numeric(noon_tbl$prec_24),
        lat = lat_val,
        long = long_val
      )
      
      out <- tryCatch({
        cffdrs::fwi(input = as.data.frame(daily_in), 
                    init = data.frame(ffmc = init$ffmc0(), dmc = init$dmc0(), dc = init$dc0(), lat = lat_val), 
                    batch = TRUE, out = "all", lat.adjust = TRUE, uppercase = FALSE)
      }, error = function(e){ message("cffdrs::fwi() failed: ", conditionMessage(e)); NULL })
      if (is.null(out)) return(NULL)
      df87 <- as.data.frame(out); names(df87) <- tolower(names(df87))
      if (!"datetime" %in% names(df87)){
        mon_col <- if ("mon" %in% names(df87)) "mon" else if ("month" %in% names(df87)) "month" else NULL
        if (!is.null(mon_col) && all(c("yr","day") %in% names(df87))){
          df87$datetime <- lubridate::make_datetime(year = as.integer(df87$yr), month = as.integer(df87[[mon_col]]), day = as.integer(df87$day), hour = 12L, tz = tz_use)
        }
      }
      if ("datetime" %in% names(df87) && !("date" %in% names(df87))){
        df87$date <- as.Date(df87$datetime, tz = tz_use)
      } else if (!("date" %in% names(df87)) && all(c("yr","day") %in% names(df87))){
        mon_col <- if ("mon" %in% names(df87)) "mon" else if ("month" %in% names(df87)) "month" else NULL
        if (!is.null(mon_col)){
          df87$date <- as.Date(sprintf("%04d-%02d-%02d", as.integer(df87$yr), as.integer(df87[[mon_col]]), as.integer(df87$day)))
        }
      }
      d87 <- data.table::as.data.table(df87)
      if ("date" %in% names(d87)) d87[, date := as.Date(date)]
      d87 <- d87[ noon_tbl[, .(date, precip_12to12 = prec_24, n_hours)], on = "date" ]
      ord <- try(order(d87$datetime), silent = TRUE); if (!inherits(ord, "try-error")) d87 <- d87[ord]
      as.data.frame(d87)
    }) |> 
      bindEvent(
        run_click(),
        init$ffmc0(),
        init$dmc0(),
        init$dc0(),
        mapping$manual_lat(),
        mapping$manual_lon(),
        tz$tz_offset_policy(),
        filt$start_date(),
        tz$tz_mode()
      )

    return(list(
      shaped_input = shaped_input,
      run_model = run_model,
      daily_fwi_df = daily_fwi_df
    ))
  })
}
