# R/modules/mod_engine.R
# Engine module: FWI25 (hourly) + FWI87 (daily) with dual-source support.
# Full parity rewrite with optimizations and telemetry fix.
#
# Inputs:
#  - raw_hourly: reactive hourly dataset from mod_prepare (passthrough or converted)
#  - daily_src : reactive original daily dataset from mod_prepare (NULL if upload was hourly)
#  - mapping, tz, filt, init, tr : submodules with getters
#  - run_click  : reactive that fires when Run is pressed
#
# Outputs:
#  - shaped_input           : run-bound shaped hourly inputs + tz diagnostics
#  - shaped_input_preview   : lightweight preview (rows/cols/tz/start)
#  - run_model              : hourly FWI25 output (data.table/data.frame)
#  - daily_fwi_df           : daily FWI87 output (data.frame with id/date/precip_12to12)
#  - metrics                : reactive list of telemetry
#  - reset                  : reactive integer that increments on new upload (UI reset signal)

mod_engine_server <- function(
  id,
  raw_hourly,
  daily_src = reactive(NULL),
  mapping, tz, filt, init, tr, run_click,
  cache = "app",
  enable_cache = TRUE
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Dependencies ----
    # source("ng/util.r", local = TRUE)
    # source("ng/NG_FWI.r", local = TRUE)
    # # source("ng/make_inputs.r", local = TRUE)
    source("ng/util_vectorized.r", local = TRUE)
    source("ng/NG_FWI_vectorized.r", local = TRUE)

    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

    # ---- Rounding policy ----
    digits_for_temp <- 2L
    digits_for_rh <- 0L
    digits_for_ws <- 1L
    digits_for_prec <- 1L
    digits_for_ffmc <- 1L
    digits_for_dmc <- 1L
    digits_for_dc <- 0L
    digits_for_isi <- 1L
    digits_for_bui <- 1L
    digits_for_fwi <- 1L
    digits_for_dsr <- 1L
    default_numeric_digits <- 2L
    never_round_names <- c("year", "month", "day", "hour", "yr", "mon", "hr", "datetime", "timestamp", "date")

    # ---- Memoized helpers ----
    memo_count_decimals <- memoise::memoise(function(x) {
      if (is.null(x) || length(x) == 0) {
        return(0L)
      }
      s <- trimws(format(x, scientific = FALSE, trim = TRUE, digits = 22))
      m <- regexpr("(?<=\\.)\\d+", s, perl = TRUE)
      ifelse(m > 0, attr(m, "match.length"), 0L)
    })

    memo_round_vec <- memoise::memoise(function(x, digits) {
      if (!is.numeric(x)) {
        return(x)
      }
      round(x, digits)
    })

    round_by_spec_dt <- function(dt, spec,
                                 default_digits = NULL,
                                 skip_classes = c("POSIXt", "Date", "difftime"),
                                 default_integers = FALSE,
                                 never_round = NULL) {
      if (!data.table::is.data.table(dt)) data.table::setDT(dt)
      # explicit spec
      if (!is.null(spec) && length(spec)) {
        for (nm in intersect(names(spec), names(dt))) {
          col <- dt[[nm]]
          if (is.numeric(col) && !inherits(col, skip_classes)) {
            dt[[nm]] <- memo_round_vec(col, spec[[nm]])
          }
        }
      }
      # defaults for the rest
      if (length(default_digits)) {
        others <- setdiff(names(dt), names(spec %||% character()))
        if (!is.null(never_round) && length(never_round)) {
          others <- setdiff(others, never_round)
        }
        for (nm in others) {
          col <- dt[[nm]]
          if (!is.numeric(col) || is.factor(col) || inherits(col, skip_classes)) next
          if (is.integer(col) && !default_integers) next
          dt[[nm]] <- memo_round_vec(col, default_digits)
        }
      }
      dt
    }

    # ---- Column helpers ----
    get_col <- function(df, nm) if (shiny::isTruthy(nm) && nzchar(nm) && nm %in% names(df)) df[[nm]] else NULL

    get_first_col <- function(df, candidates) {
      for (nm in candidates) {
        if (!is.null(nm) && nzchar(nm) && nm %in% names(df)) {
          return(df[[nm]])
        }
      }
      NULL
    }

    has_explicit_zone <- function(x) {
      if (is.null(x)) {
        return(FALSE)
      }
      x <- as.character(x)
      any(grepl("Z$|[\\+\\-]\\d{2}:?\\d{2}$", x, perl = TRUE), na.rm = TRUE)
    }

    # ---- Parsing helpers (prefer base R; fallback to lubridate if present) ----
    parse_dt_base <- memoise::memoise(function(x, tz_out) {
      if (is.null(x)) {
        return(rep(as.POSIXct(NA), length(x)))
      }
      x <- as.character(x)
      suppressWarnings(as.POSIXct(x, tz = tz_out, tryFormats = c(
        "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
        "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
        "%Y%m%d %H%M%S", "%Y%m%d %H%M",
        "%Y-%m-%d %I:%M %p", "%m/%d/%Y %I:%M %p", "%d/%m/%Y %I:%M %p"
      )))
    })

    parse_dt_utc_then_local <- memoise::memoise(function(x, tz_out) {
      x <- as.character(x)
      dt_utc <- suppressWarnings(as.POSIXct(x, tz = "UTC", tryFormats = c(
        "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
        "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
        "%Y%m%d %H%M%S", "%Y%m%d %H%M",
        "%Y-%m-%d %I:%M %p", "%m/%d/%Y %I:%M %p", "%d/%m/%Y %I:%M %p"
      )))
      if (all(is.na(dt_utc))) {
        return(dt_utc)
      }
      if (requireNamespace("lubridate", quietly = TRUE)) {
        lubridate::with_tz(dt_utc, tz = tz_out)
      } else {
        ch <- format(dt_utc, "%Y-%m-%d %H:%M:%S", tz = "UTC")
        as.POSIXct(ch, tz = tz_out)
      }
    })


    parse_z_to_hours <- function(zstr) {
      zstr <- gsub(":", "", zstr %||% "")
      zstr[zstr == ""] <- NA_character_
      sign <- ifelse(substr(zstr, 1, 1) == "-", -1, 1)
      h <- suppressWarnings(as.numeric(substr(zstr, 2, 3)))
      m <- suppressWarnings(as.numeric(substr(zstr, 4, 5)))
      h[!is.finite(h)] <- 0
      m[!is.finite(m)] <- 0
      sign * (h + m / 60)
    }


    fast_fingerprint <- function(wx) {
      if (is.null(wx) || nrow(wx) == 0) {
        return(list(n = 0))
      }
      rng <- range(wx$datetime)
      list(
        n = nrow(wx),
        t0 = as.numeric(rng[1]),
        t1 = as.numeric(rng[2]),
        sumT = round(sum(wx$temp, na.rm = TRUE), 2),
        sumR = round(sum(wx$prec, na.rm = TRUE), 3),
        lat = suppressWarnings(as.numeric(wx$lat[1])),
        lon = suppressWarnings(as.numeric(wx$long[1]))
      )
    }

    # ---- Telemetry store (reactiveVal) ----
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

    # perf holders (reactiveVals) to carry timings from reactives to observers
    perf_shape_ms <- reactiveVal(NA_real_)
    perf_model_ms <- reactiveVal(NA_real_)
    perf_daily_ms <- reactiveVal(NA_real_)

    # ---- Reset signal ----
    has_run <- reactiveVal(FALSE)
    reset_signal <- reactiveVal(0L)

    # Emit reset when raw changes or daily_src changes (new upload)
    observeEvent(list(raw_hourly(), daily_src()),
      {
        has_run(FALSE)
        reset_signal(isolate(reset_signal()) + 1L)
        set_metric(list(
          last_event = "upload",
          t_shape_ms = NA_real_, t_model_ms = NA_real_, t_daily_ms = NA_real_,
          n_rows = NA_integer_,
          key_shape = NULL, key_model = NULL, key_daily = NULL,
          stamp = Sys.time()
        ))
      },
      ignoreInit = FALSE
    )

    # ---- Lightweight preview (no parsing/validation) ----
    shaped_input_preview <- reactive({
      rh <- raw_hourly()
      list(
        rows  = if (!is.null(rh)) nrow(rh) else 0L,
        cols  = if (!is.null(rh)) ncol(rh) else 0L,
        tz    = tz$tz_use() %||% "UTC",
        start = filt$start_date()
      )
    }) |> bindCache(
      reactive({
        rh <- raw_hourly()
        list(
          dims = if (is.null(rh)) NULL else list(rows = nrow(rh), cols = ncol(rh)),
          tz = tz$tz_use(),
          start = filt$start_date()
        )
      }),
      cache = cache
    )

    # ---- SHAPED INPUT (runs ONLY when run_click fires) ----
    shaped_input_core <- reactive({
      t0 <- proc.time()[["elapsed"]]
      df <- isolate(raw_hourly())
      req(df)
      df <- data.table::as.data.table(df)

      # Validate required mappings only if standard cols missing
      has_standard <- all(c("temp", "rh", "ws") %in% names(df))
      needed_map <- c(mapping$col_temp(), mapping$col_rh(), mapping$col_ws(), mapping$col_rain())
      if (!has_standard) {
        validate(need(all(nzchar(needed_map)), tr("err_non_numeric_cols")))
        bad <- Filter(function(nm) !is.numeric(df[[nm]]), needed_map)
        validate(need(length(bad) == 0, tr("err_non_numeric_cols", cols = paste(bad, collapse = ", "))))
      }

      # Init & location validations
      validate(
        need(is.finite(init$ffmc0()) && data.table::between(init$ffmc0(), 0, 101), tr("err_ffmc_range")),
        need(is.finite(init$dmc0()) && init$dmc0() >= 0, tr("err_dmc_range")),
        need(is.finite(init$dc0()) && init$dc0() >= 0, tr("err_dc_range")),
        need(is.finite(mapping$manual_lat()) && data.table::between(mapping$manual_lat(), -90, 90), "Latitude must be between -90 and 90."),
        need(is.finite(mapping$manual_lon()) && data.table::between(mapping$manual_lon(), -180, 180), "Longitude must be between -180 and 180.")
      )

      tz_use <- isolate(tz$tz_use())
      validate(need(tz_use %in% OlsonNames(), tr("err_tz_invalid")))

      # Build datetime (prefer standard hourly columns)
      dt_local <- NULL
      if ("datetime" %in% names(df) && inherits(df$datetime, "POSIXt")) {
        dt_local <- df$datetime
      } else if ("timestamp" %in% names(df) && inherits(df$timestamp, "POSIXt")) {
        dt_local <- df$timestamp
      } else if (all(c("yr", "mon", "day", "hr") %in% names(df))) {
        dt_local <- as.POSIXct(sprintf(
          "%04d-%02d-%02d %02d:00:00",
          as.integer(df$yr), as.integer(df$mon), as.integer(df$day), as.integer(df$hr)
        ), tz = tz_use)
      } else {
        # Mapping-based fallbacks
        dt_col <- get_col(df, isolate(mapping$col_datetime()))
        date_col <- get_col(df, isolate(mapping$col_date()))
        time_col <- get_col(df, isolate(mapping$col_time()))

        make_combined_dt <- function(date_col, time_col, tz_out) {
          if (inherits(date_col, "Date")) {
            date_str <- format(date_col, "%Y-%m-%d")
          } else {
            date_str <- as.character(date_col)
          }
          if (is.numeric(time_col)) {
            hr <- floor(time_col)
            mn <- floor((time_col - hr) * 60)
            sc <- round((time_col - hr - mn / 60) * 3600)
            time_str <- sprintf(
              "%02d:%02d:%02d",
              pmax(0, pmin(23, hr)),
              pmax(0, pmin(59, mn)),
              pmax(0, pmin(59, sc))
            )
          } else {
            time_str <- as.character(time_col)
          }
          combined <- paste(date_str, time_str)
          if (has_explicit_zone(combined)) parse_dt_utc_then_local(combined, tz_out = tz_out) else parse_dt_base(combined, tz_out = tz_out)
        }

        if (!is.null(dt_col)) {
          if (has_explicit_zone(dt_col)) {
            dt_utc <- parse_dt_base(dt_col, tz_out = "UTC")
            validate(need(!all(is.na(dt_utc)), "Could not parse your Date–Time column (UTC/offset)."))
            if (requireNamespace("lubridate", quietly = TRUE)) {
              dt_local <- lubridate::with_tz(dt_utc, tz = tz_use)
            } else {
              ch <- format(dt_utc, "%Y-%m-%d %H:%M:%S", tz = "UTC")
              dt_local <- as.POSIXct(ch, tz = tz_use)
            }
          } else {
            dt_local <- parse_dt_base(dt_col, tz_out = tz_use)
            validate(need(!all(is.na(dt_local)), "Could not parse your Date–Time column."))
          }
        } else {
          y <- get_col(df, isolate(mapping$col_year()))
          m <- get_col(df, isolate(mapping$col_month()))
          d <- get_col(df, isolate(mapping$col_day()))
          h <- get_col(df, isolate(mapping$col_hour()))
          validate(need(all(!is.null(c(y, m, d, h))), "Provide Date–Time or Date + Time or Year/Month/Day/Hour."))
          dt_local <- as.POSIXct(sprintf(
            "%04d-%02d-%02d %02d:00:00",
            as.integer(y), as.integer(m), as.integer(d), as.integer(h)
          ), tz = tz_use)
        }
      }

      # Optional start-date filter (robust coercion)
      start_date_val <- isolate(filt$start_date())
      if (!is.null(start_date_val)) {
        if (identical(start_date_val, "") || (is.character(start_date_val) && !nzchar(start_date_val))) {
          start_date_val <- NULL
        } else if (inherits(start_date_val, "POSIXt")) {
          start_date_val <- as.Date(start_date_val)
        } else if (is.character(start_date_val)) {
          start_date_val <- as.Date(start_date_val)
        }
      }
      if (!is.null(start_date_val) && !is.na(start_date_val)) {
        keep <- as.Date(dt_local) >= start_date_val
        validate(need(any(keep), "All rows were filtered out by the start date."))
        df <- df[keep, , drop = FALSE]
        dt_local <- dt_local[keep]
        validate(need(nrow(df) > 0, "No rows remain after filtering; check your date filter or input data."))
      }

      # TZ offset diagnostics and policy
      std_probe <- as.POSIXct("2025-01-15 12:00:00", tz = tz_use)
      std_z <- format(std_probe, "%z")
      std_off_h <- tryCatch(tz_standard_offset_hours(tz_use), error = function(e) NA_real_)
      mmodal <- tryCatch(tz_modal_offset_hours(dt_local), error = function(e) list(offset = NA_real_, z_mode = NA_character_))
      modal_h <- mmodal$offset
      z_mode <- mmodal$z_mode
      offset_hours <- if (identical(isolate(tz$tz_offset_policy()), "std")) std_off_h else modal_h
      if (!isTRUE(all.equal(offset_hours, round(offset_hours)))) {
        warning(sprintf("Time–zone offset has minutes (%.2f h). Rounding to nearest hour for make_inputs().", offset_hours))
        offset_hours <- round(offset_hours)
      }
      validate(need(is.finite(offset_hours) && abs(offset_hours) <= 14, sprintf("Computed GMT offset (%.2f) seems invalid.", offset_hours)))

      # Optional solar radiation
      sol_col <- get_first_col(df, c("solrad", isolate(mapping$col_solrad())))
      if (!is.null(sol_col)) validate(need(is.numeric(sol_col), "Solar radiation must be numeric if provided."))

      # Build shaped inputs (prefer standards)
      lat_val <- suppressWarnings(as.numeric(isolate(mapping$manual_lat())))
      long_val <- suppressWarnings(as.numeric(isolate(mapping$manual_lon())))
      id_col <- get_first_col(df, c("id", isolate(mapping$col_id())))
      temp_col <- get_first_col(df, c("temp", isolate(mapping$col_temp())))
      rh_col <- get_first_col(df, c("rh", isolate(mapping$col_rh())))
      ws_col <- get_first_col(df, c("ws", isolate(mapping$col_ws())))
      prec_col <- get_first_col(df, c("prec", "rain", "prcp", "rf", isolate(mapping$col_rain())))
      validate(need(!is.null(temp_col), "Temperature column not found after parsing."))
      validate(need(!is.null(rh_col), "RH column not found after parsing."))
      validate(need(!is.null(ws_col), "Wind column not found after parsing."))
      validate(need(!is.null(prec_col), "Precipitation column not found after parsing."))
      lat_col <- get_first_col(df, c("lat"))
      long_col <- get_first_col(df, c("long"))

      # data.table shaping, include solrad optionally
      wx <- data.table::data.table(
        datetime  = dt_local,
        timestamp = dt_local, # downstream expects equality
        yr        = as.integer(format(dt_local, "%Y")),
        mon       = as.integer(format(dt_local, "%m")),
        day       = as.integer(format(dt_local, "%d")),
        hr        = as.integer(format(dt_local, "%H")),
        temp      = as.numeric(temp_col),
        rh        = as.numeric(rh_col),
        ws        = as.numeric(ws_col),
        prec      = as.numeric(prec_col),
        lat       = if (!is.null(lat_col)) as.numeric(lat_col) else rep(lat_val, nrow(df)),
        long      = if (!is.null(long_col)) as.numeric(long_col) else rep(long_val, nrow(df)),
        tz        = tz_use,
        id        = if (!is.null(id_col)) as.character(id_col) else rep_len(NA_character_, nrow(df))
      )
      if (!is.null(sol_col)) wx[, solrad := as.numeric(sol_col)]
      data.table::setorder(wx, datetime)

      # post-parse NA checks
      validate(need(all(!is.na(wx$temp)), "Temperature has NA after parsing."))
      validate(need(all(!is.na(wx$rh)), "RH has NA after parsing."))
      validate(need(all(!is.na(wx$ws)), "Wind has NA after parsing."))
      validate(need(all(!is.na(wx$prec)), "Rain has NA after parsing."))

      t1 <- proc.time()[["elapsed"]]
      perf_shape_ms((t1 - t0) * 1000)

      list(
        inputs       = wx,
        tz           = tz_use,
        tz_offset    = offset_hours,
        start_date   = isolate(filt$start_date()),
        n_rows       = nrow(wx),
        diag_std_z   = std_z,
        diag_modal_z = z_mode,
        include_sol  = !is.null(sol_col)
      )
    })

    # Cache keys for shaping
    shaped_input_tmp <- shaped_input_core
    if (isTRUE(enable_cache)) {
      shaped_input_tmp <- shiny::bindCache(
        shaped_input_tmp,
        reactive({
          rh <- raw_hourly()
          list(
            rh = if (is.null(rh)) NULL else list(rows = nrow(rh), cols = ncol(rh)),
            map = list(
              dt = mapping$col_datetime(), yr = mapping$col_year(), mon = mapping$col_month(),
              day = mapping$col_day(), hr = mapping$col_hour(),
              date = mapping$col_date(), time = mapping$col_time(), sol = mapping$col_solrad(),
              t = mapping$col_temp(), rh = mapping$col_rh(), ws = mapping$col_ws(),
              prec = mapping$col_rain(), id = mapping$col_id(),
              lat = mapping$manual_lat(), lon = mapping$manual_lon()
            ),
            tz = list(use = tz$tz_use(), policy = tz$tz_offset_policy(), mode = tz$tz_mode()),
            start = filt$start_date()
          )
        }),
        cache = cache
      )
    }
    shaped_input <- shiny::bindEvent(shaped_input_tmp, run_click(), ignoreInit = TRUE)

    # ---- HOURLY MODEL (FWI25) ----
    run_model_core <- reactive({
      t0 <- proc.time()[["elapsed"]]
      si <- shaped_input()
      req(si)
      req(nrow(si$inputs) > 0)
      inputs <- si$inputs

      validate(need(exists("hFWI"), "hFWI() not found after sourcing NG_FWI.r"))

      fml <- tryCatch(formals(hFWI), error = function(e) NULL)
      out <- NULL
      try(
        {
          if (!is.null(fml)) {
            argn <- names(fml)
            if (all(c("df_wx", "timezone") %in% argn)) {
              out <- hFWI(
                df_wx = inputs, timezone = si$tz_offset,
                ffmc_old = isolate(init$ffmc0()),
                dmc_old = isolate(init$dmc0()),
                dc_old = isolate(init$dc0()),
                silent = TRUE
              )
            } else if ("inputs" %in% argn) {
              out <- hFWI(
                inputs = inputs,
                ffmc_old = isolate(init$ffmc0()),
                dmc_old = isolate(init$dmc0()),
                dc_old = isolate(init$dc0()),
                silent = TRUE
              )
            } else if ("df" %in% argn) {
              out <- hFWI(
                df = inputs,
                ffmc_old = isolate(init$ffmc0()),
                dmc_old = isolate(init$dmc0()),
                dc_old = isolate(init$dc0()),
                silent = TRUE
              )
            } else {
              # positional fallbacks
              out <- try(hFWI(inputs, si$tz_offset,
                isolate(init$ffmc0()), isolate(init$dmc0()), isolate(init$dc0()),
                silent = TRUE
              ), silent = TRUE)
              if (inherits(out, "try-error")) {
                out <- hFWI(inputs, isolate(init$ffmc0()), isolate(init$dmc0()), isolate(init$dc0()), silent = TRUE)
              }
            }
          } else {
            out <- try(hFWI(
              df_wx = inputs, timezone = si$tz_offset,
              ffmc_old = isolate(init$ffmc0()),
              dmc_old = isolate(init$dmc0()),
              dc_old = isolate(init$dc0()),
              silent = TRUE
            ), silent = TRUE)
            if (inherits(out, "try-error")) {
              out <- hFWI(
                df_wx = inputs,
                ffmc_old = isolate(init$ffmc0()),
                dmc_old = isolate(init$dmc0()),
                dc_old = isolate(init$dc0()),
                silent = TRUE
              )
            }
          }
        },
        silent = TRUE
      )
      validate(need(!is.null(out), "hFWI() call failed; check the Log tab for details."))

      dt <- data.table::as.data.table(as.data.frame(out))

      # Rounding
      lat_digits <- memo_count_decimals(isolate(mapping$manual_lat()))
      if (!is.finite(lat_digits) || lat_digits < 0) lat_digits <- 4L
      long_digits <- memo_count_decimals(isolate(mapping$manual_lon()))
      if (!is.finite(long_digits) || long_digits < 0) long_digits <- 4L
      spec_model <- c(
        temp = digits_for_temp, rh = digits_for_rh, ws = digits_for_ws, rain = digits_for_prec,
        lat = lat_digits, long = long_digits,
        ffmc = digits_for_ffmc, dmc = digits_for_dmc, dc = digits_for_dc,
        isi = digits_for_isi, bui = digits_for_bui, fwi = digits_for_fwi, dsr = digits_for_dsr
      )
      dt <- round_by_spec_dt(dt, spec_model,
        default_digits   = default_numeric_digits,
        default_integers = FALSE,
        never_round      = never_round_names
      )

      t1 <- proc.time()[["elapsed"]]
      perf_model_ms((t1 - t0) * 1000)
      dt
    })

    run_model_tmp <- run_model_core
    if (isTRUE(enable_cache)) {
      run_model_tmp <- shiny::bindCache(
        run_model_tmp,
        reactive({
          si <- shaped_input()
          list(
            si = if (!is.null(si)) fast_fingerprint(si$inputs) else NULL,
            tz_off = if (!is.null(si)) si$tz_offset else NULL,
            ffmc = init$ffmc0(), dmc = init$dmc0(), dc = init$dc0()
          )
        }),
        cache = cache
      )
    }
    run_model <- shiny::bindEvent(run_model_tmp, run_click(), ignoreInit = TRUE)

    # ---- DAILY (FWI87) ----
    daily_fwi_core <- reactive({
      t0 <- proc.time()[["elapsed"]]
      if (!isTRUE(isolate(init$calc_fwi87()))) {
        return(NULL)
      }
      tz_use <- isolate(tz$tz_use()) %||% "UTC"

      # Branch A: use daily source
      ds <- isolate(daily_src())
      if (!is.null(ds) && nrow(ds) > 0) {
        d <- data.table::as.data.table(as.data.frame(ds))

        yr <- get_first_col(d, c("yr", "year"))
        mon <- get_first_col(d, c("mon", "month"))
        day <- get_first_col(d, c("day"))
        date <- get_first_col(d, c("date"))

        if (is.null(yr) || is.null(mon) || is.null(day)) {
          validate(need(!is.null(date), "Daily source missing yr/mon/day and date."))
          date_parsed <- as.Date(date)
          yr <- as.integer(format(date_parsed, "%Y"))
          mon <- as.integer(format(date_parsed, "%m"))
          day <- as.integer(format(date_parsed, "%d"))
        } else {
          yr <- as.integer(yr)
          mon <- as.integer(mon)
          day <- as.integer(day)
        }

        temp <- as.numeric(get_first_col(d, c("temp", mapping$col_temp())))
        rh <- as.numeric(get_first_col(d, c("rh", mapping$col_rh())))
        ws <- as.numeric(get_first_col(d, c("ws", mapping$col_ws())))
        pcol <- get_first_col(d, c("prec", "rain", "prcp", "rf", mapping$col_rain()))
        validate(need(!is.null(temp), "Daily source missing 'temp'."))
        validate(need(!is.null(rh), "Daily source missing 'rh'."))
        validate(need(!is.null(ws), "Daily source missing 'ws'."))
        validate(need(!is.null(pcol), "Daily source missing precipitation ('prec'/'rain'/'prcp'/'rf')."))
        prec <- as.numeric(pcol)

        lat_col <- as.numeric(get_first_col(d, c("lat")))
        long_col <- as.numeric(get_first_col(d, c("long")))
        lat_fb <- if (is.finite(lat_col[1])) lat_col[1] else suppressWarnings(as.numeric(mapping$manual_lat()))
        long_fb <- if (is.finite(long_col[1])) long_col[1] else suppressWarnings(as.numeric(mapping$manual_lon()))
        id_col <- get_first_col(d, c("id"))
        id <- if (!is.null(id_col)) as.character(id_col) else rep_len(NA_character_, length(yr))

        daily_in <- data.table::data.table(
          yr = yr, mon = mon, day = day,
          temp = temp, rh = rh, ws = ws, prec = prec,
          lat = if (!is.na(lat_fb)) lat_fb else NA_real_,
          long = if (!is.na(long_fb)) long_fb else NA_real_,
          id = id
        )
        # Noon timestamp
        daily_in[, datetime := as.POSIXct(sprintf("%04d-%02d-%02d 12:00:00", yr, mon, day), tz = tz_use)]
        daily_in[, date := as.Date(datetime)]

        out <- tryCatch(
          {
            cffdrs::fwi(
              input = as.data.frame(daily_in),
              init = data.frame(ffmc = isolate(init$ffmc0()), dmc = isolate(init$dmc0()), dc = isolate(init$dc0()), lat = daily_in$lat[1]),
              batch = TRUE, out = "all", lat.adjust = TRUE, uppercase = FALSE
            )
          },
          error = function(e) {
            message("cffdrs::fwi() failed on daily_src: ", conditionMessage(e))
            NULL
          }
        )
        if (is.null(out)) {
          return(NULL)
        }

        df87 <- as.data.frame(out)
        names(df87) <- tolower(names(df87))
        if (!("datetime" %in% names(df87))) df87$datetime <- daily_in$datetime
        if (!("date" %in% names(df87))) df87$date <- as.Date(df87$datetime, tz = tz_use)
        if (!("id" %in% names(df87))) df87$id <- daily_in$id

        d87 <- data.table::as.data.table(df87)
        if ("date" %in% names(d87)) d87[, date := as.Date(date)]
        # Rounding for daily output
        lat_digits <- memo_count_decimals(isolate(mapping$manual_lat()))
        if (!is.finite(lat_digits) || lat_digits < 0) lat_digits <- 4L
        long_digits <- memo_count_decimals(isolate(mapping$manual_lon()))
        if (!is.finite(long_digits) || long_digits < 0) long_digits <- 4L
        spec_daily <- c(
          temp = digits_for_temp, rh = digits_for_rh, ws = digits_for_ws, prec = digits_for_prec,
          lat = lat_digits, long = long_digits,
          ffmc = digits_for_ffmc, dmc = digits_for_dmc, dc = digits_for_dc,
          isi = digits_for_isi, bui = digits_for_bui, fwi = digits_for_fwi, dsr = digits_for_dsr
        )
        d87 <- round_by_spec_dt(d87, spec_daily,
          default_digits = default_numeric_digits,
          default_integers = FALSE,
          never_round = never_round_names
        )

        t1 <- proc.time()[["elapsed"]]
        perf_daily_ms((t1 - t0) * 1000)
        return(as.data.frame(d87))
      }

      # Branch B: derive daily from hourly shaped_input
      si <- isolate(shaped_input())
      req(si, si$inputs)
      req(nrow(si$inputs) > 0)
      tz_use <- if (is.null(si$tz) || !nzchar(si$tz)) "UTC" else si$tz
      wx <- data.table::as.data.table(as.data.frame(si$inputs))
      req("datetime" %in% names(wx) && inherits(wx$datetime, "POSIXt"))
      for (nm in c("temp", "rh", "ws")) {
        if (!(nm %in% names(wx))) stop(sprintf("daily_fwi_df(): '%s' not found.", nm))
      }
      pcol <- find_precip_col(names(wx))
      if (is.null(pcol)) stop("daily_fwi_df(): couldn't find precipitation ('rain'/'precip'/'prec'/'prcp'/'rf').")

      use_std <- identical(isolate(tz$tz_offset_policy()), "std")

      # Base local time columns (preserve id), avoid lubridate ops
      if (use_std) {
        cur_off_h <- parse_z_to_hours(format(wx$datetime, "%z"))
        std_off_h <- tryCatch(tz_standard_offset_hours(tz_use), error = function(e) NA_real_)
        delta_h <- cur_off_h - std_off_h
        # Adjust to LST by subtracting delta (seconds)
        wx[, datetime_LST := wx$datetime - (delta_h * 3600)]
        wx[, dt_base := datetime_LST]
      } else {
        wx[, dt_base := datetime]
      }
      wx[, hour := as.integer(format(dt_base, "%H"))]
      wx[, date := as.Date(dt_base)]

      # ONE closest-to-noon per (id, date)
      nearest_noon_per_day <- function(DT, dt_col = "dt_base", id_col = "id") {
        DT <- data.table::copy(DT)
        DT[, date := as.Date(get(dt_col))]
        DT[, hour := as.integer(format(get(dt_col), "%H"))]
        DT[, dist := abs(hour - 12L)]
        data.table::setorderv(DT, c(id_col, "date", "dist", dt_col))
        DT[DT[, .I[1L], by = c(id_col, "date")]$V1]
      }
      noon_tbl <- data.table::as.data.table(nearest_noon_per_day(wx, dt_col = "dt_base", id_col = "id"))
      noon_tbl[, date := as.Date(dt_base)]

      # Daily window 13:00(prev) -> 12:00(current) per (id, date)
      noon_tbl[, `:=`(
        start = as.POSIXct(paste0(format(date - 1L, "%Y-%m-%d"), " 13:00:00"), tz = tz_use),
        end   = as.POSIXct(paste0(format(date, "%Y-%m-%d"), " 12:00:00"), tz = tz_use)
      )]
      if (use_std) {
        end_delta_h <- parse_z_to_hours(format(noon_tbl$end, "%z")) - tryCatch(tz_standard_offset_hours(tz_use), error = function(e) NA_real_)
        noon_tbl[, `:=`(
          start = start - (end_delta_h * 3600),
          end   = end - (end_delta_h * 3600)
        )]
      }

      # Sum precipitation per (id, window) with non-equi join keyed by id
      data.table::setkey(wx, id, dt_base)
      acc <- wx[
        noon_tbl,
        on = .(id, dt_base >= start, dt_base <= end),
        allow.cartesian = TRUE,
        .(prec_24 = sum(get(pcol), na.rm = TRUE), n_hours = sum(!is.na(get(pcol)))),
        by = .EACHI
      ]
      noon_tbl[, `:=`(prec_24 = acc$prec_24, n_hours = acc$n_hours)]
      if (!(pcol %in% names(noon_tbl))) noon_tbl[, (pcol) := NA_real_]

      # Per-row lat/long from noon record; fall back to global if missing
      lat_fb <- if (is.finite(si$inputs$lat[1])) si$inputs$lat[1] else suppressWarnings(as.numeric(isolate(mapping$manual_lat())))
      long_fb <- if (is.finite(si$inputs$long[1])) si$inputs$long[1] else suppressWarnings(as.numeric(isolate(mapping$manual_lon())))
      lat_vec <- if ("lat" %in% names(noon_tbl)) noon_tbl$lat else lat_fb
      long_vec <- if ("long" %in% names(noon_tbl)) noon_tbl$long else long_fb

      # Build cffdrs::fwi input; include id (string)
      daily_in <- data.table::data.table(
        yr   = as.integer(format(noon_tbl$dt_base, "%Y")),
        mon  = as.integer(format(noon_tbl$dt_base, "%m")),
        day  = as.integer(format(noon_tbl$dt_base, "%d")),
        temp = as.numeric(noon_tbl$temp),
        rh   = as.numeric(noon_tbl$rh),
        ws   = as.numeric(noon_tbl$ws),
        prec = as.numeric(noon_tbl$prec_24),
        lat  = as.numeric(lat_vec),
        long = as.numeric(long_vec),
        id   = as.character(noon_tbl$id)
      )
      out <- tryCatch(
        {
          cffdrs::fwi(
            input = as.data.frame(daily_in),
            init = data.frame(ffmc = isolate(init$ffmc0()), dmc = isolate(init$dmc0()), dc = isolate(init$dc0()), lat = lat_fb),
            batch = TRUE, out = "all", lat.adjust = TRUE, uppercase = FALSE
          )
        },
        error = function(e) {
          message("cffdrs::fwi() failed: ", conditionMessage(e))
          NULL
        }
      )
      if (is.null(out)) {
        return(NULL)
      }

      df87 <- as.data.frame(out)
      names(df87) <- tolower(names(df87))

      # Ensure datetime/date exist (noon-based)
      if (!("datetime" %in% names(df87))) {
        mon_col <- if ("mon" %in% names(df87)) "mon" else if ("month" %in% names(df87)) "month" else NULL
        if (!is.null(mon_col) && all(c("yr", "day") %in% names(df87))) {
          df87$datetime <- as.POSIXct(sprintf(
            "%04d-%02d-%02d 12:00:00",
            as.integer(df87$yr),
            as.integer(df87[[mon_col]]),
            as.integer(df87$day)
          ), tz = tz_use)
        }
      }
      if ("datetime" %in% names(df87) && !("date" %in% names(df87))) {
        df87$date <- as.Date(df87$datetime, tz = tz_use)
      } else if (!("date" %in% names(df87)) && all(c("yr", "day") %in% names(df87))) {
        mon_col <- if ("mon" %in% names(df87)) "mon" else if ("month" %in% names(df87)) "month" else NULL
        if (!is.null(mon_col)) {
          df87$date <- as.Date(sprintf(
            "%04d-%02d-%02d",
            as.integer(df87$yr),
            as.integer(df87[[mon_col]]),
            as.integer(df87$day)
          ))
        }
      }

      d87 <- data.table::as.data.table(df87)
      if ("date" %in% names(d87)) d87[, date := as.Date(date)]

      # Attach 12->12 precip info back by (id, date)
      add_cols <- data.table::as.data.table(noon_tbl[, .(id = as.character(id), date, precip_12to12 = prec_24, n_hours)])
      data.table::setkey(add_cols, id, date)
      if (!("id" %in% names(d87))) {
        map <- unique(add_cols[, .(date, id)])
        ambig <- map[, .N, by = date][N > 1L, unique(date)]
        if (length(ambig) == 0L && "date" %in% names(d87)) {
          d87 <- merge(d87, map, by = "date", all.x = TRUE, sort = FALSE)
        }
      }
      data.table::setkey(d87, id, date)
      d87 <- d87[add_cols, on = .(id, date)]
      ord <- try(order(d87$datetime), silent = TRUE)
      if (!inherits(ord, "try-error")) d87 <- d87[ord]

      # Rounding for daily output
      lat_digits <- memo_count_decimals(isolate(mapping$manual_lat()))
      if (!is.finite(lat_digits) || lat_digits < 0) lat_digits <- 4L
      long_digits <- memo_count_decimals(isolate(mapping$manual_lon()))
      if (!is.finite(long_digits) || long_digits < 0) long_digits <- 4L
      spec_daily <- c(
        temp = digits_for_temp, rh = digits_for_rh, ws = digits_for_ws, prec = digits_for_prec,
        precip_12to12 = digits_for_prec,
        lat = lat_digits, long = long_digits,
        ffmc = digits_for_ffmc, dmc = digits_for_dmc, dc = digits_for_dc,
        isi = digits_for_isi, bui = digits_for_bui, fwi = digits_for_fwi, dsr = digits_for_dsr
      )
      d87 <- round_by_spec_dt(d87, spec_daily,
        default_digits = default_numeric_digits,
        default_integers = FALSE,
        never_round = never_round_names
      )

      t1 <- proc.time()[["elapsed"]]
      perf_daily_ms((t1 - t0) * 1000)
      as.data.frame(d87)
    })

    daily_fwi_tmp <- daily_fwi_core
    if (isTRUE(enable_cache)) {
      daily_fwi_tmp <- shiny::bindCache(
        daily_fwi_tmp,
        reactive({
          si <- shaped_input()
          list(
            si = if (!is.null(si)) fast_fingerprint(si$inputs) else NULL,
            ffmc = init$ffmc0(), dmc = init$dmc0(), dc = init$dc0(),
            policy = tz$tz_offset_policy(), do87 = init$calc_fwi87(),
            has_daily_src = !is.null(daily_src()) && nrow(daily_src()) > 0
          )
        }),
        cache = cache
      )
    }
    daily_fwi_df <- shiny::bindEvent(daily_fwi_tmp, run_click(), ignoreInit = TRUE)

    # ---- Telemetry observers (Option 2 fix) ----
    observeEvent(shaped_input(), {
      si <- shaped_input()
      set_metric(list(
        last_event = "shape",
        t_shape_ms = isolate(perf_shape_ms()),
        n_rows     = if (is.null(si)) NA_integer_ else nrow(si$inputs),
        key_shape  = if (!is.null(si)) fast_fingerprint(si$inputs) else NULL,
        stamp      = Sys.time()
      ))
    })

    observeEvent(run_model(), {
      si <- isolate(shaped_input())
      set_metric(list(
        last_event = "model",
        t_model_ms = isolate(perf_model_ms()),
        key_model = list(
          si   = if (!is.null(si)) fast_fingerprint(si$inputs) else NULL,
          ffmc = isolate(init$ffmc0()),
          dmc  = isolate(init$dmc0()),
          dc   = isolate(init$dc0())
        ),
        stamp = Sys.time()
      ))
      has_run(TRUE)
    })

    observeEvent(daily_fwi_df(), {
      si <- isolate(shaped_input())
      set_metric(list(
        last_event = "daily",
        t_daily_ms = isolate(perf_daily_ms()),
        key_daily = list(
          si = if (!is.null(si)) fast_fingerprint(si$inputs) else NULL,
          ffmc = isolate(init$ffmc0()),
          dmc = isolate(init$dmc0()),
          dc = isolate(init$dc0()),
          policy = isolate(tz$tz_offset_policy()),
          do87 = isolate(init$calc_fwi87())
        ),
        stamp = Sys.time()
      ))
    })

    # ---- Return interface ----
    return(list(
      shaped_input           = shaped_input, # run-bound
      shaped_input_preview   = shaped_input_preview, # lightweight
      run_model              = run_model, # hourly FWI25
      daily_fwi_df           = daily_fwi_df, # daily FWI87
      metrics                = reactive(.metrics()),
      reset                  = reactive(reset_signal()) # UI reset signal
    ))
  })
}
