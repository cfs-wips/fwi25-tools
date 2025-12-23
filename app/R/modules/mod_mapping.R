mod_mapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5(id = ns("lbl_column_mapping"), uiOutput(ns("lbl_column_mapping"))),
    tags$fieldset(
      id = ns("mapping_fs"), # for JS targeting
      role = "group",
      `aria-labelledby` = ns("lbl_column_mapping"),
      `aria-disabled` = "true", # start disabled until file upload

      # --- Mapping inputs (layout preserved) ---
      fluidRow(
        column(
          4,
          selectInput(ns("col_datetime"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_datetime"))
        ),
        column(
          4,
          selectInput(ns("col_id"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_id"))
        ),
        column(
          4,
          selectInput(ns("col_date"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_date"))
        ),
      ),
      fluidRow(
        column(
          4,
          selectInput(ns("col_time"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_time"))
        ),
        column(
          4, numericInput(ns("manual_lat"), label = NULL, value = NULL, min = -90, max = 90, step = 0.0001),
          uiOutput(ns("v_manual_lat"))
        ),
        column(
          4, numericInput(ns("manual_lon"), label = NULL, value = NULL, min = -180, max = 180, step = 0.0001),
          uiOutput(ns("v_manual_lon"))
        )
      ),
      fluidRow(
        column(
          4,
          selectInput(ns("col_year"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_year"))
        ),
        column(
          4,
          selectInput(ns("col_month"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_month"))
        ),
        column(
          4,
          selectInput(ns("col_day"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_day"))
        )
      ),
      fluidRow(
        column(
          4,
          selectInput(ns("col_hour"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_hour"))
        ),
        column(
          4,
          selectInput(ns("col_temp"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_temp"))
        ),
        column(
          4,
          selectInput(ns("col_rh"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_rh"))
        )
      ),
      fluidRow(
        column(
          4,
          selectInput(ns("col_ws"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_ws"))
        ),
        column(
          4,
          selectInput(ns("col_rain"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_rain"))
        ),
        column(
          4,
          selectInput(ns("col_solrad"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_solrad"))
        )
      ),
      

      # Global banner and per-station accordion (appear when blocking issues)
      uiOutput(ns("mapping_alert")),
      uiOutput(ns("station_accordion"))
    )
  )
}

mod_mapping_server <- function(id, tr, lang, cols, df) {
  moduleServer(id, function(input, output, session) {
    # Packages used (available in app environment)
    # data.table for fast summaries; lubridate for date checks
    DT <- NULL

    # --- Section heading ---
    output$lbl_column_mapping <- renderUI({
      label_with_help_rich(
        label_text = tr("column_mapping"),
        tip_text = tr("tt_mapping_header"),
        popover_html = tr("mapping_help"),
        sr_label = tr("column_mapping")
      )
    })

    last_cols <- reactiveVal(character())

    # --- JS toggle for disabled state ---
    observe({
      session$sendCustomMessage("mappingSetDisabled", list(
        id = session$ns("mapping_fs"),
        disabled = (length(cols()) == 0)
      ))
    }) |> bindEvent(cols())

    # --- Helpers ---
    tt_or_null <- function(key) {
      v <- tr(key)
      if (identical(v, key)) NULL else v
    }
    label_tag <- function(k, tt = NULL) {
      if (is.null(tt)) {
        tr(k)
      } else {
        tip <- tt_or_null(tt)
        if (is.null(tip)) tr(k) else label_with_help(tr(k), tip)
      }
    }
    escape <- function(x) gsub("([\\.^$(){}+*?\\[\\]\\\\])", "\\\\\\\\1", x, perl = TRUE)
    find_col <- function(cols, keywords) {
      rx <- paste0("^(", paste(escape(keywords), collapse = "|"), ")$")
      m <- cols[grepl(rx, cols, ignore.case = TRUE)]
      if (length(m) > 0) {
        return(m[1])
      }
      norm <- function(x) gsub("[^a-z0-9]+", "", tolower(trimws(x)))
      nc <- norm(cols)
      nk <- norm(keywords)
      idx <- which(nc %in% nk)
      if (length(idx) > 0) cols[idx[1]] else ""
    }

    # Column profiler (sample first 5000 rows)
    profiler <- reactive({
      dval <- df()
      if (is.null(dval) || !length(dval)) {
        return(NULL)
      }
      # Keep a data.table view for speed
      suppressWarnings({
        DT <<- data.table::as.data.table(dval)
      })
      n <- nrow(DT)
      if (n == 0) {
        return(NULL)
      }
      idx <- seq_len(min(5000L, n))
      # Build profile list per column
      profiles <- lapply(names(DT), function(nm) {
        v <- DT[[nm]][idx]
        # Basic type flags
        is_num <- is.numeric(v)
        is_int <- is.integer(v)
        is_chr <- is.character(v) || is.factor(v)
        # Numeric coercion success for non-numeric
        v_num <- suppressWarnings(as.numeric(v))
        na_rate_num <- mean(is.na(v_num))
        # Integer-like check on numeric
        non_na <- !is.na(v_num)
        int_like <- if (sum(non_na) > 0) all(abs(v_num[non_na] - round(v_num[non_na])) < .Machine$double.eps^0.5) else FALSE
        v_min <- suppressWarnings(min(v_num, na.rm = TRUE))
        v_max <- suppressWarnings(max(v_num, na.rm = TRUE))
        # Datetime parse success (quick sample up to 200)
        dt_rate <- NA_real_
        if (is_chr) {
          s <- v[!is.na(v)]
          if (length(s) > 0) {
            s <- s[seq_len(min(200L, length(s)))]
            orders <- c("Ymd HMS", "Ymd HM", "Ymd H", "Ymd", "mdY HMS", "mdY HM", "mdY", "dmy HMS", "dmy HM", "dmy")
            ok <- logical(length(s))
            for (o in orders) {
              parsed <- suppressWarnings(lubridate::parse_date_time(s, orders = o, quiet = TRUE))
              ok <- ok | !is.na(parsed)
              if (mean(ok) >= 0.6) break
            }
            dt_rate <- mean(ok)
          }
        }
        examples <- as.character(utils::head(v[!is.na(v)], 5))
        list(
          name = nm,
          is_num = is_num,
          is_int = is_int,
          is_chr = is_chr,
          na_rate_num = na_rate_num,
          int_like = int_like,
          min = v_min,
          max = v_max,
          dt_rate = dt_rate,
          examples = examples
        )
      })
      names(profiles) <- names(DT)
      profiles
    })

    # Utility: order choices with eligible columns first
    order_choices <- function(cc, eligible) {
      eligible <- intersect(eligible, cc)
      other <- setdiff(cc, eligible)
      c("", eligible, other)
    }

    # Eligible sets based on profiles
    eligible_sets <- reactive({
      pr <- profiler()
      if (is.null(pr)) {
        return(list())
      }
      nm <- names(pr)
      # Numeric eligible: numeric or coercible with < 10% NA
      eligible_numeric <- nm[vapply(pr, function(p) p$is_num || (!p$is_num && p$na_rate_num < 0.1), logical(1))]
      # Integer eligible: integer or integer-like values
      eligible_integer <- nm[vapply(pr, function(p) p$is_int || (p$is_num && p$int_like) || (!p$is_num && p$na_rate_num < 0.1 && p$int_like), logical(1))]
      # Datetime eligible: already POSIXt/Date or char with parse rate >= 0.6
      eligible_datetime <- nm[vapply(pr, function(p) {
        v <- DT[[p$name]]
        inherits(v, "POSIXt") || inherits(v, "Date") || (p$is_chr && !is.na(p$dt_rate) && p$dt_rate >= 0.6)
      }, logical(1))]
      list(
        numeric = eligible_numeric,
        integer = eligible_integer,
        datetime = eligible_datetime,
        profiles = pr
      )
    })

    # --- Update choices when file changes ---
    observeEvent(cols(), {
      cc <- cols()
      if (length(cc) == 0) {
        # Clear choices
        for (id_ in c("col_datetime", "col_id", "col_date", "col_time", "col_year", "col_month", "col_day", "col_hour", "col_temp", "col_rh", "col_ws", "col_rain", "col_solrad")) {
          updateSelectInput(session, id_, choices = c(""), selected = "")
        }
        return()
      }
      pr <- eligible_sets()
      elig_num <- pr$numeric %||% character()
      elig_int <- pr$integer %||% character()
      elig_dt <- pr$datetime %||% character()

      keep_or <- function(cur, pool, fallback) if (nzchar(cur) && cur %in% pool) cur else fallback

      # Datetime / ID
      updateSelectInput(session, "col_datetime",
        choices  = order_choices(cc, elig_dt),
        selected = keep_or(input$col_datetime, cc, find_col(cc, c("datetime", "timestamp", "Time and Date", "Date and Time", "Date & Time", "Time & Date", "Datetime", "DateTime")))
      )
      updateSelectInput(session, "col_id",
        choices  = c("", cc),
        selected = keep_or(input$col_id, cc, find_col(cc, c("Station Name", "Station", "ID", "WMO", "AES")))
      )

      # Date / Time
      updateSelectInput(session, "col_date",
        choices  = order_choices(cc, c(elig_dt, pr$integer %||% character())),
        selected = keep_or(input$col_date, cc, find_col(cc, c("date", "obs_date", "dt", "day_date")))
      )
      updateSelectInput(session, "col_time",
        choices  = order_choices(cc, c(elig_dt, cc[grepl("(^hhmm$|^hh:mm$|time|obs_time|hour_min)", cc, ignore.case = TRUE)])),
        selected = keep_or(input$col_time, cc, find_col(cc, c("time", "obs_time", "hour_min", "hhmm", "hh:mm")))
      )

      # Parts
      updateSelectInput(session, "col_year",
        choices  = order_choices(cc, elig_int),
        selected = keep_or(input$col_year, cc, find_col(cc, c("year", "yr", "y")))
      )
      updateSelectInput(session, "col_month",
        choices  = order_choices(cc, elig_int),
        selected = keep_or(input$col_month, cc, find_col(cc, c("month", "mon", "m")))
      )
      updateSelectInput(session, "col_day",
        choices  = order_choices(cc, elig_int),
        selected = keep_or(input$col_day, cc, find_col(cc, c("day", "dy", "d")))
      )
      updateSelectInput(session, "col_hour",
        choices  = order_choices(cc, elig_int),
        selected = keep_or(input$col_hour, cc, find_col(cc, c("hour", "hr", "h")))
      )

      # Met
      updateSelectInput(session, "col_temp",
        choices  = order_choices(cc, elig_num),
        selected = keep_or(input$col_temp, cc, find_col(cc, c("temp", "temperature", "t")))
      )
      updateSelectInput(session, "col_rh",
        choices  = order_choices(cc, elig_num),
        selected = keep_or(input$col_rh, cc, find_col(cc, c("rh", "humidity", "relative_humidity", "relative humidity")))
      )
      updateSelectInput(session, "col_ws",
        choices  = order_choices(cc, elig_num),
        selected = keep_or(input$col_ws, cc, find_col(cc, c("wind", "ws", "windspeed", "wind_speed", "wind.speed", "wind speed", "wspd", "wnd", "wndspd")))
      )
      updateSelectInput(session, "col_rain",
        choices  = order_choices(cc, elig_num),
        selected = keep_or(input$col_rain, cc, find_col(cc, c("rain", "precip", "prec", "precip_mm", "prec_mm", "rain_mm", "rf", "rn", "rn_1")))
      )
      updateSelectInput(session, "col_solrad",
        choices  = order_choices(cc, elig_num),
        selected = keep_or(input$col_solrad, cc, find_col(cc, c("solar", "solrad", "solar_radiation", "shortwave", "srad", "rsds", "swdown", "rad", "radiation", "rad_wm2")))
      )

      is_new_upload <- !identical(last_cols(), cc)
      last_cols(cc)

      dval <- df()
      if (is.null(dval) || !length(dval)) {
        return()
      } # guard for early calls
      dn <- names(dval)
      lat_col <- find_col(dn, c("lat", "latitude"))
      lon_col <- find_col(dn, c("lon", "long", "longitude"))
      lat_def <- suppressWarnings(as.numeric(if (nzchar(lat_col)) dval[[lat_col]][1] else NA))
      lon_def <- suppressWarnings(as.numeric(if (nzchar(lon_col)) dval[[lon_col]][1] else NA))
      if (!is.finite(lat_def)) lat_def <- 55
      if (!is.finite(lon_def)) lon_def <- -120
      # reseed lat/lon if it's a new upload OR inputs are non-finite (cleared/blank)
      if (is_new_upload || !is.finite(input$manual_lat) || !is.finite(input$manual_lon)) {
        updateNumericInput(session, "manual_lat", value = lat_def)
        updateNumericInput(session, "manual_lon", value = lon_def)
      }
    })

    # --- Update labels on language toggle (preserved) ---
    set_all_labels <- function() {
      updateSelectInput(session, "col_datetime", label = label_tag("col_datetime", "tt_col_datetime"))
      updateSelectInput(session, "col_id", label = label_tag("col_id", "tt_col_id"))
      updateSelectInput(session, "col_date", label = label_tag("col_date", "tt_col_date"))
      updateSelectInput(session, "col_time", label = label_tag("col_time", "tt_col_time"))
      updateSelectInput(session, "col_year", label = label_tag("col_year", "tt_col_year"))
      updateSelectInput(session, "col_month", label = label_tag("col_month", "tt_col_month"))
      updateSelectInput(session, "col_day", label = label_tag("col_day", "tt_col_day"))
      updateSelectInput(session, "col_hour", label = label_tag("col_hour", "tt_col_hour"))
      updateSelectInput(session, "col_temp", label = label_tag("col_temp", "tt_col_temp"))
      updateSelectInput(session, "col_rh", label = label_tag("col_rh", "tt_col_rh"))
      updateSelectInput(session, "col_ws", label = label_tag("col_ws", "tt_col_ws"))
      updateSelectInput(session, "col_rain", label = label_tag("col_rain", "tt_col_rain"))
      updateSelectInput(session, "col_solrad", label = label_tag("col_solrad", "tt_col_solrad"))
      updateNumericInput(session, "manual_lat", label = label_tag("lat_label", "tt_manual_lat"))
      updateNumericInput(session, "manual_lon", label = label_tag("lon_label", "tt_manual_lon"))
    }
    observeEvent(TRUE,
      {
        set_all_labels()
      },
      once = TRUE
    )
    observeEvent(lang(),
      {
        set_all_labels()
      },
      ignoreInit = FALSE
    )

    # --- Validation utilities ---
    get_sample <- function(nm) {
      if (!nzchar(nm) || is.null(DT)) {
        return(NULL)
      }
      v <- DT[[nm]]
      if (is.null(v)) {
        return(NULL)
      }
      n <- length(v)
      if (n == 0) {
        return(NULL)
      }
      v[seq_len(min(5000L, n))]
    }
    is_finite_num <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      is.finite(x)
    }

    # HH:MM or HHMM detection for Hour mapping
    extract_hour_from_string <- function(x) {
      x <- as.character(x)
      # HH:MM
      m1 <- grepl("^\\s*([01]?\\d|2[0-3]):[0-5]\\d\\s*$", x)
      if (any(m1)) {
        hh <- as.integer(sub("^\\s*([0-9]{1,2}).*$", "\\1", x[m1]))
        out <- rep(NA_integer_, length(x))
        out[m1] <- hh
        return(out)
      }
      # HHMM (e.g., 0930, 2305)
      m2 <- grepl("^\\s*([01]?\\d|2[0-3])[0-5]\\d\\s*$", x)
      if (any(m2)) {
        hh <- as.integer(sub("^\\s*([0-9]{1,2}).*$", "\\1", x[m2]))
        out <- rep(NA_integer_, length(x))
        out[m2] <- hh
        return(out)
      }
      rep(NA_integer_, length(x))
    }

    # Field validators return list(status, messages, examples, per_station) where status in ok|warn|error
    validate_field <- function(field, nm) {
      v <- get_sample(nm)
      if (is.null(v)) {
        return(list(status = "ok", messages = character(), examples = character(), per_station = NULL))
      }
      msgs <- character()
      status <- "ok"
      examples <- character()
      per_station <- NULL

      add_error <- function(msg) {
        status <<- "error"
        msgs <<- c(msgs, msg)
      }
      add_warn <- function(msg) {
        if (status != "error") status <<- "warn"
        msgs <<- c(msgs, msg)
      }

      # Numeric fields
      if (field %in% c("col_temp", "col_rh", "col_ws", "col_rain", "col_solrad")) {
        vn <- suppressWarnings(as.numeric(v))
        non_na <- !is.na(vn)
        if (sum(non_na) == 0) {
          add_error(tr("val_no_numeric"))
        } else {
          # Criticals and advisories per field
          if (field == "col_rain") {
            if (any(vn[non_na] < 0)) add_error(tr("val_rain_negative"))
            if (any(vn[non_na] > 300)) add_warn(tr("val_rain_extreme"))
          } else if (field == "col_rh") {
            if (any(vn[non_na] < 0 | vn[non_na] > 100)) add_error(tr("val_rh_range"))
          } else if (field == "col_ws") {
            if (any(vn[non_na] < 0)) add_error(tr("val_ws_negative"))
            if (any(vn[non_na] > 60)) add_warn(tr("val_ws_extreme"))
          } else if (field == "col_temp") {
            if (any(vn[non_na] < -60 | vn[non_na] > 60)) add_warn(tr("val_temp_extreme"))
          } else if (field == "col_solrad") {
            if (any(vn[non_na] < 0)) add_error(tr("val_solrad_negative"))
            if (any(vn[non_na] > 1400)) add_warn(tr("val_solrad_extreme"))
          }
        }
        examples <- as.character(utils::head(v[non_na], 3))
      }

      # Calendar parts
      if (field %in% c("col_year", "col_month", "col_day", "col_hour")) {
        # Special: hour may be HH:MM or HHMM strings
        if (field == "col_hour") {
          vn <- suppressWarnings(as.numeric(v))
          non_na_num <- !is.na(vn)
          vhh <- extract_hour_from_string(v)
          non_na_hh <- !is.na(vhh)
          ok_num <- sum(non_na_num) > 0 && all(vn[non_na_num] >= 0 & vn[non_na_num] <= 23)
          ok_hh <- sum(non_na_hh) > 0 && all(vhh[non_na_hh] >= 0 & vhh[non_na_hh] <= 23)
          if (!(ok_num || ok_hh)) {
            add_error(tr("val_hour_range"))
          } else if (ok_hh && !ok_num) {
            add_warn(tr("val_hour_hhmm"))
          }
          examples <- as.character(utils::head(v[non_na_num | non_na_hh], 3))
        } else if (field == "col_month") {
          vn <- suppressWarnings(as.numeric(v))
          non_na <- !is.na(vn)
          if (sum(non_na) == 0 || any(vn[non_na] < 1 | vn[non_na] > 12)) add_error(tr("val_month_range"))
          examples <- as.character(utils::head(v[non_na], 3))
        } else if (field == "col_day") {
          vn <- suppressWarnings(as.numeric(v))
          non_na <- !is.na(vn)
          if (sum(non_na) == 0 || any(vn[non_na] < 1 | vn[non_na] > 31)) add_error(tr("val_day_range"))
          examples <- as.character(utils::head(v[non_na], 3))
        } else if (field == "col_year") {
          vn <- suppressWarnings(as.numeric(v))
          non_na <- !is.na(vn)
          if (sum(non_na) == 0) add_error(tr("val_year_missing"))
          if (any(vn[non_na] < 1900 | vn[non_na] > 2100)) add_warn(tr("val_year_outside"))
          examples <- as.character(utils::head(v[non_na], 3))
        }
      }

      # Datetime quick parse
      if (field == "col_datetime") {
        # Allow character/Date/POSIXt; warn if parse rate low for character
        if (inherits(v, "POSIXt") || inherits(v, "Date")) {
          # ok
        } else {
          vv <- as.character(v)
          vv <- vv[!is.na(vv)]
          if (length(vv) > 0) {
            vv <- vv[seq_len(min(200L, length(vv)))]
            orders <- c("Ymd HMS", "Ymd HM", "Ymd H", "Ymd", "mdY HMS", "mdY HM", "mdY", "dmy HMS", "dmy HM", "dmy")
            ok <- logical(length(vv))
            for (o in orders) {
              parsed <- suppressWarnings(lubridate::parse_date_time(vv, orders = o, quiet = TRUE))
              ok <- ok | !is.na(parsed)
              if (mean(ok) >= 0.6) break
            }
            if (mean(ok) < 0.6) add_warn(tr("val_datetime_parse_low"))
          }
        }
        examples <- as.character(utils::head(v[!is.na(v)], 3))
      }

      # Full-date validity when Y/M/D present (done in combined check below)

      list(status = status, messages = msgs, examples = examples, per_station = per_station)
    }

    # Collect current mappings (debounced for responsiveness)
    mapping_inputs <- reactive({
      list(
        datetime = input$col_datetime,
        id       = input$col_id,
        date     = input$col_date,
        time     = input$col_time,
        year     = input$col_year,
        month    = input$col_month,
        day      = input$col_day,
        hour     = input$col_hour,
        temp     = input$col_temp,
        rh       = input$col_rh,
        ws       = input$col_ws,
        rain     = input$col_rain,
        solrad   = input$col_solrad
      )
    })
    mapping_inputs_db <- shiny::debounce(mapping_inputs, millis = 300)

    # Compute validation for each field + combined constraints
    validations <- reactive({
      mi <- mapping_inputs_db()
      res <- list()
      fields <- c("col_datetime", "col_id", "col_date", "col_time", "col_year", "col_month", "col_day", "col_hour", "col_temp", "col_rh", "col_ws", "col_rain", "col_solrad")
      mapped <- c(mi$datetime, mi$id, mi$date, mi$time, mi$year, mi$month, mi$day, mi$hour, mi$temp, mi$rh, mi$ws, mi$rain, mi$solrad)
      names(mapped) <- fields
      for (k in fields) {
        res[[k]] <- validate_field(k, mapped[[k]])
      }

      # Combined rules: datetime vs Y/M/D/H and full-date validity
      has_dt <- nzchar(mi$datetime)
      has_ymdh <- all(nzchar(mi$year), nzchar(mi$month), nzchar(mi$day), nzchar(mi$hour))
      if (has_dt && has_ymdh) {
        # Non-blocking note: prioritize datetime, ignore Y/M/D/H
        res$combined_priority <- list(status = "warn", messages = tr("val_both_paths"), examples = character(), per_station = NULL)
      }

      # Full date validity only if year/month/day are mapped
      if (all(nzchar(mi$year), nzchar(mi$month), nzchar(mi$day))) {
        y <- suppressWarnings(as.integer(get_sample(mi$year)))
        m <- suppressWarnings(as.integer(get_sample(mi$month)))
        d <- suppressWarnings(as.integer(get_sample(mi$day)))
        len <- min(length(y), length(m), length(d))
        if (len > 0) {
          y <- y[seq_len(len)]
          m <- m[seq_len(len)]
          d <- d[seq_len(len)]
          # Invalid if any NA or impossible date
          invalid <- logical(len)
          for (i in seq_len(len)) {
            if (is.na(y[i]) || is.na(m[i]) || is.na(d[i])) {
              invalid[i] <- TRUE
            } else {
              suppressWarnings({
                dd <- try(as.Date(sprintf("%04d-%02d-%02d", y[i], m[i], d[i])), silent = TRUE)
                invalid[i] <- inherits(dd, "try-error") || is.na(dd)
              })
            }
          }
          if (any(invalid)) {
            res$combined_date <- list(status = "error", messages = tr("val_date_invalid"), examples = as.character(utils::head(paste(y[invalid], m[invalid], d[invalid], sep = "-"), 3L)), per_station = NULL)
          }
        }
      }

      # Per-station summaries (top 5) for critical issues when ID is present
      id_nm <- mi$id
      if (nzchar(id_nm)) {
        # Build a small table of issue counts per station (sampled rows)
        dval <- df()
        if (!is.null(dval) && length(dval) && id_nm %in% names(dval)) {
          idx <- seq_len(min(5000L, nrow(dval)))
          idv <- as.character(dval[[id_nm]][idx])
          issue_cols <- list()
          # For each critical field, compute a logical vector of offending rows
          crit_map <- list(
            col_rain = function() {
              nm <- mi$rain
              if (!nzchar(nm) || !(nm %in% names(dval))) {
                return(rep(FALSE, length(idx)))
              }
              v <- suppressWarnings(as.numeric(dval[[nm]][idx]))
              replace(v < 0 & !is.na(v), is.na(v), FALSE)
            },
            col_rh = function() {
              nm <- mi$rh
              if (!nzchar(nm) || !(nm %in% names(dval))) {
                return(rep(FALSE, length(idx)))
              }
              v <- suppressWarnings(as.numeric(dval[[nm]][idx]))
              replace((v < 0 | v > 100) & !is.na(v), is.na(v), FALSE)
            },
            col_ws = function() {
              nm <- mi$ws
              if (!nzchar(nm) || !(nm %in% names(dval))) {
                return(rep(FALSE, length(idx)))
              }
              v <- suppressWarnings(as.numeric(dval[[nm]][idx]))
              replace(v < 0 & !is.na(v), is.na(v), FALSE)
            },
            col_solrad = function() {
              nm <- mi$solrad
              if (!nzchar(nm) || !(nm %in% names(dval))) {
                return(rep(FALSE, length(idx)))
              }
              v <- suppressWarnings(as.numeric(dval[[nm]][idx]))
              replace(v < 0 & !is.na(v), is.na(v), FALSE)
            },
            col_month = function() {
              nm <- mi$month
              if (!nzchar(nm) || !(nm %in% names(dval))) {
                return(rep(FALSE, length(idx)))
              }
              v <- suppressWarnings(as.integer(dval[[nm]][idx]))
              replace((v < 1 | v > 12) & !is.na(v), is.na(v), FALSE)
            },
            col_day = function() {
              nm <- mi$day
              if (!nzchar(nm) || !(nm %in% names(dval))) {
                return(rep(FALSE, length(idx)))
              }
              v <- suppressWarnings(as.integer(dval[[nm]][idx]))
              replace((v < 1 | v > 31) & !is.na(v), is.na(v), FALSE)
            },
            col_hour = function() {
              nm <- mi$hour
              if (!nzchar(nm) || !(nm %in% names(dval))) {
                return(rep(FALSE, length(idx)))
              }
              vv <- dval[[nm]][idx]
              vn <- suppressWarnings(as.integer(vv))
              ok_num <- !is.na(vn) & vn >= 0 & vn <= 23
              vhh <- extract_hour_from_string(vv)
              ok_hh <- !is.na(vhh) & vhh >= 0 & vhh <= 23
              !(ok_num | ok_hh)
            }
          )
          for (nm in names(crit_map)) issue_cols[[nm]] <- crit_map[[nm]]()
          # Summarize per station
          dt <- data.table::data.table(id = idv)
          for (nm in names(issue_cols)) dt[[nm]] <- issue_cols[[nm]]
          issue_names <- names(issue_cols)
          dt_counts <- dt[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = id, .SDcols = issue_names]
          dt_counts$total <- rowSums(dt_counts[, ..issue_names])
          data.table::setorder(dt_counts, -total)
          res$per_station <- head(dt_counts, 5)
        }
      }

      res
    })

    # --- UI feedback: badges + banner + station accordion ---
    badge_for <- function(status) {
      if (is.null(status)) {
        return(NULL)
      }
      if (identical(status, "ok")) {
        tags$span(class = "badge bg-success", tr("val_ok"))
      } else if (identical(status, "warn")) {
        tags$span(class = "badge bg-warning text-dark", tr("val_check"))
      } else if (identical(status, "error")) {
        tags$span(class = "badge bg-danger", tr("val_fix"))
      } else {
        NULL
      }
    }
    hint_block <- function(status, messages, examples = NULL) {
      if (identical(status, "ok")) {
        return(NULL)
      }
      msg <- paste(messages, collapse = "<br>")
      ex <- if (!is.null(examples) && length(examples)) tags$div(class = "small text-muted", paste(tr("val_examples"), paste(examples, collapse = ", "))) else NULL
      tags$div(class = "mt-1", HTML(msg), ex)
    }

    render_field_ui <- function(key) {
      renderUI({
        v <- validations()
        vv <- v[[key]]
        if (is.null(vv)) {
          return(NULL)
        }
        tagList(
          badge_for(vv$status),
          hint_block(vv$status, vv$messages, vv$examples)
        )
      })
    }

    output$v_col_datetime <- render_field_ui("col_datetime")
    output$v_col_id <- render_field_ui("col_id")
    output$v_col_date <- render_field_ui("col_date")
    output$v_col_time <- render_field_ui("col_time")
    output$v_col_year <- render_field_ui("col_year")
    output$v_col_month <- render_field_ui("col_month")
    output$v_col_day <- render_field_ui("col_day")
    output$v_col_hour <- render_field_ui("col_hour")
    output$v_col_temp <- render_field_ui("col_temp")
    output$v_col_rh <- render_field_ui("col_rh")
    output$v_col_ws <- render_field_ui("col_ws")
    output$v_col_rain <- render_field_ui("col_rain")
    output$v_col_solrad <- render_field_ui("col_solrad")
    output$v_manual_lat <- renderUI(NULL)
    output$v_manual_lon <- renderUI(NULL)

    # Consolidated banner for blocking issues
    output$mapping_alert <- renderUI({
      v <- validations()
      if (is.null(v)) {
        return(NULL)
      }
      # Collect errors
      errs <- c(
        if (!is.null(v$col_rain) && v$col_rain$status == "error") tr("val_rain_negative") else NULL,
        if (!is.null(v$col_rh) && v$col_rh$status == "error") tr("val_rh_range") else NULL,
        if (!is.null(v$col_ws) && v$col_ws$status == "error") tr("val_ws_negative") else NULL,
        if (!is.null(v$col_solrad) && v$col_solrad$status == "error") tr("val_solrad_negative") else NULL,
        if (!is.null(v$col_month) && v$col_month$status == "error") tr("val_month_range") else NULL,
        if (!is.null(v$col_day) && v$col_day$status == "error") tr("val_day_range") else NULL,
        if (!is.null(v$col_hour) && v$col_hour$status == "error") tr("val_hour_range") else NULL,
        if (!is.null(v$combined_date) && v$combined_date$status == "error") tr("val_date_invalid") else NULL
      )
      # Also readiness path check
      mi <- mapping_inputs()
      has_dt <- nzchar(mi$datetime)
      has_ymdh <- all(nzchar(mi$year), nzchar(mi$month), nzchar(mi$day), nzchar(mi$hour))
      has_met <- all(nzchar(mi$temp), nzchar(mi$rh), nzchar(mi$ws), nzchar(mi$rain))
      has_geo <- isTRUE(is.finite(input$manual_lat)) && isTRUE(is.finite(input$manual_lon))
      has_path <- (has_dt || has_ymdh)
      missing_bits <- c(
        if (!has_path) tr("val_missing_time_path") else NULL,
        if (!has_met) tr("val_missing_met") else NULL,
        if (!has_geo) tr("val_missing_geo") else NULL
      )

      items <- unique(c(errs, missing_bits))
      if (length(items) == 0) {
        return(NULL)
      }

      tags$div(
        class = "alert alert-danger mt-3",
        tags$div(tr("val_blocking_header")),
        tags$ul(
          class = "mb-0",
          lapply(items, function(x) tags$li(x))
        )
      )
    })

    # Per-station accordion (top 5 stations)
    output$station_accordion <- renderUI({
      v <- validations()
      if (is.null(v) || is.null(v$per_station)) {
        return(NULL)
      }
      d <- v$per_station
      if (nrow(d) == 0) {
        return(NULL)
      }
      ns <- session$ns
      acc_id <- ns("acc_station")
      # Build items for top 5 stations
      items <- lapply(seq_len(nrow(d)), function(i) {
        si <- d[i]
        title <- sprintf("%s — %s: %d", as.character(si$id), tr("val_station_issues"), as.integer(si$total))
        body <- tags$ul(
          lapply(setdiff(names(si), c("id", "total")), function(k) {
            if (is.numeric(si[[k]]) && si[[k]] > 0) tags$li(sprintf("%s: %d", k, as.integer(si[[k]]))) else NULL
          })
        )
        tags$div(
          class = "accordion-item",
          tags$h2(
            class = "accordion-header", id = paste0(acc_id, "-h-", i),
            tags$button(
              class = "accordion-button collapsed", `data-bs-toggle` = "collapse", `data-bs-target` = paste0("#", acc_id, "-b-", i),
              title
            )
          ),
          tags$div(
            id = paste0(acc_id, "-b-", i), class = "accordion-collapse collapse",
            tags$div(class = "accordion-body", body)
          )
        )
      })
      tags$div(class = "accordion mt-2", id = acc_id, items)
    })

    # --- Ready flag for gating Prepare/Run (now includes validity) ---
    valid_ok <- reactive({
      v <- validations()
      if (is.null(v)) {
        return(FALSE)
      }
      # Any critical error blocks
      crit <- c(
        v$col_rain$status, v$col_rh$status, v$col_ws$status, v$col_solrad$status,
        v$col_month$status, v$col_day$status, v$col_hour$status
      )
      if (any(identical(crit, "error"))) {
        return(FALSE)
      }
      if (!is.null(v$combined_date) && v$combined_date$status == "error") {
        return(FALSE)
      }
      TRUE
    })

    mapping_ready <- reactive({
      has_file <- length(cols()) > 0
      mi <- mapping_inputs()
      has_dt <- nzchar(mi$datetime)
      has_ymdh <- all(nzchar(mi$year), nzchar(mi$month), nzchar(mi$day), nzchar(mi$hour))
      has_met <- all(nzchar(mi$temp), nzchar(mi$rh), nzchar(mi$ws), nzchar(mi$rain))
      has_geo <- isTRUE(is.finite(input$manual_lat)) && isTRUE(is.finite(input$manual_lon))
      has_path <- (has_dt || has_ymdh)
      has_file && has_path && has_met && has_geo && isTRUE(valid_ok())
    })

    # Return reactives (unchanged API)
    return(list(
      col_datetime = reactive(input$col_datetime),
      col_id       = reactive(input$col_id),
      col_date     = reactive(input$col_date),
      col_time     = reactive(input$col_time),
      col_solrad   = reactive(input$col_solrad),
      col_year     = reactive(input$col_year),
      col_month    = reactive(input$col_month),
      col_day      = reactive(input$col_day),
      col_hour     = reactive(input$col_hour),
      col_temp     = reactive(input$col_temp),
      col_rh       = reactive(input$col_rh),
      col_ws       = reactive(input$col_ws),
      col_rain     = reactive(input$col_rain),
      manual_lat   = reactive(input$manual_lat),
      manual_lon   = reactive(input$manual_lon),
      ready        = mapping_ready
    ))
  })
}
