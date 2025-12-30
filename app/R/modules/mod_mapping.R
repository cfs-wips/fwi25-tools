
# ---- Mapping UI with client-side tooltips (Option A) ----

mod_mapping_ui <- function(id) {
  ns <- NS(id)
  
  # Simple “?” trigger for tooltips (keeps deps minimal in Shinylive)
  tip_icon <- function(title_id) {
    # A small badge that triggers the tooltip; accessible via tabindex
    bslib::tooltip(
      tags$span(class = "badge bg-light text-dark ms-1", "?", tabindex = "0"),
      uiOutput(title_id)  # tooltip body filled from server via tr("tt_*")
    )
  }
  
  tagList(
    # Section header (kept as before)
    h5(id = ns("lbl_column_mapping"), uiOutput(ns("lbl_column_mapping"))),
    
    # Fieldset wrapper (disabled until file upload)
    tags$fieldset(
      id = ns("mapping_fs"),  # for JS targeting
      role = "group",
      `aria-labelledby` = ns("lbl_column_mapping"),
      `aria-disabled`   = "true",
      
      # ---- Row 1 ----
      fluidRow(
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_datetime_text")),
            tip_icon(ns("tt_col_datetime"))
          ),
          selectInput(ns("col_datetime"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_datetime"))
        ),
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_id_text")),
            tip_icon(ns("tt_col_id"))
          ),
          selectInput(ns("col_id"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_id"))
        ),
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_date_text")),
            tip_icon(ns("tt_col_date"))
          ),
          selectInput(ns("col_date"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_date"))
        )
      ),
      
      # ---- Row 2 ----
      fluidRow(
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_time_text")),
            tip_icon(ns("tt_col_time"))
          ),
          selectInput(ns("col_time"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_time"))
        ),
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_manual_lat_text")),
            tip_icon(ns("tt_manual_lat"))
          ),
          numericInput(ns("manual_lat"), label = NULL, value = NULL, min = -90,  max = 90,  step = 0.0001),
          uiOutput(ns("v_manual_lat"))
        ),
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_manual_lon_text")),
            tip_icon(ns("tt_manual_lon"))
          ),
          numericInput(ns("manual_lon"), label = NULL, value = NULL, min = -180, max = 180, step = 0.0001),
          uiOutput(ns("v_manual_lon"))
        )
      ),
      
      # ---- Row 3 ----
      fluidRow(
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_year_text")),
            tip_icon(ns("tt_col_year"))
          ),
          selectInput(ns("col_year"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_year"))
        ),
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_month_text")),
            tip_icon(ns("tt_col_month"))
          ),
          selectInput(ns("col_month"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_month"))
        ),
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_day_text")),
            tip_icon(ns("tt_col_day"))
          ),
          selectInput(ns("col_day"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_day"))
        )
      ),
      
      # ---- Row 4 ----
      fluidRow(
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_hour_text")),
            tip_icon(ns("tt_col_hour"))
          ),
          selectInput(ns("col_hour"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_hour"))
        ),
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_temp_text")),
            tip_icon(ns("tt_col_temp"))
          ),
          selectInput(ns("col_temp"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_temp"))
        ),
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_rh_text")),
            tip_icon(ns("tt_col_rh"))
          ),
          selectInput(ns("col_rh"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_rh"))
        )
      ),
      
      # ---- Row 5 ----
      fluidRow(
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_ws_text")),
            tip_icon(ns("tt_col_ws"))
          ),
          selectInput(ns("col_ws"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_ws"))
        ),
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_rain_text")),
            tip_icon(ns("tt_col_rain"))
          ),
          selectInput(ns("col_rain"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_rain"))
        ),
        column(
          4,
          div(
            class = "form-label d-flex align-items-center gap-1",
            uiOutput(ns("lbl_col_solrad_text")),
            tip_icon(ns("tt_col_solrad"))
          ),
          selectInput(ns("col_solrad"), label = NULL, choices = "", selected = ""),
          uiOutput(ns("v_col_solrad"))
        )
      ),
      
      # Global banner + per-station accordion
      uiOutput(ns("mapping_alert")),
      uiOutput(ns("station_accordion"))
    )
  )
}


mod_mapping_server <- function(id, tr, lang, cols, df) {
  moduleServer(id, function(input, output, session) {
    
    # ---- Shinylive-safe helpers (atomic character everywhere) ----
    to_chr <- function(x) {
      if (is.null(x)) character(0) else unname(as.character(x))
    }
    safe_choices <- function(x) {
      vals <- to_chr(x)
      setNames(vals, vals)    # label=value (strings only)
    }
    safe_selected <- function(sel, choices_chr) {
      hit <- intersect(to_chr(sel), to_chr(choices_chr))
      if (length(hit)) hit[[1]] else NULL
    }
    
    # ---- Section heading ----
    output$lbl_column_mapping <- renderUI({
      label_with_help_rich(
        label_text = tr("column_mapping"),
        tip_text   = tr("tt_mapping_header"),
        popover_html = tr("mapping_help"),
        sr_label   = tr("column_mapping")
      )
    })
    
    last_cols <- reactiveVal(character())
    
    # ---- Disable/enable fieldset on file presence ----
    observe({
      session$sendCustomMessage("mappingSetDisabled", list(
        id       = session$ns("mapping_fs"),
        disabled = (length(cols()) == 0)
      ))
    }) |> bindEvent(cols())
    
    # ---- Label + tooltip text wiring (client-side) ----
    set_label <- function(out_id, key) {
      output[[out_id]] <- renderUI(tr(key))
    }
    # Label text (left of the "?")
    set_label("lbl_col_datetime_text", "col_datetime")
    set_label("lbl_col_id_text",       "col_id")
    set_label("lbl_col_date_text",     "col_date")
    set_label("lbl_col_time_text",     "col_time")
    set_label("lbl_manual_lat_text",   "lat_label")
    set_label("lbl_manual_lon_text",   "lon_label")
    set_label("lbl_col_year_text",     "col_year")
    set_label("lbl_col_month_text",    "col_month")
    set_label("lbl_col_day_text",      "col_day")
    set_label("lbl_col_hour_text",     "col_hour")
    set_label("lbl_col_temp_text",     "col_temp")
    set_label("lbl_col_rh_text",       "col_rh")
    set_label("lbl_col_ws_text",       "col_ws")
    set_label("lbl_col_rain_text",     "col_rain")
    set_label("lbl_col_solrad_text",   "col_solrad")
    
    # Tooltip bodies (content inside the popover)
    set_label("tt_col_datetime", "tt_col_datetime")
    set_label("tt_col_id",       "tt_col_id")
    set_label("tt_col_date",     "tt_col_date")
    set_label("tt_col_time",     "tt_col_time")
    set_label("tt_manual_lat",   "tt_manual_lat")
    set_label("tt_manual_lon",   "tt_manual_lon")
    set_label("tt_col_year",     "tt_col_year")
    set_label("tt_col_month",    "tt_col_month")
    set_label("tt_col_day",      "tt_col_day")
    set_label("tt_col_hour",     "tt_col_hour")
    set_label("tt_col_temp",     "tt_col_temp")
    set_label("tt_col_rh",       "tt_col_rh")
    set_label("tt_col_ws",       "tt_col_ws")
    set_label("tt_col_rain",     "tt_col_rain")
    set_label("tt_col_solrad",   "tt_col_solrad")
    
    # ---- Profiling and eligibility (unchanged, minor tidy) ----
    DT <- NULL
    
    escape <- function(x) gsub("([\\.^$(){}+?\\[\\]\\\\])", "\\\\\\1", x, perl = TRUE)
    find_col <- function(cols, keywords) {
      rx <- paste0("^(", paste(escape(keywords), collapse = "|"), ")$")
      m  <- cols[grepl(rx, cols, ignore.case = TRUE)]
      if (length(m) > 0) return(m[1])
      norm <- function(x) gsub("[^a-z0-9]+", "", tolower(trimws(x)))
      nc <- norm(cols); nk <- norm(keywords)
      idx <- which(nc %in% nk)
      if (length(idx) > 0) cols[idx[1]] else ""
    }
    
    profiler <- reactive({
      dval <- df()
      if (is.null(dval) || !length(dval)) return(NULL)
      suppressWarnings({ DT <<- data.table::as.data.table(dval) })
      n <- nrow(DT); if (n == 0) return(NULL)
      idx <- seq_len(min(5000L, n))
      
      profiles <- lapply(names(DT), function(nm) {
        v <- DT[[nm]][idx]
        is_num <- is.numeric(v)
        is_int <- is.integer(v)
        is_chr <- is.character(v) || is.factor(v)
        
        v_num       <- suppressWarnings(as.numeric(v))
        na_rate_num <- mean(is.na(v_num))
        non_na      <- !is.na(v_num)
        int_like    <- if (sum(non_na) > 0)
          all(abs(v_num[non_na] - round(v_num[non_na])) < .Machine$double.eps^0.5) else FALSE
        v_min <- suppressWarnings(min(v_num, na.rm = TRUE))
        v_max <- suppressWarnings(max(v_num, na.rm = TRUE))
        
        dt_rate <- NA_real_
        if (is_chr) {
          s <- v[!is.na(v)]
          if (length(s) > 0) {
            s <- s[seq_len(min(200L, length(s)))]
            orders <- c("Ymd HMS","Ymd HM","Ymd H","Ymd","mdY HMS","mdY HM","mdY","dmy HMS","dmy HM","dmy")
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
          int_like    = int_like,
          min = v_min, max = v_max,
          dt_rate = dt_rate,
          examples = examples
        )
      })
      names(profiles) <- names(DT)
      profiles
    })
    
    order_choices <- function(cc, eligible) {
      eligible <- intersect(eligible, cc)
      other    <- setdiff(cc, eligible)
      c("", eligible, other)
    }
    
    eligible_sets <- reactive({
      pr <- profiler(); if (is.null(pr)) return(list())
      nm <- names(pr)
      
      eligible_numeric <- nm[vapply(pr, function(p) p$is_num || (!p$is_num && p$na_rate_num < 0.1), logical(1))]
      eligible_integer <- nm[vapply(pr, function(p) p$is_int || (p$is_num && p$int_like) || (!p$is_num && p$na_rate_num < 0.1 && p$int_like), logical(1))]
      eligible_datetime <- nm[vapply(pr, function(p) {
        v <- DT[[p$name]]
        inherits(v, "POSIXt") || inherits(v, "Date") || (p$is_chr && !is.na(p$dt_rate) && p$dt_rate >= 0.6)
      }, logical(1))]
      
      list(numeric = eligible_numeric, integer = eligible_integer, datetime = eligible_datetime, profiles = pr)
    })
    
    # ---- Update choices when file changes (Shinylive-safe) ----
    observeEvent(cols(), {
      cc <- to_chr(cols())
      if (length(cc) == 0) {
        for (id_ in c("col_datetime","col_id","col_date","col_time","col_year","col_month","col_day",
                      "col_hour","col_temp","col_rh","col_ws","col_rain","col_solrad")) {
          updateSelectInput(session, id_, choices = safe_choices(""), selected = NULL)
        }
        return()
      }
      
      pr      <- eligible_sets()
      elig_dt <- to_chr(pr$datetime)
      elig_int<- to_chr(pr$integer)
      elig_num<- to_chr(pr$numeric)
      
      keep_or <- function(cur, pool, fallback) {
        pool_chr <- to_chr(pool); cur_chr <- to_chr(cur)
        if (nzchar(cur_chr) && cur_chr %in% pool_chr) cur_chr else to_chr(fallback)
      }
      
      # Datetime
      ch <- safe_choices(order_choices(cc, elig_dt))
      sel <- safe_selected(
        keep_or(input$col_datetime, cc,
                find_col(cc, c("datetime","timestamp","Time and Date","Date and Time",
                               "Date & Time","Time & Date","Datetime","DateTime"))),
        names(ch)
      )
      updateSelectInput(session, "col_datetime", choices = ch, selected = sel)
      
      # ID
      ch <- safe_choices(c("", cc))
      sel <- safe_selected(
        keep_or(input$col_id, cc, find_col(cc, c("Station Name","Station","ID","WMO","AES"))),
        names(ch)
      )
      updateSelectInput(session, "col_id", choices = ch, selected = sel)
      
      # Date
      ch <- safe_choices(order_choices(cc, c(elig_dt, elig_int)))
      sel <- safe_selected(
        keep_or(input$col_date, cc, find_col(cc, c("date","obs_date","dt","day_date"))),
        names(ch)
      )
      updateSelectInput(session, "col_date", choices = ch, selected = sel)
      
      # Time
      time_hint <- cc[grepl("(^hhmm$|^hh:mm$|time|obs_time|hour_min)", cc, ignore.case = TRUE)]
      ch <- safe_choices(order_choices(cc, c(elig_dt, to_chr(time_hint))))
      sel <- safe_selected(
        keep_or(input$col_time, cc, find_col(cc, c("time","obs_time","hour_min","hhmm","hh:mm"))),
        names(ch)
      )
      updateSelectInput(session, "col_time", choices = ch, selected = sel)
      
      # Parts
      ch <- safe_choices(order_choices(cc, elig_int))
      updateSelectInput(session, "col_year",
                        choices = ch,
                        selected = safe_selected(keep_or(input$col_year,  cc, find_col(cc, c("year","yr","y"))), names(ch))
      )
      updateSelectInput(session, "col_month",
                        choices = ch,
                        selected = safe_selected(keep_or(input$col_month, cc, find_col(cc, c("month","mon","m"))), names(ch))
      )
      updateSelectInput(session, "col_day",
                        choices = ch,
                        selected = safe_selected(keep_or(input$col_day,   cc, find_col(cc, c("day","dy","d"))), names(ch))
      )
      updateSelectInput(session, "col_hour",
                        choices = ch,
                        selected = safe_selected(keep_or(input$col_hour,  cc, find_col(cc, c("hour","hr","h"))), names(ch))
      )
      
      # Met
      ch_num <- safe_choices(order_choices(cc, elig_num))
      updateSelectInput(session, "col_temp",
                        choices = ch_num,
                        selected = safe_selected(keep_or(input$col_temp, cc, find_col(cc, c("temp","temperature","t"))), names(ch_num))
      )
      updateSelectInput(session, "col_rh",
                        choices = ch_num,
                        selected = safe_selected(keep_or(input$col_rh,   cc, find_col(cc, c("rh","humidity","relative_humidity","relative humidity"))), names(ch_num))
      )
      updateSelectInput(session, "col_ws",
                        choices = ch_num,
                        selected = safe_selected(keep_or(input$col_ws,   cc, find_col(cc, c("wind","ws","windspeed","wind_speed","wind.speed","wind speed","wspd","wnd","wndspd"))), names(ch_num))
      )
      updateSelectInput(session, "col_rain",
                        choices = ch_num,
                        selected = safe_selected(keep_or(input$col_rain, cc, find_col(cc, c("rain","precip","prec","precip_mm","prec_mm","rain_mm","rf","rn","rn_1"))), names(ch_num))
      )
      updateSelectInput(session, "col_solrad",
                        choices = ch_num,
                        selected = safe_selected(keep_or(input$col_solrad, cc, find_col(cc, c("solar","solrad","solar_radiation","shortwave","srad","rsds","swdown","rad","radiation","rad_wm2"))), names(ch_num))
      )
      
      # Seed lat/lon defaults
      is_new_upload <- !identical(last_cols(), cc)
      last_cols(cc)
      
      dval <- df(); if (is.null(dval) || !length(dval)) return()
      dn <- names(dval)
      lat_col <- find_col(dn, c("lat","latitude"))
      lon_col <- find_col(dn, c("lon","long","longitude"))
      lat_def <- suppressWarnings(as.numeric(if (nzchar(lat_col)) dval[[lat_col]][1] else NA))
      lon_def <- suppressWarnings(as.numeric(if (nzchar(lon_col)) dval[[lon_col]][1] else NA))
      if (!is.finite(lat_def)) lat_def <- 55
      if (!is.finite(lon_def)) lon_def <- -120
      
      if (is_new_upload || !is.finite(input$manual_lat) || !is.finite(input$manual_lon)) {
        updateNumericInput(session, "manual_lat", value = lat_def)
        updateNumericInput(session, "manual_lon", value = lon_def)
      }
    })
    
    # ---- Validation utilities (unchanged) ----
    get_sample <- function(nm) {
      if (!nzchar(nm) || is.null(DT)) return(NULL)
      v <- DT[[nm]]; if (is.null(v)) return(NULL)
      n <- length(v); if (n == 0) return(NULL)
      v[seq_len(min(5000L, n))]
    }
    is_finite_num <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      is.finite(x)
    }
    extract_hour_from_string <- function(x) {
      x <- as.character(x)
      m1 <- grepl("^\\s*([01]?\\d|2[0-3]):[0-5]\\d\\s*$", x)
      if (any(m1)) {
        hh <- as.integer(sub("^\\s*([0-9]{1,2}).*$", "\\1", x[m1]))
        out <- rep(NA_integer_, length(x)); out[m1] <- hh; return(out)
      }
      m2 <- grepl("^\\s*([01]?\\d|2[0-3])[0-5]\\d\\s*$", x)
      if (any(m2)) {
        hh <- as.integer(sub("^\\s*([0-9]{1,2}).*$", "\\1", x[m2]))
        out <- rep(NA_integer_, length(x)); out[m2] <- hh; return(out)
      }
      rep(NA_integer_, length(x))
    }
    
    validate_field <- function(field, nm) {
      v <- get_sample(nm)
      if (is.null(v)) {
        return(list(status = "ok", messages = character(), examples = character(), per_station = NULL))
      }
      msgs <- character(); status <- "ok"; examples <- character(); per_station <- NULL
      add_error <- function(msg) { status <<- "error"; msgs <<- c(msgs, msg) }
      add_warn  <- function(msg) { if (status != "error") status <<- "warn"; msgs <<- c(msgs, msg) }
      
      if (field %in% c("col_temp","col_rh","col_ws","col_rain","col_solrad")) {
        vn <- suppressWarnings(as.numeric(v)); non_na <- !is.na(vn)
        if (sum(non_na) == 0) {
          add_error(tr("val_no_numeric"))
        } else {
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
      
      if (field %in% c("col_year","col_month","col_day","col_hour")) {
        if (field == "col_hour") {
          vn <- suppressWarnings(as.integer(v)); non_na_num <- !is.na(vn)
          vhh <- extract_hour_from_string(v); non_na_hh <- !is.na(vhh)
          ok_num <- sum(non_na_num) > 0 && all(vn[non_na_num] >= 0 & vn[non_na_num] <= 23)
          ok_hh  <- sum(non_na_hh)  > 0 && all(vhh[non_na_hh] >= 0 & vhh[non_na_hh] <= 23)
          if (!(ok_num || ok_hh)) add_error(tr("val_hour_range"))
          else if (ok_hh && !ok_num) add_warn(tr("val_hour_hhmm"))
          examples <- as.character(utils::head(v[non_na_num | non_na_hh], 3))
        } else if (field == "col_month") {
          vn <- suppressWarnings(as.integer(v)); non_na <- !is.na(vn)
          if (sum(non_na) == 0 || any(vn[non_na] < 1 | vn[non_na] > 12)) add_error(tr("val_month_range"))
          examples <- as.character(utils::head(v[non_na], 3))
        } else if (field == "col_day") {
          vn <- suppressWarnings(as.integer(v)); non_na <- !is.na(vn)
          if (sum(non_na) == 0 || any(vn[non_na] < 1 | vn[non_na] > 31)) add_error(tr("val_day_range"))
          examples <- as.character(utils::head(v[non_na], 3))
        } else if (field == "col_year") {
          vn <- suppressWarnings(as.integer(v)); non_na <- !is.na(vn)
          if (sum(non_na) == 0) add_error(tr("val_year_missing"))
          if (any(vn[non_na] < 1900 | vn[non_na] > 2100)) add_warn(tr("val_year_outside"))
          examples <- as.character(utils::head(v[non_na], 3))
        }
      }
      
      if (field == "col_datetime") {
        if (!(inherits(v, "POSIXt") || inherits(v, "Date"))) {
          vv <- as.character(v); vv <- vv[!is.na(vv)]
          if (length(vv) > 0) {
            vv <- vv[seq_len(min(200L, length(vv)))]
            orders <- c("Ymd HMS","Ymd HM","Ymd H","Ymd","mdY HMS","mdY HM","mdY","dmy HMS","dmy HM","dmy")
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
      
      list(status = status, messages = msgs, examples = examples, per_station = per_station)
    }
    
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
    
    validations <- reactive({
      mi <- mapping_inputs_db()
      res <- list()
      fields <- c("col_datetime","col_id","col_date","col_time","col_year","col_month","col_day",
                  "col_hour","col_temp","col_rh","col_ws","col_rain","col_solrad")
      mapped <- c(mi$datetime, mi$id, mi$date, mi$time, mi$year, mi$month, mi$day, mi$hour,
                  mi$temp, mi$rh, mi$ws, mi$rain, mi$solrad)
      names(mapped) <- fields
      for (k in fields) res[[k]] <- validate_field(k, mapped[[k]])
      
      has_dt   <- nzchar(mi$datetime)
      has_ymdh <- all(nzchar(mi$year), nzchar(mi$month), nzchar(mi$day), nzchar(mi$hour))
      if (has_dt && has_ymdh) {
        res$combined_priority <- list(status = "warn", messages = tr("val_both_paths"),
                                      examples = character(), per_station = NULL)
      }
      
      if (all(nzchar(mi$year), nzchar(mi$month), nzchar(mi$day))) {
        y <- suppressWarnings(as.integer(get_sample(mi$year)))
        m <- suppressWarnings(as.integer(get_sample(mi$month)))
        d <- suppressWarnings(as.integer(get_sample(mi$day)))
        len <- min(length(y), length(m), length(d))
        if (len > 0) {
          y <- y[seq_len(len)]; m <- m[seq_len(len)]; d <- d[seq_len(len)]
          invalid <- logical(len)
          for (i in seq_len(len)) {
            if (is.na(y[i]) || is.na(m[i]) || is.na(d[i])) invalid[i] <- TRUE
            else {
              suppressWarnings({
                dd <- try(as.Date(sprintf("%04d-%02d-%02d", y[i], m[i], d[i])), silent = TRUE)
                invalid[i] <- inherits(dd, "try-error") || is.na(dd)
              })
            }
          }
          if (any(invalid)) {
            res$combined_date <- list(
              status   = "error",
              messages = tr("val_date_invalid"),
              examples = as.character(utils::head(paste(y[invalid], m[invalid], d[invalid], sep = "-"), 3L)),
              per_station = NULL
            )
          }
        }
      }
      
      # Per-station summaries
      id_nm <- mi$id
      if (nzchar(id_nm)) {
        dval <- df()
        if (!is.null(dval) && length(dval) && id_nm %in% names(dval)) {
          idx <- seq_len(min(5000L, nrow(dval)))
          idv <- as.character(dval[[id_nm]][idx])
          issue_cols <- list()
          crit_map <- list(
            col_rain = function() {
              nm <- mi$rain; if (!nzchar(nm) || !(nm %in% names(dval))) return(rep(FALSE, length(idx)))
              v <- suppressWarnings(as.numeric(dval[[nm]][idx]))
              replace(v < 0 & !is.na(v), is.na(v), FALSE)
            },
            col_rh = function() {
              nm <- mi$rh; if (!nzchar(nm) || !(nm %in% names(dval))) return(rep(FALSE, length(idx)))
              v <- suppressWarnings(as.numeric(dval[[nm]][idx]))
              replace((v < 0 | v > 100) & !is.na(v), is.na(v), FALSE)
            },
            col_ws = function() {
              nm <- mi$ws; if (!nzchar(nm) || !(nm %in% names(dval))) return(rep(FALSE, length(idx)))
              v <- suppressWarnings(as.numeric(dval[[nm]][idx]))
              replace(v < 0 & !is.na(v), is.na(v), FALSE)
            },
            col_solrad = function() {
              nm <- mi$solrad; if (!nzchar(nm) || !(nm %in% names(dval))) return(rep(FALSE, length(idx)))
              v <- suppressWarnings(as.numeric(dval[[nm]][idx]))
              replace(v < 0 & !is.na(v), is.na(v), FALSE)
            },
            col_month = function() {
              nm <- mi$month; if (!nzchar(nm) || !(nm %in% names(dval))) return(rep(FALSE, length(idx)))
              v <- suppressWarnings(as.integer(dval[[nm]][idx]))
              replace((v < 1 | v > 12) & !is.na(v), is.na(v), FALSE)
            },
            col_day = function() {
              nm <- mi$day; if (!nzchar(nm) || !(nm %in% names(dval))) return(rep(FALSE, length(idx)))
              v <- suppressWarnings(as.integer(dval[[nm]][idx]))
              replace((v < 1 | v > 31) & !is.na(v), is.na(v), FALSE)
            },
            col_hour = function() {
              nm <- mi$hour; if (!nzchar(nm) || !(nm %in% names(dval))) return(rep(FALSE, length(idx)))
              vv <- dval[[nm]][idx]
              vn <- suppressWarnings(as.integer(vv))
              ok_num <- !is.na(vn) & vn >= 0 & vn <= 23
              vhh <- extract_hour_from_string(vv)
              ok_hh <- !is.na(vhh) & vhh >= 0 & vhh <= 23
              !(ok_num | ok_hh)
            }
          )
          for (nm in names(crit_map)) issue_cols[[nm]] <- crit_map[[nm]]()
          dt <- data.table::data.table(id = idv)
          for (nm in names(issue_cols)) dt[[nm]] <- issue_cols[[nm]]
          issue_names <- names(issue_cols)
          dt_counts <- dt[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = id, .SDcols = issue_names]
          dt_counts$total <- rowSums(dt_counts[, ..issue_names])
          data.table::setorder(dt_counts, -total)
          validations_per_station <- head(dt_counts, 5)
          res$per_station <- validations_per_station
        }
      }
      res
    })
    
    # ---- UI feedback (badges, banner, accordion) ----
    badge_for <- function(status) {
      if (is.null(status)) return(NULL)
      if (identical(status, "ok"))     tags$span(class = "badge bg-success",              tr("val_ok"))
      else if (identical(status, "warn"))  tags$span(class = "badge bg-warning text-dark", tr("val_check"))
      else if (identical(status, "error")) tags$span(class = "badge bg-danger",            tr("val_fix"))
      else NULL
    }
    hint_block <- function(status, messages, examples = NULL) {
      if (identical(status, "ok")) return(NULL)
      msg <- paste(messages, collapse = "<br>")
      ex  <- if (!is.null(examples) && length(examples))
        tags$div(class = "small text-muted", paste(tr("val_examples"), paste(examples, collapse = ", ")))
      else NULL
      tags$div(class = "mt-1", HTML(msg), ex)
    }
    render_field_ui <- function(key) {
      renderUI({
        v <- validations(); vv <- v[[key]]
        if (is.null(vv)) return(NULL)
        tagList(badge_for(vv$status), hint_block(vv$status, vv$messages, vv$examples))
      })
    }
    
    output$v_col_datetime <- render_field_ui("col_datetime")
    output$v_col_id       <- render_field_ui("col_id")
    output$v_col_date     <- render_field_ui("col_date")
    output$v_col_time     <- render_field_ui("col_time")
    output$v_col_year     <- render_field_ui("col_year")
    output$v_col_month    <- render_field_ui("col_month")
    output$v_col_day      <- render_field_ui("col_day")
    output$v_col_hour     <- render_field_ui("col_hour")
    output$v_col_temp     <- render_field_ui("col_temp")
    output$v_col_rh       <- render_field_ui("col_rh")
    output$v_col_ws       <- render_field_ui("col_ws")
    output$v_col_rain     <- render_field_ui("col_rain")
    output$v_col_solrad   <- render_field_ui("col_solrad")
    output$v_manual_lat   <- renderUI(NULL)
    output$v_manual_lon   <- renderUI(NULL)
    
    output$mapping_alert <- renderUI({
      v <- validations(); if (is.null(v)) return(NULL)
      errs <- c(
        if (!is.null(v$col_rain)   && v$col_rain$status   == "error") tr("val_rain_negative") else NULL,
        if (!is.null(v$col_rh)     && v$col_rh$status     == "error") tr("val_rh_range") else NULL,
        if (!is.null(v$col_ws)     && v$col_ws$status     == "error") tr("val_ws_negative") else NULL,
        if (!is.null(v$col_solrad) && v$col_solrad$status == "error") tr("val_solrad_negative") else NULL,
        if (!is.null(v$col_month)  && v$col_month$status  == "error") tr("val_month_range") else NULL,
        if (!is.null(v$col_day)    && v$col_day$status    == "error") tr("val_day_range") else NULL,
        if (!is.null(v$col_hour)   && v$col_hour$status   == "error") tr("val_hour_range") else NULL,
        if (!is.null(v$combined_date) && v$combined_date$status == "error") tr("val_date_invalid") else NULL
      )
      mi <- mapping_inputs()
      has_dt   <- nzchar(mi$datetime)
      has_ymdh <- all(nzchar(mi$year), nzchar(mi$month), nzchar(mi$day), nzchar(mi$hour))
      has_met  <- all(nzchar(mi$temp), nzchar(mi$rh), nzchar(mi$ws), nzchar(mi$rain))
      has_geo  <- isTRUE(is.finite(input$manual_lat)) && isTRUE(is.finite(input$manual_lon))
      has_path <- (has_dt || has_ymdh)
      missing_bits <- c(
        if (!has_path) tr("val_missing_time_path") else NULL,
        if (!has_met)  tr("val_missing_met") else NULL,
        if (!has_geo)  tr("val_missing_geo") else NULL
      )
      items <- unique(c(errs, missing_bits))
      if (length(items) == 0) return(NULL)
      tags$div(
        class = "alert alert-danger mt-3",
        tags$div(tr("val_blocking_header")),
        tags$ul(class = "mb-0", lapply(items, function(x) tags$li(x)))
      )
    })
    
    output$station_accordion <- renderUI({
      v <- validations(); if (is.null(v) || is.null(v$per_station)) return(NULL)
      d <- v$per_station; if (nrow(d) == 0) return(NULL)
      ns <- session$ns; acc_id <- ns("acc_station")
      items <- lapply(seq_len(nrow(d)), function(i) {
        si <- d[i]
        title <- sprintf("%s — %s: %d", as.character(si$id), tr("val_station_issues"), as.integer(si$total))
        body <- tags$ul(lapply(setdiff(names(si), c("id","total")), function(k) {
          if (is.numeric(si[[k]]) && si[[k]] > 0) tags$li(sprintf("%s: %d", k, as.integer(si[[k]]))) else NULL
        }))
        tags$div(
          class = "accordion-item",
          tags$h2(class = "accordion-header", id = paste0(acc_id, "-h-", i),
                  tags$button(class = "accordion-button collapsed",
                              `data-bs-toggle` = "collapse",
                              `data-bs-target` = paste0("#", acc_id, "-b-", i),
                              title
                  )
          ),
          tags$div(id = paste0(acc_id, "-b-", i), class = "accordion-collapse collapse",
                   tags$div(class = "accordion-body", body)
          )
        )
      })
      tags$div(class = "accordion mt-2", id = acc_id, items)
    })
    
    # ---- Ready flag ----
    valid_ok <- reactive({
      v <- validations(); if (is.null(v)) return(FALSE)
      crit <- c(v$col_rain$status, v$col_rh$status, v$col_ws$status, v$col_solrad$status,
                v$col_month$status, v$col_day$status, v$col_hour$status)
      if (any(identical(crit, "error"))) return(FALSE)
      if (!is.null(v$combined_date) && v$combined_date$status == "error") return(FALSE)
      TRUE
    })
    
    mapping_ready <- reactive({
      has_file <- length(cols()) > 0
      mi <- mapping_inputs()
      has_dt   <- nzchar(mi$datetime)
      has_ymdh <- all(nzchar(mi$year), nzchar(mi$month), nzchar(mi$day), nzchar(mi$hour))
      has_met  <- all(nzchar(mi$temp), nzchar(mi$rh), nzchar(mi$ws), nzchar(mi$rain))
      has_geo  <- isTRUE(is.finite(input$manual_lat)) && isTRUE(is.finite(input$manual_lon))
      has_path <- (has_dt || has_ymdh)
      has_file && has_path && has_met && has_geo && isTRUE(valid_ok())
    })
    
    # ---- Return reactives (API unchanged) ----
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
