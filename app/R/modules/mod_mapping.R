# R/modules/mod_mapping.R
mod_mapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5(id = ns("lbl_column_mapping"), uiOutput(ns("lbl_column_mapping"))),
    tags$fieldset(
      id = ns("mapping_fs"), # for JS targeting
      role = "group",
      `aria-labelledby` = ns("lbl_column_mapping"),
      `aria-disabled` = "true", # start disabled until file upload
      # Static inputs (values persist across language toggles)
      fluidRow(
        column(6, selectInput(ns("col_datetime"), label = NULL, choices = "", selected = "")),
        column(6, selectInput(ns("col_id"), label = NULL, choices = "", selected = ""))
      ),
      fluidRow(
        column(6, selectInput(ns("col_date"), label = NULL, choices = "", selected = "")),
        column(6, selectInput(ns("col_time"), label = NULL, choices = "", selected = ""))
      ),
      fluidRow(
        column(3, selectInput(ns("col_year"), label = NULL, choices = "", selected = "")),
        column(3, selectInput(ns("col_month"), label = NULL, choices = "", selected = "")),
        column(3, selectInput(ns("col_day"), label = NULL, choices = "", selected = "")),
        column(3, selectInput(ns("col_hour"), label = NULL, choices = "", selected = ""))
      ),
      fluidRow(
        column(3, selectInput(ns("col_temp"), label = NULL, choices = "", selected = "")),
        column(3, selectInput(ns("col_rh"), label = NULL, choices = "", selected = "")),
        column(3, selectInput(ns("col_ws"), label = NULL, choices = "", selected = "")),
        column(3, selectInput(ns("col_rain"), label = NULL, choices = "", selected = ""))
      ),
      fluidRow(
        column(6, selectInput(ns("col_solrad"), label = NULL, choices = "", selected = ""))
      ),
      fluidRow(
        column(6, numericInput(ns("manual_lat"),
          label = NULL, value = NULL,
          min = -90, max = 90, step = 0.0001
        )),
        column(6, numericInput(ns("manual_lon"),
          label = NULL, value = NULL,
          min = -180, max = 180, step = 0.0001
        ))
      )
    )
  )
}


mod_mapping_server <- function(id, tr, lang, cols, df) {
  moduleServer(id, function(input, output, session) {
    # --- Section heading ---
    output$lbl_column_mapping <- renderUI({
      label_with_help_rich(
        label_text   = tr("column_mapping"),
        tip_text     = tr("tt_mapping_header"),
        popover_html = tr("mapping_help"),
        sr_label     = tr("column_mapping")
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

    # --- Helper functions ---
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

    find_col <- function(cols, keywords) {
      escape <- function(x) gsub("([\\.^$(){}+*?|\\[\\]\\\\])", "\\\\\\1", x, perl = TRUE)
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

    # --- Update choices when file changes ---
    observeEvent(cols(), {
      cc <- cols()
      
      choices <- c("", cc)
      keep_or <- function(cur, pool, fallback) if (nzchar(cur) && cur %in% pool) cur else fallback

      updateSelectInput(session, "col_datetime",
        choices = choices,
        selected = keep_or(input$col_datetime, cc, find_col(cc, c("datetime", "timestamp", "Time and Date", "Date and Time", "Date & Time", "Time & Date", "Datetime", "DateTime")))
      )
      updateSelectInput(session, "col_id",
        choices = choices,
        selected = keep_or(input$col_id, cc, find_col(cc, c("Station Name", "Station", "ID", "WMO", "AES")))
      )
      updateSelectInput(session, "col_date",
        choices = choices,
        selected = keep_or(input$col_date, cc, find_col(cc, c("date", "obs_date", "dt", "day_date")))
      )
      updateSelectInput(session, "col_time",
        choices = choices,
        selected = keep_or(input$col_time, cc, find_col(cc, c("time", "obs_time", "hour_min", "hhmm", "hh:mm")))
      )
      updateSelectInput(session, "col_year",
        choices = choices,
        selected = keep_or(input$col_year, cc, find_col(cc, c("year", "yr", "y")))
      )
      updateSelectInput(session, "col_month",
        choices = choices,
        selected = keep_or(input$col_month, cc, find_col(cc, c("month", "mon", "m")))
      )
      updateSelectInput(session, "col_day",
        choices = choices,
        selected = keep_or(input$col_day, cc, find_col(cc, c("day", "dy", "d")))
      )
      updateSelectInput(session, "col_hour",
        choices = choices,
        selected = keep_or(input$col_hour, cc, find_col(cc, c("hour", "hr", "h")))
      )
      updateSelectInput(session, "col_temp",
        choices = choices,
        selected = keep_or(input$col_temp, cc, find_col(cc, c("temp", "temperature", "t")))
      )
      updateSelectInput(session, "col_rh",
        choices = choices,
        selected = keep_or(input$col_rh, cc, find_col(cc, c("rh", "humidity", "relative_humidity", "relative humidity")))
      )
      updateSelectInput(session, "col_ws",
        choices = choices,
        selected = keep_or(input$col_ws, cc, find_col(cc, c("wind","ws", "windspeed", "wind_speed", "wind.speed", "wind speed", "wspd", "wnd", "wndspd")))
      )
      updateSelectInput(session, "col_rain",
        choices = choices,
        selected = keep_or(input$col_rain, cc, find_col(cc, c("rain", "precip", "prec", "precip_mm", "prec_mm", "rain_mm", "rf", "rn","rn_1")))
      )
      updateSelectInput(session, "col_solrad",
        choices = choices,
        selected = keep_or(input$col_solrad, cc, find_col(cc, c("solar", "solrad", "solar_radiation", "shortwave", "srad", "rsds", "swdown", "rad", "radiation", "rad_wm2")))
      )
      
      is_new_upload <- !identical(last_cols(), cc)
      last_cols(cc)
      
      
      dval <- df()
      if (is.null(dval) || !length(dval)) return()   # guard for early calls
      
      dn <- names(dval)
      lat_col <- find_col(dn, c("lat", "latitude"))
      lon_col <- find_col(dn, c("lon", "long", "longitude"))
      
      lat_def <- suppressWarnings(as.numeric(if (nzchar(lat_col)) dval[[lat_col]][1] else NA))
      lon_def <- suppressWarnings(as.numeric(if (nzchar(lon_col)) dval[[lon_col]][1] else NA))
      if (!is.finite(lat_def)) lat_def <- 55
      if (!is.finite(lon_def)) lon_def <- -120
      
      # NEW: reseed lat/lon if it’s a new upload OR inputs are non-finite (cleared/blank)
      if (is_new_upload || !is.finite(input$manual_lat) || !is.finite(input$manual_lon)) {
        updateNumericInput(session, "manual_lat", value = lat_def)
        updateNumericInput(session, "manual_lon", value = lon_def)
      }
    })
    
    
    

    # --- Update labels on language toggle ---
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

    # --- Ready flag for gating Prepare/Run ---
    mapping_ready <- reactive({
      has_file <- length(cols()) > 0
      has_dt <- nzchar(input$col_datetime)
      has_ymdh <- all(
        nzchar(input$col_year), nzchar(input$col_month),
        nzchar(input$col_day), nzchar(input$col_hour)
      )
      has_met <- all(
        nzchar(input$col_temp), nzchar(input$col_rh),
        nzchar(input$col_ws), nzchar(input$col_rain)
      )
      has_geo <- isTRUE(is.finite(input$manual_lat)) && isTRUE(is.finite(input$manual_lon))
      has_file && (has_dt || has_ymdh) && has_met && has_geo
    })

    # Return reactives
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
