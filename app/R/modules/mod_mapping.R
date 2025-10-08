# R/modules/mod_mapping.R
mod_mapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(id = ns("lbl_column_mapping"), textOutput(ns("lbl_column_mapping"))),
    tags$div(role = "group", `aria-labelledby` = ns("lbl_column_mapping"), tags$div(class = "mapping-block", uiOutput(ns("mapping_ui"))))
  )
}

mod_mapping_server <- function(id, tr, cols, df) {
  moduleServer(id, function(input, output, session) {
    output$lbl_column_mapping <- renderText(tr("column_mapping"))

    find_col <- function(cols, keywords) {
      rx <- paste0("^(", paste(keywords, collapse = "|"), ")$")
      m <- cols[grepl(rx, cols, ignore.case = TRUE)]
      if (length(m) > 0) m[1] else ""
    }

    output$mapping_ui <- renderUI({
      has_file <- length(cols()) > 0
      cc <- cols()
      pick_sel <- function(val) if (has_file) val else ""
      fc <- function(x, include_blank = TRUE) if (include_blank) c("", x) else x
      disabled_attr <- if (!has_file) NA else NULL
      aria_state <- if (!has_file) "true" else "false"
      tagList(
        tags$fieldset(
          class = "mapping-ui-fieldset",
          disabled = disabled_attr,
          `aria-disabled` = aria_state,
          helpText(tr("mapping_help")),
          selectInput(session$ns("col_datetime"), tr("col_datetime"), choices = fc(cc), selected = pick_sel(find_col(cc, c("datetime", "timestamp")))),
          fluidRow(
            column(3, selectInput(session$ns("col_year"), tr("col_year"), choices = fc(cc), selected = pick_sel(find_col(cc, c("year", "yr", "y"))))),
            column(3, selectInput(session$ns("col_month"), tr("col_month"), choices = fc(cc), selected = pick_sel(find_col(cc, c("month", "mon", "m"))))),
            column(3, selectInput(session$ns("col_day"), tr("col_day"), choices = fc(cc), selected = pick_sel(find_col(cc, c("day", "dy", "d"))))),
            column(3, selectInput(session$ns("col_hour"), tr("col_hour"), choices = fc(cc), selected = pick_sel(find_col(cc, c("hour", "hr", "h")))))
          ),
          fluidRow(
            column(3, selectInput(session$ns("col_temp"), tr("col_temp"), choices = fc(cc), selected = pick_sel(find_col(cc, c("temp","temperature","t"))))),
            column(3, selectInput(session$ns("col_rh"),   tr("col_rh"),   choices = fc(cc), selected = pick_sel(find_col(cc, c("rh","relative humidity","relative.humidity","relative_humidity","humidity"))))),
            column(3, selectInput(session$ns("col_ws"),   tr("col_ws"),   choices = fc(cc), selected = pick_sel(find_col(cc, c("ws","windspeed","wind_speed","wind.speed","wind speed","Wspd","Wnd","Wndspd"))))),
            column(3, selectInput(session$ns("col_rain"), tr("col_rain"), choices = fc(cc), selected = pick_sel(find_col(cc, c("rain","prec","precip","precip_mm","prec_mm","rain_mm","rf","rn")))))
          ),
          fluidRow(
            column(6, numericInput(session$ns("manual_lat"), tr("lat_label"), value = 55, min = -90, max = 90, step = 0.0001)),
            column(6, numericInput(session$ns("manual_lon"), tr("lon_label"), value = -120, min = -180, max = 180, step = 0.0001))
          )
        )
      )
    })

    # Prefill manual lat/lon from first row if present
    observeEvent(cols(), ignoreInit = TRUE, {
      has_file <- length(cols()) > 0
      df <- df()
      cc <- names(df)
      lat_col <- find_col(cc, c("lat", "latitude"))
      lon_col <- find_col(cc, c("lon", "long", "longitude"))

      lat_default <- suppressWarnings(as.numeric(if (nzchar(lat_col)) df[[lat_col]][1] else NA))
      lon_default <- suppressWarnings(as.numeric(if (nzchar(lon_col)) df[[lon_col]][1] else NA))

      if (!is.finite(lat_default)) lat_default <- 55
      if (!is.finite(lon_default)) lon_default <- -120

      updateNumericInput(session, "manual_lat", value = lat_default)
      updateNumericInput(session, "manual_lon", value = lon_default)
    })

    return(list(
      col_datetime = reactive(input$col_datetime),
      col_year = reactive(input$col_year),
      col_month = reactive(input$col_month),
      col_day = reactive(input$col_day),
      col_hour = reactive(input$col_hour),
      col_temp = reactive(input$col_temp),
      col_rh = reactive(input$col_rh),
      col_ws = reactive(input$col_ws),
      col_rain = reactive(input$col_rain),
      manual_lat = reactive(input$manual_lat),
      manual_lon = reactive(input$manual_lon)
    ))
  })
}
