# R/modules/mod_filter.R

mod_filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(id = ns("lbl_filter"), uiOutput(ns("lbl_filter"))),
    
    div(
      role = "group",
      `aria-labelledby` = ns("lbl_start_date"),
      uiOutput(ns("lbl_start_date")),
      dateInput(ns("start_date"), label = NULL, value = NA)
    ),
    
    tags$p(
      id = ns("help_drop_rows"),
      class = "help-block",
      textOutput(ns("txt_drop_rows_help"))
    )
  )
}

mod_filter_server <- function(id, tr, raw_file, mapping) {
  moduleServer(id, function(input, output, session) {
    
    output$lbl_filter <- renderUI({
      label_with_help(tr("filter"), tr("drop_rows_help"))
    })
    
    output$lbl_start_date <- renderUI({
      tags$label(
        `for` = session$ns("start_date"),
        label_with_help(tr("drop_rows_prior"), tr("tt_start_date"))
      )
    })
    
    output$txt_drop_rows_help <- renderText(tr("drop_rows_help"))
    
    # Robust column finder
    find_col <- function(cols, keywords) {
      rx <- paste0("^(", paste(keywords, collapse = "|"), ")$")
      m  <- cols[grepl(rx, cols, ignore.case = TRUE)]
      if (length(m) > 0) m[1] else ""
    }
    
    # Prefill start_date from first row if possible
    observeEvent(raw_file(), {
      df   <- raw_file()
      cols <- names(df)
      
      dt_col    <- find_col(cols, c("datetime", "timestamp"))
      first_date <- NA
      
      if (nzchar(dt_col)) {
        first_dt <- suppressWarnings(as.POSIXct(df[[dt_col]][1], tz = "UTC"))
        if (!is.na(first_dt)) {
          first_date <- as.Date(first_dt)
        } else {
          first_date <- suppressWarnings(as.Date(df[[dt_col]][1]))
        }
      }
      
      if (is.na(first_date)) {
        ycol <- find_col(cols, c("year", "yr"))
        mcol <- find_col(cols, c("month", "mon"))
        dcol <- find_col(cols, c("day", "dy"))
        if (nzchar(ycol) && nzchar(mcol) && nzchar(dcol)) {
          first_date <- suppressWarnings(
            as.Date(paste(df[[ycol]][1], df[[mcol]][1], df[[dcol]][1]), format = "%Y %m %d")
          )
        }
      }
      
      if (!is.na(first_date)) {
        updateDateInput(session, "start_date", value = first_date)
      }
    })
    
    return(list(start_date = reactive(input$start_date)))
  })
}