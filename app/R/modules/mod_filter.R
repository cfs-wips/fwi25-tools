# R/modules/mod_filter.R
mod_filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(id = ns("lbl_filter"), textOutput(ns("lbl_filter"))),
    div(`aria-describedby` = ns("help_drop_rows"), dateInput(ns("start_date"), label = NULL, value = NA)),
    tags$p(id = ns("help_drop_rows"), class = "help-block", textOutput(ns("txt_drop_rows_help")))
  )
}

mod_filter_server <- function(id, tr, raw_file, mapping) {
  moduleServer(id, function(input, output, session) {
    output$lbl_filter <- renderText(tr("filter"))
    output$txt_drop_rows_help <- renderText(tr("drop_rows_help"))
    observe({
      updateDateInput(session, "start_date", label = tr("drop_rows_prior"))
    })
    find_col <- function(cols, keywords) {
      rx <- paste0("^(", paste(keywords, collapse = "|"), ")$")
      m <- cols[grepl(rx, cols, ignore.case = TRUE)]
      if (length(m) > 0) m[1] else ""
    }
    # Prefill start_date from first row if possible
    observeEvent(raw_file(), {
      df <- raw_file()
      cols <- names(df)
      datetime_col <- find_col(cols, c("datetime", "timestamp"))
      datetime_col <- suppressWarnings(as.numeric(if (nzchar(datetime_col)) df[[datetime_col]][1] else NA))
      first_date <- NULL
      if (!is.na(datetime_col) == T) {
        first_date <- suppressWarnings(as.Date(df[[datetime_col]][1]))
      } else {
        ycol <- find_col(cols, c("year", "yr"))
        mcol <- find_col(cols, c("month", "mon"))
        dcol <- find_col(cols, c("day", "dy"))

        if (!any(is.na(c(ycol, mcol, dcol)))) {
          first_date <- as.Date(paste(df[[ycol]][1], df[[mcol]][1], df[[dcol]][1]), format = "%Y %m %d")
        }
      }
      if (!is.null(first_date) == T && !is.na(first_date) == T) {
        updateDateInput(session, "start_date", value = first_date)
      }
    })

    return(list(start_date = reactive(input$start_date)))
  })
}
