# R/modules/mod_upload.R
mod_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4(id = ns("lbl_upload_csv"), textOutput(ns("lbl_upload_csv"))),
    tags$div(role = "group", `aria-labelledby` = ns("lbl_upload_csv"), uiOutput(ns("csv_input_ui"))),
    checkboxInput(ns("has_header"), label = "", value = TRUE)
  )
}

mod_upload_server <- function(id, tr){
  moduleServer(id, function(input, output, session){
    output$lbl_upload_csv <- renderText(tr("upload_csv"))
    output$csv_input_ui <- renderUI({
      fileInput(session$ns("csv"), label = NULL,
                buttonLabel = tr("csv_button_label"),
                placeholder = tr("csv_place_holder"),
                accept = c(".csv","text/csv"))
    })
    observe({ updateCheckboxInput(session, "has_header", label = tr("csv_has_header")) })

    raw_file <- reactive({
      req(input$csv)
      tryCatch(
        data.table::fread(input$csv$datapath, sep = ",", na.strings = c("", "NA", "NaN", "null"), header = isTRUE(input$has_header)),
        error = function(e)
          utils::read.csv(input$csv$datapath, header = isTRUE(input$has_header), na.strings = c("", "NA", "NaN", "null"), check.names = FALSE, stringsAsFactors = FALSE)
      )
    })

    return(list(
      raw_file = raw_file,
      cols = reactive(if (is.null(input$csv)) character(0) else names(raw_file())),
      csv_name = reactive(if (is.null(input$csv)) "results" else input$csv$name)
    ))
  })
}
