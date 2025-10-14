# R/modules/mod_upload.R

mod_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Section title with tooltip + <details> (unchanged)
    h4(id = ns("lbl_upload_csv"), uiOutput(ns("lbl_upload_csv"))),
    
    # TIGHT: place fileInput and checkbox on one line
    tags$div(
      class = "upload-inline",
      role = "group",
      `aria-labelledby` = ns("lbl_upload_csv"),
      
      # File input (rendered via server)
      uiOutput(ns("csv_input_ui")),
      
      # Checkbox with external label (tooltip) inline
      tags$div(
        class = "upload-checkbox",
        uiOutput(ns("lbl_has_header")),
        checkboxInput(ns("has_header"), label = NULL, value = TRUE, width = "auto")
      )
    )
  )
}

mod_upload_server <- function(id, tr){
  moduleServer(id, function(input, output, session){
    
    # Rich title
    output$lbl_upload_csv <- renderUI({
      label_with_help_rich(
        label_text   = tr("upload_csv"),
        tip_text     = tr("csv_spec_sr"),
        popover_html = tr("csv_spec_html"),
        sr_label     = tr("upload_csv")
      )
    })
    
    # File input
    output$csv_input_ui <- renderUI({
      fileInput(
        session$ns("csv"),
        label = NULL,
        buttonLabel = tr("csv_button_label"),
        placeholder = tr("csv_place_holder"),
        accept = c(".csv","text/csv")
      )
    })
    
    # External label + tooltip for checkbox (keeps a11y + avoids [object Object])
    output$lbl_has_header <- renderUI({
      tags$label(
        `for` = session$ns("has_header"),
        label_with_help(tr("csv_has_header"), tr("tt_has_header"))
      )
    })
    
    raw_file <- reactive({
      req(input$csv)
      tryCatch(
        data.table::fread(
          input$csv$datapath,
          sep = ",",
          na.strings = c("","NA","NaN","null"),
          header = isTRUE(input$has_header),
          blank.lines.skip = TRUE
        ),
        error = function(e)
          utils::read.csv(
            input$csv$datapath,
            header = isTRUE(input$has_header),
            na.strings = c("","NA","NaN","null"),
            check.names = FALSE,
            stringsAsFactors = FALSE,
            blank.lines.skip = TRUE
          )
      )
    })
    
    return(list(
      raw_file = raw_file,
      cols     = reactive(if (is.null(input$csv)) character(0) else names(raw_file())),
      csv_name = reactive(if (is.null(input$csv)) "results" else input$csv$name)
    ))
  })
}