# R/modules/mod_upload.R

mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(id = ns("lbl_upload_csv"), uiOutput(ns("lbl_upload_csv"))),
    tags$div(
      class = "upload-inline",
      role = "group",
      `aria-labelledby` = ns("lbl_upload_csv"),
      # Static file input (no renderUI)
      fileInput(
        ns("csv"),
        label = NULL,
        buttonLabel = NULL, # We'll override via translation
        placeholder = NULL,
        accept = c(".csv", "text/csv")
      ),
      tags$div(
        class = "upload-checkbox",
        uiOutput(ns("lbl_has_header")),
        checkboxInput(ns("has_header"), label = NULL, value = TRUE, width = "auto")
      )
    )
  )
}


mod_upload_server <- function(id, tr, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$id
    # Rich title
    output$lbl_upload_csv <- renderUI({
      label_with_help_rich(
        label_text = tr("upload_csv"),
        tip_text = tr("csv_spec_sr"),
        popover_html = tr("csv_spec_html"),
        sr_label = tr("upload_csv")
      )
    })


    # External label + tooltip for checkbox (keeps a11y + avoids [object Object])
    output$lbl_has_header <- renderUI({
      tags$label(
        `for` = session$ns("has_header"),
        label_with_help(tr("csv_has_header"), tr("tt_has_header"))
      )
    })


    observe({
      id <- session$ns("csv")
      session$sendCustomMessage("updateFileInputLabels", list(
        id = session$ns("csv"), # resolves to "upload-csv-csv"
        buttonLabel = tr("csv_button_label"),
        placeholder = tr("csv_place_holder")
      ))
    }) |> bindEvent(lang())


    # Upload tracking
    upload_id <- reactiveVal(0L)
    observeEvent(input$csv,
      {
        upload_id(isolate(upload_id()) + 1L)
      },
      ignoreInit = TRUE
    )

    # Cache file path
    file_fp <- reactiveVal(NULL)
    observeEvent(input$csv, {
      if (!is.null(input$csv$datapath)) {
        file_fp(input$csv$datapath)
      }
    })

    raw_file <- reactive({
      req(file_fp())
      tryCatch(
        data.table::fread(file_fp(),
          sep = ",", na.strings = c("NA", "NaN", "null", ""),
          header = isTRUE(input$has_header), blank.lines.skip = TRUE
        ),
        error = function(e) {
          utils::read.csv(file_fp(),
            header = isTRUE(input$has_header),
            na.strings = c("NA", "NaN", "null", ""), check.names = FALSE,
            stringsAsFactors = FALSE, blank.lines.skip = TRUE
          )
        }
      )
    })

    return(list(
      raw_file = raw_file,
      cols = reactive(if (is.null(file_fp())) character(0) else names(raw_file())),
      csv_name = reactive(if (is.null(input$csv)) "results" else input$csv$name),
      upload_id = upload_id
    ))
  })
}
