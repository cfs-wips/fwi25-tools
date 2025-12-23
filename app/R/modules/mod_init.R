# R/modules/mod_init.R

mod_init_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        3,
        mod_filter_ui("filter")
      ),
      column(
        3,
        # Label with tooltip as HTML
        uiOutput(ns("lbl_ffmc0")),
        numericInput(ns("ffmc0"), label = NULL, value = 85, min = 0, max = 101, step = 1)
      ),
      column(
        3,
        uiOutput(ns("lbl_dmc0")),
        numericInput(ns("dmc0"), label = NULL, value = 6, min = 0, step = 1)
      ),
      column(
        3,
        uiOutput(ns("lbl_dc0")),
        numericInput(ns("dc0"), label = NULL, value = 15, min = 0, step = 1)
      )
    ),

    # Checkbox with external <label> that includes the help icon
    div(
      role = "group",
      class = "upload-checkbox",
      `aria-labelledby` = ns("lbl_calc_fwi87"),
      uiOutput(ns("lbl_calc_fwi87")),
      checkboxInput(ns("calc_fwi87"), label = "", value = T)
    )
  )
}

mod_init_server <- function(id, tr) {
  moduleServer(id, function(input, output, session) {
    i18n_or <- function(key, default) {
      val <- tryCatch(tr(key), error = function(e) NULL)
      if (is.null(val)) {
        return(default)
      }
      val_chr <- as.character(val)
      if (!length(val_chr) || !nzchar(val_chr)) {
        return(default)
      }
      # treat '??key??' as missing and use the default
      if (grepl("^\\?\\?.*\\?\\?$", val_chr)) {
        return(default)
      }
      val_chr
    }

    # Render labels with tooltips; use <label for="..."> for accessibility + click behavior
    output$lbl_ffmc0 <- renderUI({
      tags$label(
        `for` = session$ns("ffmc0"),
        label_with_help(tr("init_ffmc"), i18n_or("tt_init_ffmc"))
      )
    })
    output$lbl_dmc0 <- renderUI({
      tags$label(
        `for` = session$ns("dmc0"),
        label_with_help(i18n_or("init_dmc"), i18n_or("tt_init_dmc"))
      )
    })
    output$lbl_dc0 <- renderUI({
      tags$label(
        `for` = session$ns("dc0"),
        label_with_help(i18n_or("init_dc"), i18n_or("tt_init_dc"))
      )
    })
    output$lbl_calc_fwi87 <- renderUI({
      tags$label(
        `for` = session$ns("calc_fwi87"),
        label_with_help(i18n_or("calc_fwi87"), i18n_or("tt_calc_fwi87"))
      )
    })

    return(list(
      ffmc0      = reactive(input$ffmc0),
      dmc0       = reactive(input$dmc0),
      dc0        = reactive(input$dc0),
      calc_fwi87 = reactive(isTRUE(input$calc_fwi87))
    ))
  })
}
