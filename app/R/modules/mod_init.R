# R/modules/mod_init.R

mod_init_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        # Label with tooltip as HTML
        uiOutput(ns("lbl_ffmc0")),
        numericInput(ns("ffmc0"), label = NULL, value = 85, min = 0, max = 101, step = 1)
      ),
      column(
        4,
        uiOutput(ns("lbl_dmc0")),
        numericInput(ns("dmc0"), label = NULL, value = 6, min = 0, step = 1)
      ),
      column(
        4,
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
      checkboxInput(ns("calc_fwi87"), label = "", value = TRUE)
    )
  )
}

mod_init_server <- function(id, tr){
  moduleServer(id, function(input, output, session){
    
    # Render labels with tooltips; use <label for="..."> for accessibility + click behavior
    output$lbl_ffmc0 <- renderUI({
      tags$label(
        `for` = session$ns("ffmc0"),
        label_with_help(tr("init_ffmc"), tr("tt_init_ffmc"))
      )
    })
    output$lbl_dmc0 <- renderUI({
      tags$label(
        `for` = session$ns("dmc0"),
        label_with_help(tr("init_dmc"), tr("tt_init_dmc"))
      )
    })
    output$lbl_dc0 <- renderUI({
      tags$label(
        `for` = session$ns("dc0"),
        label_with_help(tr("init_dc"), tr("tt_init_dc"))
      )
    })
    output$lbl_calc_fwi87 <- renderUI({
      tags$label(
        `for` = session$ns("calc_fwi87"),
        label_with_help(tr("calc_fwi87"), tr("tt_calc_fwi87"))
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