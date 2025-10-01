# R/modules/mod_init.R
mod_init_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, numericInput(ns("ffmc0"), label = NULL, value = 85, min = 0, max = 101, step = 1)),
      column(4, numericInput(ns("dmc0"),  label = NULL, value = 6, min = 0, step = 1)),
      column(4, numericInput(ns("dc0"),   label = NULL, value = 15, min = 0, step = 1))
    ),
    checkboxInput(ns("calc_fwi87"), label = "", value = TRUE)
  )
}

mod_init_server <- function(id, tr){
  moduleServer(id, function(input, output, session){
    observe({
      updateNumericInput(session, "ffmc0", label = tr("init_ffmc"))
      updateNumericInput(session, "dmc0",  label = tr("init_dmc"))
      updateNumericInput(session, "dc0",   label = tr("init_dc"))
      updateCheckboxInput(session, "calc_fwi87", label = tr("calc_fwi87"))
    })
    return(list(
      ffmc0 = reactive(input$ffmc0),
      dmc0  = reactive(input$dmc0),
      dc0   = reactive(input$dc0),
      calc_fwi87 = reactive(isTRUE(input$calc_fwi87))
    ))
  })
}
