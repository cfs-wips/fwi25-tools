# R/modules/mod_results_table.R
mod_results_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$section(class = "gc-card",
      tags$div(role = 'region', `aria-label` = "FWI25 results table", uiOutput(ns("title"))),
      shinycssloaders::withSpinner(DT::DTOutput(ns("tbl"), width = "100%", height = "40vh"))
    )
  )
}

mod_results_table_server <- function(id, tr, dt_i18n, results){
  moduleServer(id, function(input, output, session){
    output$title <- renderUI({ if (isTruthy(results())) h4(tr("fwi25_results_title")) })

    output$tbl <- DT::renderDT({
      req(results())
      df <- results()
      DT::datatable(
        df, escape = TRUE, filter = "top",
        extensions = c("Buttons","Scroller"),
        class = "display nowrap compact hover stripe gc-dt",
        options = list(
          pageLength = 25, scrollX = TRUE, deferRender = TRUE,
          scrollY = 500, scroller = TRUE, dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", text = tr("dt_btn_copy")),
            list(extend = "csv",  text = tr("dt_btn_csv"), file = 'hFWI.csv'),
            list(extend = "excel", text = tr("dt_btn_excel"), filename = "hFWI.xlsx")
          ),
          processing = TRUE,
          language = dt_i18n()
        )
      )
    })
  })
}
