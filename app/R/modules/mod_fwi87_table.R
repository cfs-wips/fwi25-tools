# R/modules/mod_fwi87_table.R
mod_fwi87_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$section(class = "gc-card",
      tags$div(role = 'region', `aria-label` = "FWI87 results table", uiOutput(ns("title"))),
      shinycssloaders::withSpinner(DT::DTOutput(ns("tbl"), width = "100%"))
    )
  )
}

mod_fwi87_table_server <- function(id, tr, dt_i18n, df87){
  moduleServer(id, function(input, output, session){
    output$title <- renderUI({ if (isTruthy(df87()) && NROW(df87()) > 0) h4(tr("fwi87_results_title")) })

    output$tbl <- DT::renderDT({
      d <- df87(); req(!is.null(d), nrow(d) > 0)
      DT::datatable(
        d, escape = TRUE, filter = "top",
        extensions = c("Buttons","Scroller"),
        class = "display nowrap compact hover stripe gc-dt",
        options = list(
          pageLength = 10, scrollX = TRUE, deferRender = TRUE,
          scrollY=350, 
          scroller = TRUE, dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", text = tr("dt_btn_copy")),
            list(extend = "csv",  text = tr("dt_btn_csv"), filename = "dailyFWI.csv"),
            list(extend = "excel", text = tr("dt_btn_excel"), filename = "dailyFWI.xlsx")
          ),
          processing = TRUE,
          language = dt_i18n()
        )
      )
    })
  })
}
