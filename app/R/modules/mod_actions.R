# R/modules/mod_actions.R
mod_actions_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(6, uiOutput(ns("run_ui"))),
    column(6, uiOutput(ns("dl_ui")))
  )
}

mod_actions_server <- function(id, tr, results, csv_name){
  moduleServer(id, function(input, output, session){
    output$run_ui <- renderUI(actionButton(session$ns("run"), label = tr("run_hfwi"), class = "btn-primary", `aria-label` = tr("aria_run_label")))
    output$dl_ui  <- renderUI(downloadButton_sl(session$ns("dl"), tr("download_results")))

    safe_fwrite <- function(x, file){ tryCatch(data.table::fwrite(x, file), error = function(e) utils::write.csv(x, file, row.names = FALSE)) }

    output$dl <- downloadHandler(
      filename = function(){ sprintf("hfwi_%s", basename(csv_name() %||% "results")) },
      content = function(file){ safe_fwrite(results(), file) },
      contentType = "text/csv"
    )

    return(list(run = reactive(input$run)))
  })
}
