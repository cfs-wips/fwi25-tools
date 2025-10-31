# R/modules/mod_actions.R

mod_actions_ui <- function(id) {
  ns <- NS(id)
  # Run + Download + Help (compact row)
  fluidRow(
    column(5, uiOutput(ns("run_ui"))),
    column(5, uiOutput(ns("dl_ui"))),
    column(
      2,
      # Small help icon button (always available)
      tags$div(
        style = "display:flex;justify-content:flex-end;align-items:center;height:100%;",
        actionButton(
          ns("help"),
          label = NULL,
          class = "btn btn-link",
          title = "Help / Aide",
          `aria-label` = "Open help / Ouvrir l'aide",
          icon = shiny::icon("circle-question") # Font Awesome 6 alias in recent Shiny
        )
      )
    )
  )
}

# small helper used above (same %||% semantics as elsewhere)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

#' @param tr translator function (from mod_i18n_server()$tr)
#' @param results reactive data.frame returned by the engine (used for download)
#' @param csv_name reactive() used to build a friendly filename
mod_actions_server <- function(id, tr, results, csv_name) {
  moduleServer(id, function(input, output, session) {
    # --- Run button ---
    output$run_ui <- renderUI(
      actionButton(
        session$ns("run"),
        label = tr("run_hfwi"),
        class = "btn-primary",
        `aria-label` = tr("aria_run_label")
      )
    )

    # --- Download button ---
    output$dl_ui <- renderUI(
      downloadButton_sl(session$ns("dl"), tr("download_results"))
    )

    safe_fwrite <- function(x, file) {
      tryCatch(
        data.table::fwrite(x, file),
        error = function(e) utils::write.csv(x, file, row.names = FALSE)
      )
    }

    output$dl <- downloadHandler(
      filename = function() {
        sprintf(
          "hfwi_%s",
          basename(csv_name() %||% "results")
        )
      },
      content = function(file) {
        req(results()) # block until results exist
        safe_fwrite(results(), file)
      },
      contentType = "text/csv"
    )

    # --- Help icon opens the same i18n-backed modal used on first load ---
    observeEvent(input$help, {
      showModal(
        modalDialog(
          title = tr("modal_title"),
          HTML(tr("modal_body_html")),
          easyClose = TRUE,
          footer = modalButton(tr("modal_close")),
          size = "l"
        )
      )
    })

    # Return the run click as before
    return(list(run = reactive(input$run)))
  })
}
