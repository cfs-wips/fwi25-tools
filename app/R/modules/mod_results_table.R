# R/modules/mod_results_table.R

mod_results_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$section(class = "gc-card",
                 tags$div(
                   role = 'region',
                   `aria-label` = "FWI25 results table",
                   uiOutput(ns("title"))
                 ),
                 # Content region: placeholder + DT output (both always in DOM)
                 div(class = "gc-card__content",
                     # Placeholder text (no spinner)—hidden via CSS when DT is spinning or after it renders
                     div(
                       class = "gc-placeholder",
                       `aria-live` = "polite",
                       `aria-busy` = "true",
                       div(class = "gc-placeholder__text", uiOutput(ns("hint")))
                     ),
                     # The actual table output—wrapped with a spinner (shown while recalculating)
                     shinycssloaders::withSpinner(
                       DT::DTOutput(ns("tbl"), width = "100%"),
                       type = 8, color = "#26374A"
                     )
                 )
    )
  )
}

mod_results_table_server <- function(id, tr, dt_i18n, results){
  moduleServer(id, function(input, output, session){
    
    # i18n helper with safe fallbacks
    i18n_or <- function(key, default) {
      val <- tryCatch(tr(key), error = function(e) NULL)
      if (is.null(val)) return(default)
      val_chr <- as.character(val)
      if (length(val_chr) == 0 || !nzchar(val_chr)) default else val_chr
    }
    
    # Title is always present (keeps the region header stable)
    output$title <- renderUI({
      h4(i18n_or("fwi25_results_title", "FWI25 results"))
    })
    outputOptions(output, "title", suspendWhenHidden = FALSE)
    
    # Placeholder hint (does not touch results(); avoids req() aborts)
    output$hint <- renderUI({
      i18n_or("hint_run_to_compute", "Click Run to compute results.")
    })
    outputOptions(output, "hint", suspendWhenHidden = FALSE)
    
    # Table renders only when results are available
    output$tbl <- DT::renderDT({
      req(results())
      df <- results()
      
      DT::datatable(
        df, escape = TRUE, filter = "top",
        extensions = c("Buttons","Scroller"),
        class = "display nowrap compact hover stripe gc-dt",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 275,
          scroller = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy",  text = tr("dt_btn_copy")),
            list(extend = "csv",   text = tr("dt_btn_csv"),   file = 'hFWI.csv'),
            list(extend = "excel", text = tr("dt_btn_excel"), filename = "hFWI.xlsx")
          ),
          processing = TRUE,
          language = dt_i18n()
        )
      )
    })
    outputOptions(output, "tbl", suspendWhenHidden = FALSE)
  })
}