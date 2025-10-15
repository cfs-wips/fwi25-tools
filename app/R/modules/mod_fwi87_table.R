# R/modules/mod_fwi87_table.R

mod_fwi87_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$section(class = "gc-card",
                 tags$div(
                   role = 'region',
                   `aria-label` = "FWI87 results table",
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

mod_fwi87_table_server <- function(id, tr, dt_i18n, df87){
  moduleServer(id, function(input, output, session){
    
    # i18n helper with safe fallbacks
    i18n_or <- function(key, default) {
      val <- tryCatch(tr(key), error = function(e) NULL)
      if (is.null(val)) return(default)
      val_chr <- as.character(val)
      if (length(val_chr) == 0 || !nzchar(val_chr)) default else val_chr
    }
    
    output$title <- renderUI({
      h4(i18n_or("fwi87_results_title", "FWI87 daily results"))
    })
    outputOptions(output, "title", suspendWhenHidden = FALSE)
    
    output$hint <- renderUI({
      # Generic hint covers both “Run first” and “ensure FWI87 is enabled”
      i18n_or("hint_run_to_compute", "Click Run to compute results.")
    })
    outputOptions(output, "hint", suspendWhenHidden = FALSE)
    
    output$tbl <- DT::renderDT({
      d <- df87()
      req(!is.null(d), nrow(d) > 0)
      
      DT::datatable(
        d, escape = TRUE, filter = "top",
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
            list(extend = "csv",   text = tr("dt_btn_csv"),   filename = "dailyFWI.csv"),
            list(extend = "excel", text = tr("dt_btn_excel"), filename = "dailyFWI.xlsx")
          ),
          processing = TRUE,
          language = dt_i18n()
        )
      )
    })
    # IMPORTANT for conditionalPanel: do not suspend this output when hidden
    outputOptions(output, "tbl", suspendWhenHidden = FALSE)
  })
}