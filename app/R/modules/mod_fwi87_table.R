# R/modules/mod_fwi87_table.R
# ------------------------------------------------------------------------------
# FWI87 (daily) table
# - Show only after Run (req df87())
# - Render any POSIX datetime columns in the data's TZ
# - Alignment fix: safe global script + micro-delay after tab shown
# ------------------------------------------------------------------------------

mod_fwi87_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$script(HTML(
      "
      $(function(){
        var adjust = function(){
          try{ $.fn.dataTable.tables({visible:true, api:true}).columns.adjust(); }catch(e){}
        };
        $(document).on('shown.bs.tab shown.bs.collapse', function(){
          setTimeout(adjust, 0);
        });
        $(window).on('resize.dt', adjust);
      });
      "
    )),
    tags$section(class = "gc-card",
                 tags$div(role = 'region', `aria-label` = "FWI87 results table", uiOutput(ns("title"))),
                 div(class = "gc-card__content",
                     div(class = "gc-placeholder", `aria-live` = "polite", `aria-busy` = "true",
                         div(class = "gc-placeholder__text", uiOutput(ns("hint")))),
                     shinycssloaders::withSpinner(
                       DT::DTOutput(ns("tbl"), width = "100%"),
                       type = 8, color = "#26374A"
                     )
                 )
    )
  )
}

mod_fwi87_table_server <- function(id, tr, dt_i18n, df87, tz_reactive){
  moduleServer(id, function(input, output, session){
    
    i18n_or <- function(key, default){
      val <- tryCatch(tr(key), error = function(e) NULL)
      if (is.null(val)) return(default)
      val_chr <- as.character(val)
      if (length(val_chr) == 0 || !nzchar(val_chr)) default else val_chr
    }
    
    output$title <- renderUI({ h4(i18n_or("fwi87_results_title", "FWI87 daily results")) })
    outputOptions(output, "title", suspendWhenHidden = FALSE)
    
    output$hint <- renderUI({ i18n_or("hint_run_to_compute", "Click Run to compute results.") })
    outputOptions(output, "hint", suspendWhenHidden = FALSE)
    
    to_display_tz <- function(df, tz){
      if (is.null(df) || !nrow(df)) return(df)
      is_posix <- vapply(df, function(x) inherits(x, c("POSIXt","POSIXct","POSIXlt")), logical(1))
      if (any(is_posix)){
        for (nm in names(df)[is_posix]){
          df[[nm]] <- lubridate::with_tz(df[[nm]], tzone = tz)
        }
      }
      df
    }
    
    table_data <- reactive({
      tz_use <- tz_reactive()
      d <- df87()
      req(!is.null(d), nrow(d) > 0)
      d <- as.data.frame(d)
      to_display_tz(d, tz_use)
    })
    
    output$tbl <- DT::renderDT({
      d <- table_data()
      DT::datatable(
        d,
        rownames = FALSE,
        escape   = TRUE,
        filter   = "top",
        class    = "display nowrap compact hover stripe gc-dt",
        extensions = c("Buttons","Scroller"),
        options = list(
          language    = dt_i18n(),
          autoWidth   = TRUE,
          scrollX     = TRUE,
          deferRender = TRUE,
          scroller    = TRUE,
          pageLength  = 25,
          lengthMenu  = list(c(10,25,50,100,-1), c('10','25','50','100','All')),
          scrollY     = 300,
          dom         = "Bfrtip",
          buttons = list(
            list(extend = "copy",  text = tr("dt_btn_copy")),
            list(extend = "csv",   text = tr("dt_btn_csv"),   filename = "dailyFWI"),
            list(extend = "excel", text = tr("dt_btn_excel"), filename = "dailyFWI")
          )
        )
      )
    }, server = TRUE, fillContainer = TRUE)
    outputOptions(output, "tbl", suspendWhenHidden = FALSE)
  })
}
