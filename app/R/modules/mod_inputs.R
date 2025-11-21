mod_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "gc-card",
             radioButtons(ns("view_mode"), "View:", choices = c("Raw" = "raw", "Hourly" = "hourly"), inline = TRUE),
             div(
               class = "gc-spin-wrap",
               DT::dataTableOutput(ns("inputs_table")),
               div(class = "gc-spin-overlay",
                   div(class = "gc-spinner", `aria-hidden` = "true"),
                   span(class = "sr-only", "Loading…")
               )
             )
    )
  )
}
mod_inputs_server <- function(id, tr, dt_i18n, raw_data, hourly_data) {
  moduleServer(id, function(input, output, session) {
    output$inputs_table <- DT::renderDataTable({
      # DataTables callback ensures column widths re-sync after init/draw/resize.
      cb <- DT::JS("
        var tbl = table;          // DataTables API instance
        function adjust(){ try { tbl.columns.adjust(); } catch(e){} }
        setTimeout(adjust, 0);    // after init
        tbl.on('draw.dt', adjust);
        $(window).on('resize.dt', adjust);
      ")
      if (input$view_mode == "raw") {
        validate(need(!is.null(raw_data()), "No raw data available"))
        data<-raw_data()
      } else {
        validate(need(!is.null(hourly_data()), "No hourly data available"))
        data <- hourly_data()
      }
      DT::datatable(
        data,
        rownames = FALSE,
        fillContainer = TRUE,
        escape = TRUE,
        filter = "top",
        class = "display nowrap compact hover stripe gc-dt",
        extensions = c("Buttons", "Scroller"),
        options = list(
          language = dt_i18n(),
          autoWidth = TRUE,
          scrollX = TRUE,
          deferRender = TRUE,
          scroller = TRUE,
          scrollY = 300,
          scrollCollapse = TRUE,
          pageLength = 25,
          lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", text = tr("dt_btn_copy")),
            list(extend = "csv", text = tr("dt_btn_csv"), filename = "hFWI"),
            list(extend = "excel", text = tr("dt_btn_excel"), filename = "hFWI")
          ),
          initComplete = DT::JS("function(){ this.api().columns.adjust(); }")
        ),
        callback = cb
      )
    }, options = list(scrollX = TRUE))
  })
}