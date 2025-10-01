# R/modules/mod_plot.R
mod_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("plot_dataset"), label = NULL, choices = c("results" = "results", "inputs" = "inputs"), selected = "results")),
      column(4, selectizeInput(ns("plot_y_multi"), label = NULL, choices = NULL, multiple = TRUE, options = list(placeholder = ""))),
      column(2, numericInput(ns("facet_ncol"), label = NULL, value = 2, min = 1, max = 4, step = 1)),
      column(2, checkboxInput(ns("facet_free_y"), label = NULL, value = TRUE))
    ),
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("plot_ts"), height = "80vh"), type = 4)
  )
}

mod_plot_server <- function(id, tr, label_for_col, shaped_input, results, df87){
  moduleServer(id, function(input, output, session){
    observe({
      updateSelectInput(session, "plot_dataset", label = tr("data_source"),
        choices = setNames(c("results","inputs"), c(tr("data_src_results"), tr("data_src_inputs"))), selected = input$plot_dataset %||% "results")
      updateNumericInput(session, "facet_ncol", label = tr("facets_per_row"))
      updateCheckboxInput(session, "facet_free_y", label = tr("free_y"))
    })

    data_for_plot <- reactive({
      req(shaped_input(), input$plot_dataset)
      df <- if (identical(input$plot_dataset, "inputs")) as.data.frame(shaped_input()$inputs) else as.data.frame(results())
      validate(need(nrow(df) > 0, "Selected dataset has no rows."))
      dt_candidates <- names(df)[grepl("datetime|timestamp|date|time", names(df), ignore.case = TRUE)]
      dt_col <- character(0)
      if (length(dt_candidates)){
        typed <- dt_candidates[sapply(df[dt_candidates], function(x) inherits(x, c("POSIXt","Date")) || is.character(x))]
        dt_col <- if (length(typed)) typed[1] else dt_candidates[1]
      }
      if (!length(dt_col) && all(c("year","month","day","hour") %in% names(df))){
        tz_use <- shaped_input()$tz %||% "UTC"
        df$datetime <- lubridate::make_datetime(year = as.integer(df$year), month = as.integer(df$month), day = as.integer(df$day), hour = as.integer(df$hour), tz = tz_use)
        dt_col <- "datetime"
      }
      validate(need(length(dt_col) == 1, "Couldn't find or construct a datetime/timestamp column."))
      ord <- try(order(df[[dt_col]]), silent = TRUE); if (!inherits(ord, "try-error")) df <- df[ord, , drop = FALSE]
      attr(df, "dt_col") <- dt_col
      df
    })

    populate_plot_choices <- function(){
      df <- data_for_plot(); dt_col <- attr(df, "dt_col")
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      raw_choices <- setdiff(num_cols, dt_col)
      if (length(raw_choices) == 0L){
        updateSelectizeInput(session, "plot_y_multi", choices = character(0), selected = character(0))
        return(invisible(NULL))
      }
      prev <- isolate(input$plot_y_multi) %||% character(0)
      still_valid <- intersect(prev, raw_choices)
      pref <- c("ffmc","dmc","dc","fwi")
      lc <- tolower(raw_choices)
      idx <- match(pref, lc, nomatch = 0)
      wanted <- raw_choices[idx[idx > 0]]
      default_sel <- if (length(still_valid)) still_valid else if (length(wanted)) wanted else utils::head(raw_choices, 3)
      named_choices <- stats::setNames(raw_choices, vapply(raw_choices, function(x) label_for_col(x, type = "short"), character(1)))
      updateSelectizeInput(session, "plot_y_multi", choices = named_choices, selected = unique(default_sel))
    }

    observeEvent(list(data_for_plot(), input$plot_dataset), { populate_plot_choices() }, ignoreInit = TRUE)

    output$plot_ts <- plotly::renderPlotly({
      df <- data_for_plot(); dt_col <- attr(df, "dt_col")
      req(length(input$plot_y_multi) >= 1)
      yvars <- unique(input$plot_y_multi)
      keep <- unique(c(dt_col, yvars))
      df_small <- df[, keep, drop = FALSE]
      ord <- try(order(df_small[[dt_col]]), silent = TRUE); if (!inherits(ord, "try-error")) df_small <- df_small[ord, , drop = FALSE]
      long_df <- df_small |>
        tidyr::pivot_longer(cols = tidyselect::all_of(yvars), names_to = "variable", values_to = "value") |>
        dplyr::filter(!is.na(.data$value))
      req(nrow(long_df) > 0)
      var_label_levels <- vapply(yvars, function(v) label_for_col(v, type = "short"), character(1))
      long_df$var_label <- vapply(as.character(long_df$variable), function(v) label_for_col(v, type = "short"), character(1))
      long_df$var_label <- factor(long_df$var_label, levels = var_label_levels)
      long_df$source <- tr("legend_fwi25")
      long_df <- dplyr::mutate(long_df, dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))

      overlay_df <- NULL
      if (identical(input$plot_dataset, "results")){
        d87 <- df87()
        if (!is.null(d87) && nrow(d87)){
          dt87 <- if ("datetime" %in% names(d87)) "datetime" else NULL
          if (is.null(dt87) && all(c("year","month","day") %in% names(d87))){
            si <- shaped_input(); d87$datetime <- lubridate::make_datetime(d87$year, d87$month, d87$day, hour = 12L, tz = si$tz); dt87 <- "datetime"
          }
          if (!is.null(dt87)){
            common <- intersect(yvars, intersect(names(d87), names(df)))
            if (length(common)){
              keep87 <- unique(c(dt87, common))
              d87_small <- d87[, keep87, drop = FALSE]
              overlay_df <- d87_small |>
                tidyr::pivot_longer(cols = tidyselect::all_of(common), names_to = "variable", values_to = "value") |>
                dplyr::filter(!is.na(.data$value))
              if (nrow(overlay_df)){
                overlay_df$var_label <- vapply(as.character(overlay_df$variable), function(v) label_for_col(v, type = "short"), character(1))
                overlay_df$var_label <- factor(overlay_df$var_label, levels = var_label_levels)
                overlay_df$source <- tr("legend_fwi87")
                overlay_df <- dplyr::mutate(overlay_df, dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))
              } else overlay_df <- NULL
            }
          }
        }
      }

      ncol_facets <- { val <- input$facet_ncol; if (is.null(val) || is.na(val) || val < 1) 1L else as.integer(val) }
      title_txt <- if (length(yvars) == 1) sprintf(tr("plot_var_over_time"), label_for_col(yvars[1], type = "short")) else tr("plot_sel_vars_over_time")
      col_fwi25 <- "#26374A"; col_fwi87 <- "#BC3331"
      p <- ggplot2::ggplot(long_df, ggplot2::aes(x = .data[[dt_col]], y = .data$value, colour = .data$source, linetype = .data$source)) +
        ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE) +
        { if (nrow(long_df) < 20000) ggplot2::geom_point(size = 0.8, alpha = 0.7, na.rm = TRUE) else NULL } +
        { if (!is.null(overlay_df)) ggplot2::geom_line(data = overlay_df, ggplot2::aes(x = .data$datetime, y = .data$value, colour = .data$source, linetype = .data$source), linewidth = 0.8, na.rm = TRUE) else NULL } +
        ggplot2::facet_wrap(~var_label, ncol = ncol_facets, scales = if (isTRUE(input$facet_free_y)) "free_y" else "fixed") +
        ggplot2::scale_colour_manual(values = c(`FWI2025` = col_fwi25, `IFM2025` = col_fwi25, `FWI87` = col_fwi87, `IFM87` = col_fwi87)) +
        ggplot2::scale_linetype_manual(values = c(`FWI2025` = "solid", `IFM2025` = "solid", `FWI87` = "dashed", `IFM87` = "dashed")) +
        ggplot2::labs(x = tr("plot_time_x"), y = NULL, title = title_txt, colour = NULL, linetype = NULL) +
        theme_goc()

      plotly::ggplotly(p, tooltip = c("x","y","colour")) |>
        plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","lasso2d")) |>
        plotly::plotly_build()
    })
  })
}
