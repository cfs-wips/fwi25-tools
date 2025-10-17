# R/modules/mod_plot.R

mod_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        selectInput(
          ns("plot_dataset"),
          label = NULL, # set/translated from server
          choices = c("results" = "results", "inputs" = "inputs"),
          selected = "inputs"
        )
      ),
      column(
        4,
        # Render the label (with tooltip/help) separately to avoid JSON
        uiOutput(ns("plot_y_multi_label")),
        selectizeInput(
          ns("plot_y_multi"),
          label = NULL, # label comes from the UI output above
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "")
        )
      ),
      column(
        2,
        numericInput(
          ns("facet_ncol"),
          label = NULL, value = 2, min = 1, max = 4, step = 1
        )
      ),
      column(
        2,
        checkboxInput(
          ns("facet_free_y"),
          label = NULL, value = TRUE
        )
      )
    ),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(ns("plot_ts"), height = "80vh"),
      type = 4
    )
  )
}

mod_plot_server <- function(id, tr, label_for_col, shaped_input, results, df87){
  moduleServer(id, function(input, output, session){
    
    # --- Translate simple inputs -------------------------------------------------
    observe({
      cur_ds <- if (isTruthy(input$plot_dataset)) input$plot_dataset else "inputs"
      updateSelectInput(
        session, "plot_dataset",
        label = tr("data_source"),
        choices = setNames(
          c("results","inputs"),
          c(tr("data_src_results"), tr("data_src_inputs"))
        ),
        selected = cur_ds
      )
      updateNumericInput(session, "facet_ncol", label = tr("facets_per_row"))
      updateCheckboxInput(session, "facet_free_y", label = tr("free_y"))
    })
    
    # Label with tooltip/help
    output$plot_y_multi_label <- shiny::renderUI({
      label_with_help(tr("plot_vars"), tr("tt_plot_vars"))
    })
    
    # --- Detect data & datetime column ------------------------------------------
    data_for_plot <- reactive({
      req(input$plot_dataset)
      
      df <- if (identical(input$plot_dataset, "inputs")) {
        si <- shaped_input()
        validate(need(!is.null(si), tr("err_upload_and_map_first")))
        as.data.frame(si$inputs)
      } else {
        as.data.frame(results())
      }
      validate(need(NROW(df) > 0, tr("err_dataset_no_rows")))
      
      # Find a datetime-like column name
      dt_candidates <- names(df)[
        grepl("datetime|timestamp|date|time", names(df), ignore.case = TRUE)
      ]
      dt_col <- character(0)
      if (length(dt_candidates)){
        typed <- dt_candidates[
          vapply(df[dt_candidates],
                 function(x) inherits(x, c("POSIXt","Date")) || is.character(x),
                 logical(1))
        ]
        dt_col <- if (length(typed)) typed[1] else dt_candidates[1]
      }
      if (!length(dt_col) && all(c("year","month","day","hour") %in% names(df))) {
        tz_use <- {
          si <- shaped_input()
          si_tz <- try(si$tz, silent = TRUE)
          if (inherits(si_tz, "try-error") || is.null(si_tz) || !nzchar(si_tz)) "UTC" else si_tz
        }
        df$datetime <- lubridate::make_datetime(
          year  = as.integer(df$year),
          month = as.integer(df$month),
          day   = as.integer(df$day),
          hour  = as.integer(df$hour),
          tz    = tz_use
        )
        dt_col <- "datetime"
      }
      validate(need(length(dt_col) == 1, tr("err_no_datetime_found")))
      
      ord <- try(order(df[[dt_col]]), silent = TRUE)
      if (!inherits(ord, "try-error")) df <- df[ord, , drop = FALSE]
      attr(df, "dt_col") <- dt_col
      df
    })
    
    # --- Populate choices + sensible defaults -----------------------------------
    prev_dataset <- reactiveVal(NULL)
    
    populate_plot_choices <- function(reset_defaults = FALSE){
      df <- data_for_plot()
      dt_col <- attr(df, "dt_col")
      
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      raw_choices <- setdiff(num_cols, dt_col)
      if (length(raw_choices) == 0L){
        updateSelectizeInput(session, "plot_y_multi",
                             choices = character(0), selected = character(0))
        return(invisible(NULL))
      }
      
      pref <- if (identical(input$plot_dataset, "inputs"))
        c("temp","rh","ws","rain") else c("ffmc","dmc","dc","fwi")
      
      lc  <- tolower(raw_choices)
      idx <- match(pref, lc, nomatch = 0)
      want <- raw_choices[idx[idx > 0]]
      
      still_valid <- {
        prev <- isolate(input$plot_y_multi)
        if (!length(prev)) character(0) else intersect(prev, raw_choices)
      }
      default_sel <- if (reset_defaults) {
        if (length(want)) want else utils::head(raw_choices, 4)
      } else {
        if (length(still_valid)) still_valid else if (length(want)) want else utils::head(raw_choices, 4)
      }
      
      named_choices <- stats::setNames(
        raw_choices,
        vapply(raw_choices, function(x) label_for_col(x, type = "short"), character(1))
      )
      
      updateSelectizeInput(
        session, "plot_y_multi",
        choices = named_choices,
        selected = unique(default_sel)
      )
      
      prev_dataset(input$plot_dataset)
    }
    
    observeEvent(list(data_for_plot(), input$plot_dataset), {
      reset <- !identical(prev_dataset(), input$plot_dataset)
      populate_plot_choices(reset_defaults = reset)
    }, ignoreInit = FALSE)
    
    # --- Render Plot -------------------------------------------------------------
    output$plot_ts <- plotly::renderPlotly({
      df <- data_for_plot()
      dt_col <- attr(df, "dt_col")
      
      req(length(input$plot_y_multi) >= 1)
      yvars <- unique(input$plot_y_multi)
      
      keep_cols <- unique(c(dt_col, yvars, "id"))
      keep_cols <- intersect(keep_cols, names(df))
      df_small <- df[, keep_cols, drop = FALSE]
      
      # Drop requested yvars that don't exist (prevents pivot_longer error)
      common <- intersect(yvars, names(df_small))
      if (length(common) == 0L){
        populate_plot_choices(reset_defaults = TRUE)
        validate(need(FALSE, tr("err_no_selected_vars_in_dataset")))
      }
      if (length(common) < length(yvars)){
        updateSelectizeInput(session, "plot_y_multi", selected = unique(common))
      }
      
      
      # Long format
      long_df <- df_small |>
        tidyr::pivot_longer(cols = tidyselect::all_of(common),
                            names_to = "variable", values_to = "value") |>
        dplyr::filter(!is.na(.data$value))
      
      # Labels
      var_label_levels <- vapply(common, function(v) label_for_col(v, type = "short"), character(1))
      long_df$var_label <- vapply(as.character(long_df$variable),
                                  function(v) label_for_col(v, type = "short"),
                                  character(1))
      long_df$var_label <- factor(long_df$var_label, levels = var_label_levels)
      
      # Legend/source labels
      lab_fwi25 <- tr("legend_fwi25")
      lab_fwi87 <- tr("legend_fwi87")
      long_df$source <- lab_fwi25
      
      # Overlay from daily FWI (FWI87)
      overlay_df <- NULL
      if (identical(input$plot_dataset, "results")){
        d87 <- df87()
        if (!is.null(d87) && NROW(d87)){
          dt87 <- if ("datetime" %in% names(d87)) "datetime" else NULL
          if (is.null(dt87) && all(c("year","month","day") %in% names(d87))){
            si <- shaped_input()
            tz_use <- if (!is.null(si$tz) && nzchar(si$tz)) si$tz else "UTC"
            d87$datetime <- lubridate::make_datetime(
              d87$year, d87$month, d87$day, hour = 12L, tz = tz_use
            )
            dt87 <- "datetime"
          }
          if (!is.null(dt87)){
            common87 <- intersect(common, intersect(names(d87), names(df)))
            if (length(common87)){
              keep87 <- unique(c(dt87, common87, "id"))
              d87_small <- d87[, keep87, drop = FALSE]
              overlay_df <- d87_small |>
                tidyr::pivot_longer(cols = tidyselect::all_of(common87),
                                    names_to = "variable", values_to = "value") |>
                dplyr::filter(!is.na(.data$value))
              if (NROW(overlay_df)){
                overlay_df$var_label <- vapply(as.character(overlay_df$variable),
                                               function(v) label_for_col(v, type = "short"),
                                               character(1))
                overlay_df$var_label <- factor(overlay_df$var_label, levels = var_label_levels)
                overlay_df$source <- lab_fwi87
              } else overlay_df <- NULL
            }
          }
        }
      }
      
      # Round numeric for tooltips
      long_df   <- dplyr::mutate(long_df,   dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))
      if (!is.null(overlay_df))
        overlay_df <- dplyr::mutate(overlay_df, dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))
      
      # Make a colour key that distinguishes source × station
      get_id_str <- function(d){
        if ("id" %in% names(d)) as.character(d$id) else "station"
      }
      long_df$series_id <- paste(long_df$source, get_id_str(long_df), sep = "__")
      if (!is.null(overlay_df))
        overlay_df$series_id <- paste(overlay_df$source, get_id_str(overlay_df), sep = "__")
      
      # Build palettes: FWI25 uses Dark2, FWI87 uses Set2
      id_vals <- if ("id" %in% names(long_df)) unique(as.character(long_df$id)) else "station"
      n_ids   <- length(id_vals)
      n_cols  <- max(3, min(8, n_ids)) # Brewer palettes sizes
      pal_fwi25 <- rev(RColorBrewer::brewer.pal(n_cols, "Paired")[seq_len(n_ids)])
      pal_fwi87 <- RColorBrewer::brewer.pal(n_cols, "Set3")[seq_len(n_ids)]
      
      names(pal_fwi25) <- paste(lab_fwi25, id_vals, sep="__")
      names(pal_fwi87) <- paste(lab_fwi87, id_vals, sep="__")
      colour_map <- c(pal_fwi25, pal_fwi87)
      
      # Legend labels: "StationID — Source"
      breaks_in_data <- unique(c(long_df$series_id, if (!is.null(overlay_df)) overlay_df$series_id))
      label_series <- function(x){
        parts <- strsplit(x, "__", fixed = TRUE)[[1]]
        sprintf("%s — %s", parts[2], parts[1])
      }
      label_map <- stats::setNames(vapply(breaks_in_data, label_series, character(1)), breaks_in_data)
      
      # Title and facets
      ncol_facets <- { val <- input$facet_ncol; if (is.null(val) || is.na(val) || val < 1) 1L else as.integer(val) }
      title_txt <- if (length(common) == 1)
        sprintf(tr("plot_var_over_time"), label_for_col(common[1], type = "short"))
      else tr("plot_sel_vars_over_time")
      
      base_aes <- ggplot2::aes(
        x = .data[[dt_col]], y = .data$value,
        colour = .data$series_id, linetype = .data$source
      )
      
      p <- ggplot2::ggplot(long_df, base_aes) +
        ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE)
      if (NROW(long_df) < 20000)
        p <- p + ggplot2::geom_point(size = 0.8, alpha = 0.75, na.rm = TRUE)
      
      if (!is.null(overlay_df)) {
        p <- p + ggplot2::geom_line(
          data = overlay_df,
          ggplot2::aes(x = .data$datetime, y = .data$value,
                       colour = .data$series_id, linetype = .data$source),
          linewidth = 0.8, na.rm = TRUE, inherit.aes = FALSE
        )
      }
      
      p <- p +
        ggplot2::facet_wrap(~var_label, ncol = ncol_facets,
                            scales = if (isTRUE(input$facet_free_y)) "free_y" else "fixed") +
        ggplot2::scale_colour_manual(values = colour_map, breaks = breaks_in_data, labels = label_map) +
        ggplot2::scale_linetype_manual(values = stats::setNames(c("solid","dashed"), c(lab_fwi25, lab_fwi87))) +
        ggplot2::labs(x = tr("plot_time_x"), y = NULL, title = title_txt, colour = NULL, linetype = NULL) +
        theme_goc()
      
      plotly::ggplotly(p, tooltip = c("x","y","colour")) |>
        plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","lasso2d")) |>
        plotly::plotly_build()
    })
  })
}