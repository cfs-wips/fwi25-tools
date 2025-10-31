# --- R/modules/mod_plot.R ----------------------------------------------------
# Plot module (no FWI87 as selectable source; overlay only)
# - Auto-selects hourly results when available
# - X-axis honors DST policy using a self-contained labeler (no scales::label_datetime)
#     * civil (DST from data)  -> Olson TZ (e.g., "America/Toronto")
#     * standard (Ignore DST)  -> fixed offset TZ ("Etc/GMT±N") via STANDARD offset
# - Tick labels include the offset (±hh:mm)
# - First-load seeding for variable selector (fixes "empty until toggle" issue)
# -----------------------------------------------------------------------------

mod_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$section(
      class = "gc-card",
      tags$div(
        role = "region", `aria-label` = "FWI plots",
        h4(textOutput(ns("title")))
      ),
      div(
        class = "gc-card__content",
        fluidRow(
          column(
            4,
            selectInput(
              ns("plot_dataset"),
              label = NULL, # server sets i18n label
              choices = setNames(
                c("results", "inputs"),
                c("hFWI results (hourly)", "Input (preview)")
              ),
              selected = "inputs"
            )
          ),
          column(
            4,
            uiOutput(ns("plot_y_multi_label")),
            selectizeInput(
              ns("plot_y_multi"),
              label = NULL,
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "")
            )
          ),
          column(
            2,
            numericInput(ns("facet_ncol"), label = NULL, value = 2, min = 1, max = 4, step = 1)
          ),
          column(
            2,
            checkboxInput(ns("facet_free_y"), label = NULL, value = TRUE)
          )
        ),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("plot_ts"), height = "80vh"),
          type = 4
        )
      )
    )
  )
}

#' @param tr translator function
#' @param i18n i18n object (for language reactive)
#' @param label_for_col function mapping column names to localized labels
#' @param shaped_input reactive: PREVIEW inputs (pre-run)
#' @param results reactive: hourly hFWI results (post-run)
#' @param df87 reactive: daily FWI87 results (overlay only; never selectable)
#' @param tz_reactive reactive( character(1) Olson TZ ), e.g., tz$tz_use
#' @param ignore_dst_reactive reactive policy:
#'        TRUE  => fixed standard offset (ignore DST),
#'        FALSE => civil (DST-aware).
#'        Accepts logical/character/numeric; we normalize.
mod_plot_server <- function(
  id, tr, i18n, label_for_col,
  shaped_input, results, df87,
  tz_reactive, ignore_dst_reactive = reactive(TRUE)
) {
  moduleServer(id, function(input, output, session) {
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

    i18n_or <- function(key, default) {
      val <- tryCatch(tr(key), error = function(e) NULL)
      if (is.null(val)) {
        return(default)
      }
      val_chr <- as.character(val)
      if (!length(val_chr) || !nzchar(val_chr)) {
        return(default)
      }
      # treat '??key??' as missing and use the default
      if (grepl("^\\?\\?.*\\?\\?$", val_chr)) {
        return(default)
      }
      val_chr
    }


    # Force a redraw after programmatic seeding of variables
    reseed_tick <- reactiveVal(0L)
    bump_reseed <- function() reseed_tick(isolate(reseed_tick()) + 1L)

    # Use your "Plot" tab label for the card title (from mod_i18n.R)
    output$title <- renderText({
      i18n_or("tab_plot", "Plot")
    })
    outputOptions(output, "title", suspendWhenHidden = FALSE)

    # ---------------- Policy normalization & TZ helpers ----------------
    normalize_policy <- function(val) {
      tolower_chr <- function(x) {
        x <- as.character(x)
        x[is.na(x)] <- ""
        tolower(trimws(x))
      }
      if (is.logical(val)) {
        return(isTRUE(val))
      }
      if (is.numeric(val)) {
        return(isTRUE(val == 1))
      }
      if (is.character(val)) {
        v <- tolower_chr(val)
        if (v %in% c("standard", "ignore", "ignore_dst", "fixed", "std", "no_dst", "offset_fixed")) {
          return(TRUE)
        }
        if (v %in% c("civil", "dst", "from_data", "data", "olson", "local", "use_dst")) {
          return(FALSE)
        }
      }
      FALSE
    }

    std_off_hours <- function(tz) {
      if (exists("tz_standard_offset_hours", mode = "function")) {
        tz_standard_offset_hours(tz) # from your mod_utils.R
      } else {
        ref <- as.POSIXct("2000-01-15 12:00:00", tz = tz)
        z <- format(ref, "%z")
        sgn <- ifelse(substr(z, 1, 1) == "-", -1, 1)
        hh <- suppressWarnings(as.integer(substr(z, 2, 3)))
        mm <- suppressWarnings(as.integer(substr(z, 4, 5)))
        sgn * (hh + mm / 60)
      }
    }
    fixed_tz_from_offset <- function(offset_hours) {
      ifelse(is.finite(offset_hours),
        sprintf("Etc/GMT%+d", -as.integer(offset_hours)), # reversed sign in Etc/GMT
        NA_character_
      )
    }

    # Compute the display TZ for the axis based on the toggle
    display_tz_reactive <- reactive({
      olson <- tz_reactive() %||% "UTC"
      use_std <- normalize_policy(ignore_dst_reactive())
      if (use_std) {
        off <- std_off_hours(olson)
        fixed_tz_from_offset(off)
      } else {
        olson
      }
    })

    # Self-contained datetime labeler (no scales::label_datetime)
    # Prints "YYYY-mm-dd HH:MM ±hh:mm" in the requested TZ.
    make_datetime_labeler <- function(tz, fmt = "%Y-%m-%d %H:%M %z") {
      force(tz)
      force(fmt)
      function(x) {
        xx <- as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
        s <- format(lubridate::with_tz(xx, tz), fmt)
        sub("([+\\-]\\d{2})(\\d{2})$", "\\1:\\2", s) # ±hhmm -> ±hh:mm
      }
    }

    # ---------------- Availability flags ----------------
    has_preview <- reactive({
      si <- shaped_input()
      !is.null(si) && !is.null(si$inputs) && nrow(as.data.frame(si$inputs)) > 0
    })
    has_hourly <- reactive({
      d <- results()
      !is.null(d) && nrow(as.data.frame(d)) > 0
    })

    # ---------------- Source labels/choices (use your keys) ----------------
    source_labels <- reactive({
      c(
        results = i18n_or("data_src_results", "Results (hFWI output)"),
        inputs  = i18n_or("data_src_inputs", "Inputs (weather)")
      )
    })

    observe({
      labs <- source_labels()
      choices_named <- setNames(c("results", "inputs"), c(labs[["results"]], labs[["inputs"]]))
      current <- isolate(input$plot_dataset)
      pick_default <- if (isTRUE(has_hourly())) "results" else if (isTRUE(has_preview())) "inputs" else "inputs"
      sel <- current
      if (is.null(sel) || !(sel %in% c("results", "inputs"))) sel <- pick_default

      updateSelectInput(
        session, "plot_dataset",
        label = i18n_or("data_source", "Data source"),
        choices = choices_named,
        selected = sel
      )
    })

    # Auto-select "results" as soon as hourly data exists after run
    observeEvent(has_hourly(),
      {
        if (isTRUE(has_hourly())) {
          labs <- source_labels()
          choices_named <- setNames(c("results", "inputs"), c(labs[["results"]], labs[["inputs"]]))
          updateSelectInput(session, "plot_dataset", choices = choices_named, selected = "results")
        }
      },
      ignoreInit = TRUE,
      priority = 100
    )

    # Label + tooltip for Y variables
    output$plot_y_multi_label <- renderUI({
      label_with_help(
        i18n_or("plot_vars", "Select variables to plot"),
        i18n_or("tt_plot_vars", "Choose one or more variables to visualize.")
      )
    })

    # ---------------- Data & datetime detection ----------------
    data_for_plot <- reactive({
      req(input$plot_dataset)
      df <- if (identical(input$plot_dataset, "inputs")) {
        si <- shaped_input()
        validate(need(!is.null(si), i18n_or("err_upload_and_map_first", "Upload and map your file first.")))
        as.data.frame(si$inputs)
      } else {
        as.data.frame(results())
      }
      validate(need(NROW(df) > 0, i18n_or("err_dataset_no_rows", "No rows available in the selected dataset.")))

      # Find a datetime-like column (prefer typed POSIXt/Date)
      name_patt <- "(?i)(^datetime$|^timestamp$|date_time|^date$|^time$|valid_time)"
      dt_candidates <- names(df)[grepl(name_patt, names(df), perl = TRUE)]
      dt_col <- character(0)
      if (length(dt_candidates)) {
        typed <- dt_candidates[
          vapply(df[dt_candidates], function(x) inherits(x, c("POSIXt", "Date")), logical(1))
        ]
        dt_col <- if (length(typed)) typed[1] else dt_candidates[1]
      }

      # Build datetime from parts if needed
      if (!length(dt_col) && all(c("year", "month", "day", "hour") %in% names(df))) {
        olson <- tz_reactive() %||% "UTC"
        df$datetime <- lubridate::make_datetime(
          year = as.integer(df$year), month = as.integer(df$month),
          day = as.integer(df$day), hour = as.integer(df$hour),
          tz = olson
        )
        dt_col <- "datetime"
      }
      validate(need(length(dt_col) == 1, i18n_or("err_no_datetime_found", "Could not infer a time column to plot.")))

      # Order by time
      ord <- try(order(df[[dt_col]]), silent = TRUE)
      if (!inherits(ord, "try-error")) df <- df[ord, , drop = FALSE]
      attr(df, "dt_col") <- dt_col
      df
    })

    # ---------------- Preferences & choice population ----------------
    preferred_for <- function(dataset_key) {
      if (identical(dataset_key, "inputs")) {
        c("temp", "rh", "wind", "ws", "rain")
      } else {
        c("ffmc", "dmc", "dc", "fwi") # extend with "isi","bui" if desired
      }
    }
    build_choices <- function(df) {
      dt_col <- attr(df, "dt_col")
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      setdiff(num_cols, dt_col)
    }
    label_choices <- function(cols) {
      labs <- try(label_for_col(cols, type = "short"), silent = TRUE)
      if (inherits(labs, "try-error") || length(labs) != length(cols)) labs <- cols
      stats::setNames(cols, labs)
    }

    # A) When DATASET changes: force dataset-specific defaults
    observeEvent(input$plot_dataset,
      {
        df <- req(data_for_plot())
        dataset_key <- input$plot_dataset %||% "inputs"
        shiny::freezeReactiveValue(input, "plot_y_multi")

        raw_choices <- build_choices(df)
        if (!length(raw_choices)) {
          updateSelectizeInput(session, "plot_y_multi", choices = character(0), selected = character(0))
          return(invisible(NULL))
        }
        prefs <- preferred_for(dataset_key)
        lc <- tolower(raw_choices)
        want_idx <- match(prefs, lc, nomatch = 0)
        want <- raw_choices[want_idx[want_idx > 0]]
        selected <- if (length(want)) want else utils::head(raw_choices, 4)
        updateSelectizeInput(session, "plot_y_multi",
          choices = label_choices(raw_choices),
          selected = unique(selected)
        )
        bump_reseed()
      },
      ignoreInit = TRUE,
      priority = 100
    )

    # B) When DATA changes (same dataset): keep selection if still valid; else defaults
    observeEvent(data_for_plot(),
      {
        df <- req(data_for_plot())
        dataset_key <- isolate(input$plot_dataset %||% "inputs")
        shiny::freezeReactiveValue(input, "plot_y_multi")

        raw_choices <- build_choices(df)
        if (!length(raw_choices)) {
          updateSelectizeInput(session, "plot_y_multi", choices = character(0), selected = character(0))
          return(invisible(NULL))
        }

        existing <- intersect(isolate(input$plot_y_multi %||% character(0)), raw_choices)
        sel <- if (length(existing)) {
          existing
        } else {
          prefs <- preferred_for(dataset_key)
          lc <- tolower(raw_choices)
          want_idx <- match(prefs, lc, nomatch = 0)
          want <- raw_choices[want_idx[want_idx > 0]]
          if (length(want)) want else utils::head(raw_choices, 4)
        }

        updateSelectizeInput(session, "plot_y_multi",
          choices = label_choices(raw_choices),
          selected = unique(sel)
        )
        bump_reseed()
      },
      ignoreInit = FALSE,
      priority = 70
    ) # <-- allow at init

    # C) One-time "first load" seeder (belt and suspenders)
    observeEvent(data_for_plot(),
      {
        df <- req(data_for_plot())
        if (!length(isolate(input$plot_y_multi))) {
          dataset_key <- isolate(input$plot_dataset %||% "inputs")
          raw_choices <- build_choices(df)
          if (length(raw_choices)) {
            prefs <- preferred_for(dataset_key)
            lc <- tolower(raw_choices)
            want_idx <- match(prefs, lc, nomatch = 0)
            want <- raw_choices[want_idx[want_idx > 0]]
            sel <- if (length(want)) want else utils::head(raw_choices, 4)
            updateSelectizeInput(session, "plot_y_multi",
              choices = label_choices(raw_choices),
              selected = unique(sel)
            )
            bump_reseed()
          }
        }
      },
      once = TRUE,
      priority = 95
    )

    # Localize control labels (without resetting selections)
    observe({
      updateSelectInput(session, "plot_dataset",
        label = i18n_or("data_source", "Data source"),
        selected = isolate(input$plot_dataset)
      )
      updateNumericInput(session, "facet_ncol", label = i18n_or("facets_per_row", "Facets per row"))
      updateCheckboxInput(session, "facet_free_y", label = i18n_or("free_y", "Free y–scale per facet"))
    })

    # ---------------- Render Plot (DST-aware axis, self-contained labels) ----
    output$plot_ts <- plotly::renderPlotly({
      reseed_tick()
      df <- data_for_plot()
      dt_col <- attr(df, "dt_col")
      req(length(input$plot_y_multi) >= 1)
      yvars <- unique(input$plot_y_multi)

      keep_cols <- unique(c(dt_col, yvars, "id"))
      keep_cols <- intersect(keep_cols, names(df))
      df_small <- df[, keep_cols, drop = FALSE]

      # Filter to selected vars actually present
      common <- intersect(yvars, names(df_small))
      validate(need(length(common) > 0, i18n_or("err_no_selected_vars_in_dataset", "Selected variables not present in this dataset.")))

      # Long format
      long_df <- df_small |>
        tidyr::pivot_longer(cols = tidyselect::all_of(common), names_to = "variable", values_to = "value") |>
        dplyr::filter(!is.na(.data$value))

      # Variable labels
      var_label_levels <- vapply(common, function(v) label_for_col(v, type = "short"), character(1))
      long_df$var_label <- vapply(
        as.character(long_df$variable),
        function(v) label_for_col(v, type = "short"),
        character(1)
      )
      long_df$var_label <- factor(long_df$var_label, levels = var_label_levels)

      # Legend/source labels (use your i18n keys)
      lab_fwi25 <- i18n_or("legend_fwi25", "FWI25 (hourly)")
      lab_fwi87 <- i18n_or("legend_fwi87", "FWI87 (daily)") # overlay label only
      long_df$source <- lab_fwi25

      # Optional overlay of FWI87 daily when dataset = results
      overlay_df <- NULL
      if (identical(input$plot_dataset, "results")) {
        d87 <- df87()
        if (!is.null(d87) && NROW(d87)) {
          d87 <- as.data.frame(d87)
          dt87 <- if ("datetime" %in% names(d87)) "datetime" else NULL
          if (is.null(dt87) && all(c("year", "month", "day") %in% names(d87))) {
            olson <- tz_reactive() %||% "UTC"
            d87$datetime <- lubridate::make_datetime(d87$year, d87$month, d87$day, hour = 12L, tz = olson)
            dt87 <- "datetime"
          }
          if (!is.null(dt87)) {
            common87 <- intersect(common, intersect(names(d87), names(df)))
            if (length(common87)) {
              keep87 <- unique(c(dt87, common87, "id"))
              d87_small <- d87[, intersect(keep87, names(d87)), drop = FALSE]
              overlay_df <- d87_small |>
                tidyr::pivot_longer(cols = tidyselect::all_of(common87), names_to = "variable", values_to = "value") |>
                dplyr::filter(!is.na(.data$value))
              if (NROW(overlay_df)) {
                overlay_df$var_label <- vapply(
                  as.character(overlay_df$variable),
                  function(v) label_for_col(v, type = "short"),
                  character(1)
                )
                overlay_df$var_label <- factor(overlay_df$var_label, levels = var_label_levels)
                overlay_df$source <- lab_fwi87
              } else {
                overlay_df <- NULL
              }
            }
          }
        }
      }

      # Round numeric for tooltips
      long_df <- dplyr::mutate(long_df, dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))
      if (!is.null(overlay_df)) {
        overlay_df <- dplyr::mutate(overlay_df, dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))
      }

      # Colour mapping per source × station
      get_id_str <- function(d) if ("id" %in% names(d)) as.character(d$id) else "station"
      long_df$series_id <- paste(long_df$source, get_id_str(long_df), sep = "__")
      if (!is.null(overlay_df)) {
        overlay_df$series_id <- paste(overlay_df$source, get_id_str(overlay_df), sep = "__")
      }

      id_vals <- if ("id" %in% names(long_df)) unique(as.character(long_df$id)) else "station"
      n_ids <- length(id_vals)
      n_cols <- max(3, min(8, n_ids))
      pal_fwi25 <- RColorBrewer::brewer.pal(n_cols, "Dark2")[seq_len(n_ids)]
      pal_fwi87 <- RColorBrewer::brewer.pal(n_cols, "Set2")[seq_len(n_ids)]
      lab_fwi25_full <- paste(lab_fwi25, id_vals, sep = "__")
      lab_fwi87_full <- paste(lab_fwi87, id_vals, sep = "__")
      names(pal_fwi25) <- lab_fwi25_full
      names(pal_fwi87) <- lab_fwi87_full
      colour_map <- c(pal_fwi25, pal_fwi87)

      breaks_in_data <- unique(c(long_df$series_id, if (!is.null(overlay_df)) overlay_df$series_id))
      label_series <- function(x) {
        parts <- strsplit(x, "__", fixed = TRUE)[[1]]
        sprintf("%s — %s", parts[2], parts[1])
      }
      label_map <- stats::setNames(vapply(breaks_in_data, label_series, character(1)), breaks_in_data)

      # Title and facets (use your i18n keys)
      ncol_facets <- {
        val <- input$facet_ncol
        if (is.null(val) || is.na(val) || val < 1) 1L else as.integer(val)
      }
      title_txt <- if (length(common) == 1) {
        sprintf(i18n_or("plot_var_over_time", "%s over time"), label_for_col(common[1], type = "short"))
      } else {
        i18n_or("plot_sel_vars_over_time", "Selected variables over time")
      }

      # ----- Axis timezone selection and labeler -----
      display_tz <- display_tz_reactive() # "America/..." or "Etc/GMT±N"
      lbl <- make_datetime_labeler(display_tz, fmt = "%Y-%m-%d %H:%M %z")

      # Build ggplot
      base_aes <- ggplot2::aes(
        x = .data[[attr(df, "dt_col")]], y = .data$value,
        colour = .data$series_id, linetype = .data$source
      )
      p <- ggplot2::ggplot(long_df, base_aes) +
        ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE)
      if (NROW(long_df) < 20000) {
        p <- p + ggplot2::geom_point(size = 0.8, alpha = 0.75, na.rm = TRUE)
      }

      if (!is.null(overlay_df)) {
        p <- p + ggplot2::geom_line(
          data = overlay_df,
          ggplot2::aes(x = .data$datetime, y = .data$value, colour = .data$series_id, linetype = .data$source),
          linewidth = 0.8, na.rm = TRUE, inherit.aes = FALSE
        )
      }

      # Choose correct x scale (datetime vs date)
      is_date <- inherits(df[[attr(df, "dt_col")]], "Date")

      if (is_date) {
        p <- p + ggplot2::scale_x_date(labels = function(x) format(as.Date(x), "%Y-%m-%d"))
      } else {
        # ggplot2 >= 3.5.0 supports 'timezone'; older versions will ignore it harmlessly
        has_tz_param <- "timezone" %in% names(formals(ggplot2::scale_x_datetime))
        if (isTRUE(has_tz_param)) {
          p <- p + ggplot2::scale_x_datetime(timezone = display_tz, labels = lbl)
        } else {
          p <- p + ggplot2::scale_x_datetime(labels = lbl)
        }
      }

      p <- p +
        ggplot2::facet_wrap(
          ~var_label,
          ncol = ncol_facets,
          scales = if (isTRUE(input$facet_free_y)) "free_y" else "fixed"
        ) +
        ggplot2::scale_colour_manual(values = colour_map, breaks = breaks_in_data, labels = label_map) +
        ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "dashed"), c(lab_fwi25, lab_fwi87))) +
        ggplot2::labs(
          x = i18n_or("plot_time_x", "Time"),
          y = NULL,
          title = title_txt, colour = NULL, linetype = NULL
        ) +
        theme_goc()

      # Preserve ggplot's tick labels (with offsets) in plotly
      plotly::ggplotly(p, tooltip = c("x", "y", "colour")) |>
        plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d")) |>
        plotly::plotly_build()
    })
    outputOptions(output, "plot_ts", suspendWhenHidden = FALSE)
  })
}
