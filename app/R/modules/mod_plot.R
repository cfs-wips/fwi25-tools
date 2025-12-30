# =========================
# UI: mod_plot_ui
# =========================
mod_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$section(
      class = "gc-card",
      tags$div(role = "region", `aria-label` = "FWI plots", h4(textOutput(ns("title")))),
      div(
        class = "gc-card__content",
        fluidRow(
          column(4, selectInput(ns("plot_dataset"), label = NULL, choices = c(), selected = NULL)),
          column(
            4, uiOutput(ns("plot_y_multi_label")),
            selectizeInput(ns("plot_y_multi"),
              label = NULL, choices = NULL, multiple = TRUE,
              options = list(placeholder = "")
            )
          ),
          column(2, numericInput(ns("facet_ncol"), label = NULL, value = 2, min = 1, max = 4, step = 1)),
          column(2, checkboxInput(ns("facet_free_y"), label = NULL, value = TRUE))
        ),
        div(
          class = "gc-spin-wrap",
          plotly::plotlyOutput(ns("plot_ts"), height = "80vh")
          # NOTE: Inline JS handlers removed. They now live in app-init.js.
        )
      )
    )
  )
}

# =========================
# SERVER: mod_plot_server
# =========================
mod_plot_server <- function(
  id, tr, lang, label_for_col,
  shaped_input, results, df87,
  tz_reactive, ignore_dst_reactive = reactive(TRUE),
  tab_active = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
    i18n_or <- function(key, default) {
      val <- tryCatch(tr(key), error = function(e) NULL)
      if (is.null(val) || !nzchar(as.character(val)) || grepl("^\\?\\?.*\\?\\?$", val)) default else as.character(val)
    }


    observeEvent(data_for_plot(), {
      cat(
        "[plot] data_for_plot -> dt_col:", attr(data_for_plot(), "dt_col"),
        "; rows:", nrow(data_for_plot()), "\n"
      )
    })
    observeEvent(input$plot_dataset, {
      cat("[plot] dataset:", input$plot_dataset, "\n")
    })
    observeEvent(input$plot_y_multi, {
      cat("[plot] y_multi:", paste(input$plot_y_multi, collapse = ", "), "\n")
    })


    # ---- DST policy helpers ----
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
        tz_standard_offset_hours(tz)
      } else {
        ref <- as.POSIXct("2000-01-15 12:00:00", tz = tz)
        z <- format(ref, "%z")
        sgn <- ifelse(substr(z, 1, 1) == "-", -1, 1)
        hh <- suppressWarnings(as.integer(substr(z, 2, 3)))
        mm <- suppressWarnings(as.integer(substr(z, 4, 5)))
        sgn * (hh + mm / 60)
      }
    }
    parse_z_to_hours_fallback <- function(z) {
      z <- as.character(z)
      sgn <- ifelse(substr(z, 1, 1) == "-", -1, 1)
      hh <- suppressWarnings(as.integer(substr(z, 2, 3)))
      mm <- suppressWarnings(as.integer(substr(z, 4, 5)))
      sgn * (hh + mm / 60)
    }
    compute_x_plot <- function(dt, tz_use, policy_is_std) {
      if (!inherits(dt, "POSIXt")) dt <- as.POSIXct(dt, tz = tz_use)
      if (isTRUE(policy_is_std)) {
        cur_off_h <- if (exists("parse_z_to_hours", mode = "function")) {
          parse_z_to_hours(format(dt, "%z"))
        } else {
          parse_z_to_hours_fallback(format(dt, "%z"))
        }
        std_h <- std_off_hours(tz_use)
        delta_h <- cur_off_h - std_h
        dt - lubridate::dhours(delta_h)
      } else {
        dt
      }
    }

    trace_mode_for <- function(n_rows) if (isTRUE(n_rows <= 20000)) "lines+markers" else "lines"
    trace_marker_for <- function(mode, color, size = 5, opacity = 0.9) {
      if (grepl("markers", mode, fixed = TRUE)) list(color = color, size = size, opacity = opacity) else NULL
    }

    # ---- Robust annotations: coerce axis domains to numeric; skip if not ready ----
    build_annotations <- function(layout, facet_titles, lbl_time) {
      xnames <- grep("^xaxis\\d*$", names(layout), value = TRUE)
      ynames <- gsub("^x", "y", xnames)

      ann <- list()
      n_facets <- length(facet_titles)
      n_axes <- length(xnames)

      # If axes not present, add only global x label and return
      if (n_axes == 0L) {
        ann[[length(ann) + 1]] <- list(
          text = lbl_time,
          x = 0.5, xref = "paper",
          y = -0.12, yref = "paper",
          showarrow = FALSE,
          xanchor = "center", yanchor = "top",
          font = list(size = 14)
        )
        return(ann)
      }

      for (i in seq_len(n_facets)) {
        if (i > n_axes) break
        xn <- xnames[i]
        yn <- ynames[i]
        xd <- layout[[xn]]$domain
        yd <- layout[[yn]]$domain
        xd <- suppressWarnings(as.numeric(unlist(xd)))
        yd <- suppressWarnings(as.numeric(unlist(yd)))
        if (length(xd) < 2L || length(yd) < 2L || any(is.na(xd)) || any(is.na(yd))) next

        ann[[length(ann) + 1]] <- list(
          text = facet_titles[i],
          x = mean(xd), xref = "paper",
          y = yd[2] + 0.04, yref = "paper",
          showarrow = FALSE,
          xanchor = "center", yanchor = "bottom",
          font = list(size = 12)
        )
      }

      ann[[length(ann) + 1]] <- list(
        text = lbl_time,
        x = 0.5, xref = "paper",
        y = -0.12, yref = "paper",
        showarrow = FALSE,
        xanchor = "center", yanchor = "top",
        font = list(size = 14)
      )

      ann
    }

    axis_index <- function(ax) if (ax == "y") 1L else suppressWarnings(as.integer(sub("^y", "", ax)))
    build_hover_template_compact <- function(lbl_series, lbl_station, variable_label, y_digits = 3) {
      sprintf(
        "<b>%s</b>: %%{customdata[0]}<br><b>%s</b>: %%{customdata[1]}<br><b>%s</b>: %%{y:.%df}<extra></extra>",
        lbl_series, lbl_station, variable_label, y_digits
      )
    }
    build_legend_name <- function(station_id, src_type, i18n_or) {
      src_lbl <- if (identical(src_type, "fwi87")) i18n_or("legend_fwi87", "FWI87 (daily)") else i18n_or("legend_fwi25", "FWI25 (hourly)")
      paste0(station_id, " — ", src_lbl)
    }
    make_palettes <- function(id_vals, i18n_or) {
      n_ids <- length(id_vals)
      okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      tableau10 <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")
      if (n_ids == 1L) {
        pal_fwi25 <- c("#56B4E9")
        pal_fwi87 <- c("#E69F00")
      } else {
        combined <- c(okabe_ito, tableau10)
        base_cols <- combined[seq_len(min(n_ids, length(combined)))]
        to_fwi25 <- function(hex) hex
        to_fwi87 <- function(hex, desaturate = 0.20, lighten = 0.18) {
          rv <- grDevices::col2rgb(hex) / 255
          gray <- mean(rv)
          rv <- rv + (gray - rv) * desaturate
          rv <- pmin(rv + lighten, 1)
          grDevices::rgb(rv[1], rv[2], rv[3])
        }
        pal_fwi25 <- vapply(base_cols, to_fwi25, character(1))
        pal_fwi87 <- vapply(base_cols, to_fwi87, character(1))
      }
      names(pal_fwi25) <- paste(i18n_or("legend_fwi25", "FWI25 (hourly)"), id_vals, sep = "__")
      names(pal_fwi87) <- paste(i18n_or("legend_fwi87", "FWI87 (daily)"), id_vals, sep = "__")
      list(fwi25 = pal_fwi25, fwi87 = pal_fwi87, map = c(pal_fwi25, pal_fwi87))
    }

    # ---- UI labels & controls ----
    output$title <- renderText({
      i18n_or("tab_plot", "Plot")
    })
    outputOptions(output, "title", suspendWhenHidden = FALSE)
    output$plot_y_multi_label <- renderUI({
      label_with_help(i18n_or("plot_vars", "Select variables to plot"), i18n_or("tt_plot_vars", "Choose one or more variables to visualize."))
    })
    observe({
      updateNumericInput(session, "facet_ncol", label = i18n_or("plot_facets_ncol_label", "Facets per row"))
      updateCheckboxInput(session, "facet_free_y", label = i18n_or("plot_free_y_label", "Free y–scale per facet"))
    })

    has_preview <- reactive({
      si <- shaped_input()
      !is.null(si) && !is.null(si$inputs) && nrow(as.data.frame(si$inputs)) > 0
    })
    has_hourly <- reactive({
      d <- results()
      !is.null(d) && nrow(as.data.frame(d)) > 0
    })

    source_labels <- reactive({
      list(
        results = i18n_or("data_src_results", "Results (hFWI output)"),
        inputs  = i18n_or("data_src_inputs", "Inputs (weather)")
      )
    })


    update_y_choices <- function(dataset_key, df) {
      # Prevent churn while we rebuild choices/selection
      shiny::freezeReactiveValue(input, "plot_y_multi")

      # Numeric columns minus the datetime column
      raw_choices <- names(df)
      dt_col <- attr(df, "dt_col")
      raw_choices <- setdiff(raw_choices, dt_col)


      # Preferred variables by dataset
      preferred_for <- function(key) {
        if (identical(key, "inputs")) {
          c("temp", "rh", "wind", "ws", "rain")
        } else {
          c("ffmc", "dmc", "dc", "fwi", "isi", "bui")
        }
      }
      prefs <- preferred_for(dataset_key)
      lc <- tolower(raw_choices)
      want <- raw_choices[match(prefs, lc, nomatch = 0L)]
      proposed_selected <- unique(if (length(want)) want else utils::head(raw_choices, 4))

      choices_list <- as.list(raw_choices)
      names(choices_list) <- vapply(
        raw_choices,
        function(v) label_for_col(v, type = "short"),
        FUN.VALUE = character(1)
      )
      updateSelectizeInput(
        session, "plot_y_multi",
        choices = choices_list, # named LIST (values = raw var names; names = labels)
        selected = proposed_selected
      )

      current_sel <- isolate(input$plot_y_multi)
      current_sel <- intersect(current_sel %||% character(0), raw_choices)
      final_sel <- if (length(current_sel)) current_sel else proposed_selected
      updateSelectizeInput(
        session, "plot_y_multi",
        choices = choices_list, # named LIST (values = raw var names; names = labels)
        selected = final_sel
      )
    }

    observe({
      labs <- source_labels()
      choices_named <- setNames(list("results", "inputs"), c(labs$results, labs$inputs))
      current <- isolate(input$plot_dataset)
      pick_default <- if (isTRUE(has_hourly())) "results" else if (isTRUE(has_preview())) "inputs" else "inputs"
      sel <- if (is.null(current) || !(current %in% c("results", "inputs"))) pick_default else current
      updateSelectInput(session, "plot_dataset", label = i18n_or("data_source", "Data source"), choices = choices_named, selected = sel)
    })
    observeEvent(has_hourly(),
      {
        if (isTRUE(has_hourly())) {
          labs <- source_labels()
          choices_named <- setNames(list("results", "inputs"), c(labs[["results"]], labs[["inputs"]]))
          updateSelectInput(session, "plot_dataset", choices = choices_named, selected = "results")
        }
      },
      ignoreInit = F,
      priority = 100
    )

    # ---- Data prep reactive ----
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
      dt_candidates <- names(df)[grepl("(?i)(^datetime$|^timestamp$|date_time|^date$|^time$|valid_time)", names(df), perl = TRUE)]
      dt_col <- if (length(dt_candidates)) {
        typed <- dt_candidates[vapply(df[dt_candidates], function(x) inherits(x, c("POSIXt", "Date")), logical(1))]
        if (length(typed)) typed[1] else dt_candidates[1]
      } else {
        character(0)
      }
      if (!length(dt_col) && all(c("year", "month", "day", "hour") %in% names(df))) {
        olson <- tz_reactive() %||% "UTC"
        df$datetime <- lubridate::make_datetime(year = df$year, month = df$month, day = df$day, hour = df$hour, tz = olson)
        dt_col <- "datetime"
      }
      attr(df, "dt_col") <- dt_col
      df
    })

    # ---- Populate Y choices ----
    preferred_for <- function(dataset_key) {
      if (dataset_key == "inputs") c("temp", "rh", "wind", "ws", "rain") else c("ffmc", "dmc", "dc", "fwi")
    }

    # --- Populate Y choices when dataset changes (inputs vs results) ---

    observeEvent(input$plot_dataset,
      {
        df <- req(data_for_plot())
        update_y_choices(input$plot_dataset, df)
      },
      ignoreInit = FALSE,
      priority = 100
    )

    observeEvent(data_for_plot(),
      {
        req(input$plot_dataset)
        update_y_choices(input$plot_dataset, data_for_plot())
      },
      ignoreInit = FALSE,
      priority = 100
    )
    # ---- Auto-init when Plot tab first opens ----
    plotted_once <- reactiveVal(FALSE)
    observeEvent(results(),
      {
        req(tab_active() == "Plot", !plotted_once())
        d <- results()
        req(!is.null(d), nrow(as.data.frame(d)) > 0)
        if (is.null(input$plot_dataset) || !identical(input$plot_dataset, "results")) updateSelectInput(session, "plot_dataset", selected = "results")

        update_y_choices("results", as.data.frame(d))
        plotted_once(TRUE)
      },
      ignoreInit = FALSE,
      priority = 200
    )

    # ---- RenderPlotly ----
    output$plot_ts <- plotly::renderPlotly({
      df <- data_for_plot()
      dt_col <- attr(df, "dt_col")
      req(length(input$plot_y_multi) >= 1)
      yvars <- unique(input$plot_y_multi)
      keep_cols <- unique(c(dt_col, yvars, "id"))
      keep_cols <- intersect(keep_cols, names(df))
      df_small <- df[, keep_cols, drop = FALSE]
      common <- intersect(yvars, names(df_small))
      validate(need(length(common) > 0, i18n_or("err_no_selected_vars_in_dataset", "Selected variables not present in this dataset.")))
      DT <- data.table::as.data.table(data.table::copy(df_small))
      id_keep <- intersect(c(dt_col, "id"), names(DT))
      long_df <- data.table::melt(DT, id.vars = id_keep, measure.vars = common, variable.name = "variable", value.name = "value", variable.factor = FALSE)[!is.na(value)]
      var_label_levels <- vapply(common, function(v) label_for_col(v, type = "short"), character(1))
      long_df[, var_label := vapply(as.character(variable), function(v) label_for_col(v, type = "short"), character(1))]
      long_df[, var_label := factor(var_label, levels = var_label_levels)]

      # Overlay daily results (FWI87) if available
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
              DT87 <- data.table::as.data.table(data.table::copy(d87_small))
              id_keep87 <- intersect(c("datetime", "id"), names(DT87))
              overlay_df <- data.table::melt(DT87, id.vars = id_keep87, measure.vars = common87, variable.name = "variable", value.name = "value", variable.factor = FALSE)[!is.na(value)]
              if (NROW(overlay_df)) {
                overlay_df[, var_label := vapply(as.character(variable), function(v) label_for_col(v, type = "short"), character(1))]
                overlay_df[, var_label := factor(var_label, levels = var_label_levels)]
              } else {
                overlay_df <- NULL
              }
            }
          }
        }
      }

      # Round numerics for nicer tooltips
      num_cols <- names(long_df)[vapply(long_df, is.numeric, logical(1))]
      if (length(num_cols)) long_df[, (num_cols) := lapply(.SD, function(x) round(x, 3)), .SDcols = num_cols]
      if (!is.null(overlay_df)) {
        num_cols2 <- names(overlay_df)[vapply(overlay_df, is.numeric, logical(1))]
        if (length(num_cols2)) overlay_df[, (num_cols2) := lapply(.SD, function(x) round(x, 3)), .SDcols = num_cols2]
      }

      # Series key (source × station); source labels localized
      lab_fwi25 <- i18n_or("legend_fwi25", "FWI25 (hourly)")
      lab_fwi87 <- i18n_or("legend_fwi87", "FWI87 (daily)")
      long_df[, source := lab_fwi25]
      long_df[, series_id := paste(source, if ("id" %in% names(long_df)) as.character(id) else "station", sep = "__")]
      if (!is.null(overlay_df)) {
        overlay_df[, source := lab_fwi87]
        overlay_df[, series_id := paste(source, if ("id" %in% names(overlay_df)) as.character(id) else "station", sep = "__")]
      }

      # Colors (Okabe–Ito + Tableau fallback; single-station override)
      id_vals <- if ("id" %in% names(long_df)) unique(as.character(long_df$id)) else "station"
      pals <- make_palettes(id_vals, i18n_or)
      colour_map <- pals$map

      # Compute x_plot honoring DST policy
      tz_use <- tz_reactive() %||% "UTC"
      use_std <- normalize_policy(ignore_dst_reactive())
      long_df[, x_plot := compute_x_plot(get(dt_col), tz_use, use_std)]
      if (!is.null(overlay_df)) overlay_df[, x_plot := compute_x_plot(datetime, tz_use, use_std)]

      # Build subplots (facets = variable labels)
      facet_levels <- levels(long_df$var_label) %||% unique(long_df$var_label)
      ncol_facets <- if (is.null(input$facet_ncol) || is.na(input$facet_ncol) || input$facet_ncol < 1) 1L else as.integer(input$facet_ncol)
      nrows <- ceiling(length(facet_levels) / ncol_facets)

      # Localized labels for hover template
      lbl_series <- i18n_or("series", "Series")
      lbl_station <- i18n_or("plot_color_by_station", "Station")
      lbl_time <- i18n_or("plot_time_x", "Time")

      # Legend dedup — show each legend group once
      shown_once <- new.env(parent = emptyenv())
      make_facet_fig <- function(f_label) {
        fig <- plotly::plot_ly()
        # Hourly (FWI25): solid lines + optional markers
        for (idv in id_vals) {
          dd <- long_df[long_df$var_label == f_label & long_df$id == idv, , drop = FALSE]
          if (NROW(dd)) {
            k <- paste(lab_fwi25, idv, sep = "__")
            col <- unname(colour_map[[k]])
            nm <- paste0(idv, " — ", lab_fwi25)
            show_leg <- is.null(shown_once[[k]])
            if (show_leg) shown_once[[k]] <- TRUE
            mode_main <- trace_mode_for(NROW(dd))
            marker_main <- trace_marker_for(mode_main, color = col, size = 5, opacity = 0.9)
            tip <- build_hover_template_compact(lbl_series = lbl_series, lbl_station = lbl_station, variable_label = f_label, y_digits = 3)
            fig <- plotly::add_trace(
              fig,
              x = dd$x_plot, y = dd$value,
              type = "scatter", mode = mode_main,
              name = nm,
              hovertemplate = tip,
              customdata = lapply(seq_len(nrow(dd)), function(i) list(lab_fwi25, idv)),
              line = list(color = col, width = 1.8, dash = "solid"),
              marker = marker_main, # may be NULL when mode="lines"
              legendgroup = k,
              showlegend = show_leg
            )
          }
        }
        # Daily overlay (FWI87): dashed lines, no markers
        if (!is.null(overlay_df)) {
          for (idv in id_vals) {
            dd87 <- overlay_df[overlay_df$var_label == f_label & overlay_df$id == idv, , drop = FALSE]
            if (NROW(dd87)) {
              k <- paste(lab_fwi87, idv, sep = "__")
              col <- unname(colour_map[[k]])
              nm <- paste0(idv, " — ", lab_fwi87)
              show_leg <- is.null(shown_once[[k]])
              if (show_leg) shown_once[[k]] <- TRUE
              tip <- build_hover_template_compact(lbl_series = lbl_series, lbl_station = lbl_station, variable_label = f_label, y_digits = 3)
              fig <- plotly::add_trace(
                fig,
                x = (dd87$x_plot %||% dd87$datetime), y = dd87$value,
                type = "scatter", mode = "lines", # lines only
                name = nm,
                hovertemplate = tip,
                customdata = lapply(seq_len(nrow(dd87)), function(i) list(lab_fwi87, idv)),
                line = list(color = col, width = 1.8, dash = "dash"),
                legendgroup = k,
                showlegend = show_leg
              )
            }
          }
        }
        # Facet axes; global x label handled by annotations (subplot uses titleX = FALSE)
        fig <- plotly::layout(
          fig,
          yaxis = list(title = f_label, automargin = TRUE, title = list(standoff = 12)),
          xaxis = list(title = lbl_time, tickformat = "%Y-%m-%d", automargin = TRUE)
        )

        fig
      }

      figs <- lapply(facet_levels, make_facet_fig)

      sp <- do.call(
        plotly::subplot,
        c(figs,
          nrows = nrows, shareX = FALSE, shareY = isFALSE(input$facet_free_y),
          titleX = FALSE, titleY = TRUE, margin = 0.06
        ) # was 0.03
      )


      # Global plot title (localized at render time)
      title_txt <- if (length(common) == 1) {
        sprintf(i18n_or("plot_var_over_time", "%s over time"), label_for_col(common[1], type = "short"))
      } else {
        i18n_or("plot_sel_vars_over_time", "Selected variables over time")
      }

      sp <- plotly::layout(
        sp,
        title = list(text = title_txt, pad = list(t = 6, b = 6)),
        legend = list(orientation = "h", x = 0, y = -0.15),
        hovermode = "x unified",
        uirevision = "keep-zoom",
        # extra breathing room around the whole plot
        margin = list(t = 90, b = 90, l = 70, r = 30)
      )
      sp <- plotly::config(sp, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d"))


      sp
    })

    # ---- Proxy updates for language (no redraw) ----
    # observeEvent(list(lang()), # tab_active()),
    #   {
    #     # req(tab_active() == "Plot")
    #     # Request layout & trace meta from browser using NAMESPACED ID (module-safe)
    #     session$sendCustomMessage("getPlotlyLayout", list(id = session$ns("plot_ts")))
    #     session$sendCustomMessage("getPlotlyTraces", list(id = session$ns("plot_ts")))
    #   },
    #   ignoreInit = TRUE
    # )

    observeEvent(list(input$plot_ts_layout, input$plot_ts_traces),
      {
        req(input$plot_ts_layout, input$plot_ts_traces)

        update_plot_language <- function(layout, traces_meta) {
          # Localized labels (ensure they exist in this scope)
          lbl_series <- i18n_or("series", "Series")
          lbl_station <- i18n_or("plot_color_by_station", "Station")
          lbl_time <- i18n_or("plot_time_x", "Time")

          facet_titles <- vapply(input$plot_y_multi, function(v) label_for_col(v, "short"), character(1))
          new_title <- if (length(input$plot_y_multi) == 1) {
            sprintf(i18n_or("plot_var_over_time", "%s over time"), label_for_col(input$plot_y_multi[1], "short"))
          } else {
            i18n_or("plot_sel_vars_over_time", "Selected variables over time")
          }
          relayout_payload <- list("title.text" = new_title, "_dummy" = as.integer(Sys.time()), "uirevision" = as.integer(Sys.time()))
          # If axes missing, skip facet annotations for now
          if (!length(grep("^xaxis\\d*$", names(layout), value = TRUE))) {
            relayout_payload <- list("title.text" = new_title, "_dummy" = as.integer(Sys.time()))
          } else {
            relayout_payload <- list(
              "title.text"  = new_title,
              "annotations" = build_annotations(layout, facet_titles, lbl_time),
              "_dummy"      = as.integer(Sys.time()) # force layout update
            )
          }

          y_axes <- grep("^yaxis\\d*$", names(input$plot_ts_layout), value = TRUE)
          x_axes <- sub("^y", "x", y_axes)

          for (i in seq_along(y_axes)) {
            relayout_payload[[paste0(y_axes[i], ".title.text")]] <- facet_titles[i]
            relayout_payload[[paste0(y_axes[i], ".title.standoff")]] <- 12
            relayout_payload[[paste0(x_axes[i], ".title.text")]] <- lbl_time
            relayout_payload[[paste0(x_axes[i], ".title.standoff")]] <- 8
          }

          # for (i in seq_along(input$plot_y_multi)) {
          #   relayout_payload[[paste0("yaxis", if (i > 1) i, ".title.text")]] <- label_for_col(input$plot_y_multi[i], "short")
          #   relayout_payload[[paste0("yaxis", if (i > 1) i, ".title.standoff")]] <- 12
          #   # NEW: also update x-axis titles on language change
          #   relayout_payload[[paste0("xaxis", if (i > 1) i, ".title.text")]] <- lbl_time
          #   relayout_payload[[paste0("xaxis", if (i > 1) i, ".title.standoff")]] <- 8
          # }


          templates <- vapply(seq_along(traces_meta$indices), function(i) {
            fi <- axis_index(traces_meta$yaxis[i])
            vlab <- if (!is.na(fi) && fi >= 1L && fi <= length(facet_titles)) facet_titles[fi] else i18n_or("variable", "Variable")
            build_hover_template_compact(lbl_series = lbl_series, lbl_station = lbl_station, variable_label = vlab, y_digits = 3)
          }, character(1))
          names_vec <- mapply(build_legend_name, traces_meta$id, traces_meta$srcType, MoreArgs = list(i18n_or = i18n_or))
          names_list <- as.list(unname(names_vec))
          templates_list <- as.list(unname(templates))
          idx <- unname(as.integer(traces_meta$indices))
          idx_list <- as.list(idx)
          traces_meta$id <- unname(traces_meta$id)
          traces_meta$srcType <- unname(traces_meta$srcType)
          traces_meta$yaxis <- unname(traces_meta$yaxis)

          px <- plotly::plotlyProxy("plot_ts", session)
          px |> plotly::plotlyProxyInvoke("relayout", relayout_payload)
          px |> plotly::plotlyProxyInvoke("restyle", list(name = names_list), idx_list)
          px |> plotly::plotlyProxyInvoke("restyle", list(hovertemplate = templates_list), idx_list)
        }

        update_plot_language(input$plot_ts_layout, input$plot_ts_traces)
      },
      ignoreInit = TRUE
    )

    # # Ensure meta is requested after renders/flushes (Shiny, Shinylive/WebR)
    # session$onFlushed(function() {
    #   session$sendCustomMessage("getPlotlyLayout", list(id = session$ns("plot_ts")))
    #   session$sendCustomMessage("getPlotlyTraces", list(id = session$ns("plot_ts")))
    # }, once = T)

    outputOptions(output, "plot_ts", suspendWhenHidden = FALSE)
  })
}
