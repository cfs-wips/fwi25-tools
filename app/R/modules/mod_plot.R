# R/modules/mod_plot.R
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
              selected = "inputs" # start with inputs before first run
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
        # Spinner overlay (pure CSS; no dependencies)
        div(
          class = "gc-spin-wrap",
          plotly::plotlyOutput(ns("plot_ts"), height = "80vh"),
          div(class = "gc-spin-overlay",
              div(class = "gc-spinner", `aria-hidden` = "true"),
              span(class = "sr-only", "Loading…")
          )
        )
      )
    )
  )
}

#' Plot module server
#'
#' @param tr translator function
#' @param i18n i18n object (for language reactive)
#' @param label_for_col function mapping column names to localized labels
#' @param shaped_input reactive: PREVIEW inputs (pre-run)
#' @param results reactive: hourly hFWI results (post-run)
#' @param df87 reactive: daily FWI87 results (overlay only; never selectable)
#' @param tz_reactive reactive( character(1) Olson TZ ), e.g., tz$tz_use
#' @param ignore_dst_reactive reactive policy (TRUE => standard offset \n FALSE => civil/DST)
#' @param tab_active optional reactive() returning active tab value (e.g., input$main_tabs)
mod_plot_server <- function(
    id, tr, i18n, label_for_col,
    shaped_input, results, df87,
    tz_reactive, ignore_dst_reactive = reactive(TRUE),
    tab_active = reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
    
    i18n_or <- function(key, default) {
      val <- tryCatch(tr(key), error = function(e) NULL)
      if (is.null(val)) return(default)
      val_chr <- as.character(val)
      if (!length(val_chr) || !nzchar(val_chr)) return(default)
      if (grepl("^\\?\\?.*\\?\\?$", val_chr)) return(default)
      val_chr
    }
    
    # force redraw after programmatic seeding of variables
    reseed_tick <- reactiveVal(0L)
    bump_reseed <- function() reseed_tick(isolate(reseed_tick()) + 1L)
    
    # Title
    output$title <- renderText({ i18n_or("tab_plot", "Plot") })
    outputOptions(output, "title", suspendWhenHidden = FALSE)
    
    # ---- TZ / DST helpers
    normalize_policy <- function(val) {
      tolower_chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; tolower(trimws(x)) }
      if (is.logical(val)) return(isTRUE(val))
      if (is.numeric(val)) return(isTRUE(val == 1))
      if (is.character(val)) {
        v <- tolower_chr(val)
        if (v %in% c("standard","ignore","ignore_dst","fixed","std","no_dst","offset_fixed")) return(TRUE)
        if (v %in% c("civil","dst","from_data","data","olson","local","use_dst")) return(FALSE)
      }
      FALSE
    }
    std_off_hours <- function(tz) {
      if (exists("tz_standard_offset_hours", mode = "function")) {
        tz_standard_offset_hours(tz)
      } else {
        ref <- as.POSIXct("2000-01-15 12:00:00", tz = tz)
        z <- format(ref, "%z")
        sgn <- ifelse(substr(z,1,1)=="-", -1, 1)
        hh <- suppressWarnings(as.integer(substr(z,2,3)))
        mm <- suppressWarnings(as.integer(substr(z,4,5)))
        sgn * (hh + mm/60)
      }
    }
    parse_z_to_hours_fallback <- function(z) {
      z <- as.character(z)
      sgn <- ifelse(substr(z,1,1)=="-", -1, 1)
      hh <- suppressWarnings(as.integer(substr(z,2,3)))
      mm <- suppressWarnings(as.integer(substr(z,4,5)))
      sgn * (hh + mm/60)
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
    
    # ---- Availability flags
    has_preview <- reactive({
      si <- shaped_input()
      !is.null(si) && !is.null(si$inputs) && nrow(as.data.frame(si$inputs)) > 0
    })
    has_hourly <- reactive({
      d <- results()
      !is.null(d) && nrow(as.data.frame(d)) > 0
    })
    
    # ---- Source labels / choices
    source_labels <- reactive({
      c(
        results = i18n_or("data_src_results", "Results (hFWI output)"),
        inputs  = i18n_or("data_src_inputs",  "Inputs (weather)")
      )
    })
    observe({
      labs <- source_labels()
      choices_named <- setNames(c("results","inputs"),
                                c(labs[["results"]], labs[["inputs"]]))
      current <- isolate(input$plot_dataset)
      pick_default <- if (isTRUE(has_hourly())) "results" else if (isTRUE(has_preview())) "inputs" else "inputs"
      sel <- current
      if (is.null(sel) || !(sel %in% c("results","inputs"))) sel <- pick_default
      updateSelectInput(
        session, "plot_dataset",
        label = i18n_or("data_source", "Data source"),
        choices = choices_named,
        selected = sel
      )
    })
    # Flip to "results" when they first appear
    observeEvent(has_hourly(), {
      if (isTRUE(has_hourly())) {
        labs <- source_labels()
        choices_named <- setNames(c("results","inputs"),
                                  c(labs[["results"]], labs[["inputs"]]))
        updateSelectInput(session, "plot_dataset", choices = choices_named, selected = "results")
        bump_reseed()
      }
    }, ignoreInit = TRUE, priority = 100)
    
    # Y variables label
    output$plot_y_multi_label <- renderUI({
      label_with_help(
        i18n_or("plot_vars", "Select variables to plot"),
        i18n_or("tt_plot_vars", "Choose one or more variables to visualize.")
      )
    })
    
    # ---- Data & datetime detection
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
      # find datetime-like column
      name_patt <- "(?i)(^datetime$|^timestamp$|date_time|^date$|^time$|valid_time)"
      dt_candidates <- names(df)[grepl(name_patt, names(df), perl = TRUE)]
      dt_col <- character(0)
      if (length(dt_candidates)) {
        typed <- dt_candidates[
          vapply(df[dt_candidates], function(x) inherits(x, c("POSIXt","Date")), logical(1))
        ]
        dt_col <- if (length(typed)) typed[1] else dt_candidates[1]
      }
      # Build datetime from Y/M/D/H when needed
      if (!length(dt_col) && all(c("year","month","day","hour") %in% names(df))) {
        olson <- tz_reactive() %||% "UTC"
        df$datetime <- lubridate::make_datetime(
          year = as.integer(df$year), month = as.integer(df$month),
          day  = as.integer(df$day),  hour  = as.integer(df$hour),
          tz = olson
        )
        dt_col <- "datetime"
      }
      validate(need(length(dt_col) == 1, i18n_or("err_no_datetime_found", "Could not infer a time column to plot.")))
      ord <- try(order(df[[dt_col]]), silent = TRUE)
      if (!inherits(ord, "try-error")) df <- df[ord, , drop = FALSE]
      attr(df, "dt_col") <- dt_col
      df
    })
    
    # ---- Choice population & seeding
    preferred_for <- function(dataset_key) {
      if (identical(dataset_key, "inputs")) {
        c("temp","rh","wind","ws","rain")
      } else {
        c("ffmc","dmc","dc","fwi") # extend with "isi","bui" if desired
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
    observeEvent(input$plot_dataset, {
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
      updateSelectizeInput(
        session, "plot_y_multi",
        choices = label_choices(raw_choices),
        selected = unique(selected)
      )
      bump_reseed()
    }, ignoreInit = TRUE, priority = 100)
    observeEvent(data_for_plot(), {
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
      updateSelectizeInput(
        session, "plot_y_multi",
        choices = label_choices(raw_choices),
        selected = unique(sel)
      )
      bump_reseed()
    }, ignoreInit = FALSE, priority = 70)
    observeEvent(data_for_plot(), {
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
    }, once = TRUE, priority = 95)
    
    # ---- Localize facet control labels
    observe({
      updateSelectInput(session, "plot_dataset",
                        label = i18n_or("data_source", "Data source"),
                        selected = isolate(input$plot_dataset)
      )
      updateNumericInput(session, "facet_ncol", label = i18n_or("plot_facets_ncol_label", "Facets per row"))
      updateCheckboxInput(session, "facet_free_y", label = i18n_or("plot_free_y_label", "Free y–scale per facet"))
    })
    
    # ---- ONE-TIME auto-init when Plot tab is first opened and results exist
    plotted_once <- reactiveVal(FALSE)
    observeEvent(list(tab_active(), results()), {
      req(tab_active() == "Plot")
      req(!plotted_once())
      d <- results()
      req(!is.null(d), nrow(as.data.frame(d)) > 0)
      # Ensure dataset is 'results'
      if (is.null(input$plot_dataset) || !identical(input$plot_dataset, "results")) {
        updateSelectInput(session, "plot_dataset", selected = "results")
      }
      # Seed sensible defaults if none yet selected
      if (is.null(input$plot_y_multi) || length(input$plot_y_multi) == 0) {
        cols <- names(as.data.frame(d))
        pref_ci <- c("ffmc","dmc","dc","fwi","isi","bui")
        pick <- cols[tolower(cols) %in% pref_ci]
        if (length(pick) == 0) {
          num_cols <- cols[vapply(as.data.frame(d)[cols], is.numeric, logical(1))]
          pick <- head(setdiff(num_cols, c("datetime","timestamp","id","tz","timezone")), 4)
        }
        if (length(pick) > 0) {
          updateSelectizeInput(session, "plot_y_multi", selected = unique(pick))
        }
      }
      bump_reseed()
      plotted_once(TRUE)
    }, ignoreInit = FALSE, priority = 200)
    
    # ================= Render Plotly (native) =================
    # Helper: add facet titles as annotations above each subplot panel
    add_facet_titles <- function(sp, facet_titles) {
      lay <- sp$x$layout
      xnames <- grep("^xaxis\\d*$", names(lay), value = TRUE)
      ynames <- gsub("^x", "y", xnames)
      ann <- list()
      for (i in seq_along(facet_titles)) {
        xn <- xnames[i]; yn <- ynames[i]
        if (is.null(lay[[xn]]$domain) || is.null(lay[[yn]]$domain)) next
        xd <- lay[[xn]]$domain; yd <- lay[[yn]]$domain
        ann[[length(ann) + 1L]] <- list(
          text = facet_titles[i],
          x = mean(xd), xref = "paper",
          y = yd[2] + 0.04, yref = "paper",
          showarrow = FALSE,
          xanchor = "center", yanchor = "bottom",
          font = list(size = 12, family = "sans")
        )
      }
      sp <- plotly::layout(sp, annotations = c(lay$annotations %||% list(), ann), margin = list(t = 80))
      sp
    }
    
    output$plot_ts <- plotly::renderPlotly({
      reseed_tick() # force redraw after seeding
      
      df <- data_for_plot()
      dt_col <- attr(df, "dt_col")
      req(length(input$plot_y_multi) >= 1)
      yvars <- unique(input$plot_y_multi)
      keep_cols <- unique(c(dt_col, yvars, "id"))
      keep_cols <- intersect(keep_cols, names(df))
      df_small <- df[, keep_cols, drop = FALSE]
      common <- intersect(yvars, names(df_small))
      validate(need(length(common) > 0,
                    i18n_or("err_no_selected_vars_in_dataset", "Selected variables not present in this dataset.")
      ))
      
      # -------- Long format via data.table::melt (replacing tidyr::pivot_longer + dplyr::filter)
      DT <- data.table::as.data.table(data.table::copy(df_small))
      id_keep <- intersect(c(dt_col, "id"), names(DT))
      long_df <- data.table::melt(
        DT,
        id.vars       = id_keep,
        measure.vars  = common,
        variable.name = "variable",
        value.name    = "value",
        variable.factor = FALSE
      )
      long_df <- long_df[!is.na(value)]
      
      # Variable labels (facet headers, localized via your helper)
      var_label_levels <- vapply(common, function(v) label_for_col(v, type = "short"), character(1))
      long_df[, var_label := vapply(as.character(variable), function(v)
        label_for_col(v, type = "short"), character(1)) ]
      long_df[, var_label := factor(var_label, levels = var_label_levels)]
      
      # Legend/source labels
      lab_fwi25 <- i18n_or("legend_fwi25", "FWI25 (hourly)")
      lab_fwi87 <- i18n_or("legend_fwi87", "FWI87 (daily)")
      long_df[, source := lab_fwi25]
      
      # Optional overlay (dataset == "results")
      overlay_df <- NULL
      if (identical(input$plot_dataset, "results")) {
        d87 <- df87()
        if (!is.null(d87) && NROW(d87)) {
          d87 <- as.data.frame(d87)
          dt87 <- if ("datetime" %in% names(d87)) "datetime" else NULL
          if (is.null(dt87) && all(c("year","month","day") %in% names(d87))) {
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
              id_keep87 <- intersect(c("datetime","id"), names(DT87))
              overlay_df <- data.table::melt(
                DT87,
                id.vars       = id_keep87,
                measure.vars  = common87,
                variable.name = "variable",
                value.name    = "value",
                variable.factor = FALSE
              )
              overlay_df <- overlay_df[!is.na(value)]
              if (NROW(overlay_df)) {
                overlay_df[, var_label := vapply(as.character(variable),
                                                 function(v) label_for_col(v, type = "short"), character(1))]
                overlay_df[, var_label := factor(var_label, levels = var_label_levels)]
                overlay_df[, source := lab_fwi87]
              } else {
                overlay_df <- NULL
              }
            }
          }
        }
      }
      
      # Round numeric for tooltips (cosmetic)
      num_cols <- names(long_df)[vapply(long_df, is.numeric, logical(1))]
      if (length(num_cols)) {
        long_df[, (num_cols) := lapply(.SD, function(x) round(x, 3)), .SDcols = num_cols]
      }
      if (!is.null(overlay_df)) {
        num_cols2 <- names(overlay_df)[vapply(overlay_df, is.numeric, logical(1))]
        if (length(num_cols2)) {
          overlay_df[, (num_cols2) := lapply(.SD, function(x) round(x, 3)), .SDcols = num_cols2]
        }
      }
      
      # Series key per source × station
      get_id_str <- function(d) if ("id" %in% names(d)) as.character(d$id) else "station"
      long_df[, series_id := paste(source, get_id_str(.SD), sep = "__")]
      if (!is.null(overlay_df)) overlay_df[, series_id := paste(source, get_id_str(.SD), sep = "__")]
      
      # Okabe–Ito palette per station + lightened overlay
      id_vals <- if ("id" %in% names(long_df)) unique(as.character(long_df$id)) else "station"
      n_ids <- length(id_vals)
      okabe_ito <- grDevices::palette("Okabe-Ito")
      base_cols <- if (n_ids <= length(okabe_ito)) okabe_ito[seq_len(n_ids)] else {
        c(okabe_ito, grDevices::palette("Tableau 10"))[n_ids]
      }
      to_fwi25 <- function(hex) hex
      to_fwi87 <- function(hex, desaturate_factor = 0.2, lighten_factor = 0.18) {
        rgb_vals <- col2rgb(hex) / 255
        gray <- mean(rgb_vals)
        rgb_vals <- rgb_vals + (gray - rgb_vals) * desaturate_factor
        rgb_vals <- pmin(rgb_vals + lighten_factor, 1)
        grDevices::rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3])
      }
      if (n_ids == 1L) {
        pal_fwi25 <- c("#E69F00")
        pal_fwi87 <- c("#56B4E9")
      } else {
        pal_fwi25 <- vapply(base_cols, to_fwi25, character(1))
        pal_fwi87 <- vapply(base_cols, to_fwi87, character(1))
      }
      names(pal_fwi25) <- paste(lab_fwi25, id_vals, sep = "__")
      names(pal_fwi87) <- paste(lab_fwi87, id_vals, sep = "__")
      colour_map <- c(pal_fwi25, pal_fwi87)
      
      # Compute x_plot honouring DST policy
      tz_use <- tz_reactive() %||% "UTC"
      use_std <- normalize_policy(ignore_dst_reactive())
      x_col <- attr(df, "dt_col")
      long_df[, x_plot := compute_x_plot(get(x_col), tz_use, use_std)]
      if (!is.null(overlay_df)) overlay_df[, x_plot := compute_x_plot(datetime, tz_use, use_std)]
      
      # Build subplots by facet (var_label) with a single global legend
      ncol_facets <- {
        val <- input$facet_ncol
        if (is.null(val) || is.na(val) || val < 1) 1L else as.integer(val)
      }
      facet_levels <- levels(long_df$var_label)
      if (is.null(facet_levels)) facet_levels <- unique(long_df$var_label)
      n_facets <- length(facet_levels)
      nrows <- ceiling(n_facets / ncol_facets)
      
      # Tooltip labels
      lbl_series  <- i18n_or("series", "Series")
      lbl_station <- i18n_or("plot_color_by_station", "Station")
      lbl_time    <- i18n_or("plot_time_x", "Time")
      
      shown_once <- new.env(parent = emptyenv())
      make_facet_fig <- function(f_label) {
        sub_main <- long_df[long_df$var_label == f_label, , drop = FALSE]
        sub_ovl  <- if (!is.null(overlay_df)) overlay_df[overlay_df$var_label == f_label, , drop = FALSE] else NULL
        
        add_traces_for <- function(fig, dat) {
          if (is.null(dat) || !NROW(dat)) return(fig)
          keys <- unique(dat$series_id)
          for (k in keys) {
            dd <- dat[dat$series_id == k, , drop = FALSE]
            s  <- unique(dd$source)[1]
            id <- if ("id" %in% names(dd)) unique(dd$id)[1] else "station"
            nm <- paste0(id, " — ", s)
            col <- unname(colour_map[[k]])
            dash <- if (identical(s, lab_fwi25)) "solid" else "dash"
            mode <- if (NROW(dd) <= 20000) "lines+markers" else "lines"
            show_leg <- is.null(shown_once[[k]])
            if (show_leg) shown_once[[k]] <- TRUE
            tip <- sprintf(
              "<b>%s</b>: %s<br><b>%s</b>: %s<br><b>%s</b>: %s<br><b>%s</b>: %s",
              lbl_series, s,
              lbl_station, id,
              lbl_time, format(dd$x_plot, "%Y-%m-%d %H:%M"),
              f_label, formatC(dd$value, digits = 6, format = "fg")
            )
            fig <- plotly::add_trace(
              fig,
              x = dd$x_plot, y = dd$value,
              type = "scatter", mode = mode,
              name = nm,
              text = tip, hoverinfo = "text",
              line = list(color = col, width = 1.8, dash = dash),
              marker = list(color = col, size = 5, opacity = 0.9),
              legendgroup = k,
              showlegend = show_leg
            )
          }
          fig
        }
        
        fig <- plotly::plot_ly()
        fig <- add_traces_for(fig, sub_main)
        fig <- add_traces_for(fig, sub_ovl)
        fig <- plotly::layout(
          fig,
          yaxis = list(title = f_label),
          xaxis = list(
            title = i18n_or("plot_time_x", "Time"),
            tickformat = "%Y-%m-%d %H:%M"
          )
        )
        fig
      }
      
      figs <- lapply(facet_levels, make_facet_fig)
      sp <- do.call(
        plotly::subplot,
        c(figs,
          nrows = nrows,
          shareX = TRUE,
          shareY = isFALSE(input$facet_free_y),
          titleX = TRUE,
          titleY = TRUE,
          margin = 0.03
        )
      )
      title_txt <- if (length(common) == 1) {
        sprintf(i18n_or("plot_var_over_time", "%s over time"),
                label_for_col(common[1], type = "short"))
      } else {
        i18n_or("plot_sel_vars_over_time", "Selected variables over time")
      }
      sp <- plotly::layout(
        sp,
        title = list(text = title_txt),
        legend = list(orientation = "h", x = 0, y = -0.15), # single horizontal legend
        hovermode = "x unified"
      )
      sp <- plotly::config(sp, displaylogo = FALSE,
                           modeBarButtonsToRemove = c("select2d","lasso2d"))
      sp <- add_facet_titles(sp, facet_titles = facet_levels)
      sp
    })
    outputOptions(output, "plot_ts", suspendWhenHidden = FALSE)
  })
}
