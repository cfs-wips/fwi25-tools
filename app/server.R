
# server.R
library(shiny)

# ---- Load NG-CFFDRS code vendored at build time
source("ng/util.r", local = TRUE)
source("ng/make_inputs.r", local = TRUE)
source("ng/NG_FWI.r", local = TRUE)
source("ng/old_cffdrs.r", local = TRUE)

server <- function(input, output, session) {
  # --- Simple GoC theme for ggplot (uses GCDS colours/fonts) ---
  theme_goc <- function(base_size = 12, base_family = "Noto Sans") {
    primary <- "#26374A" # GCDS primary background
    border <- "#7D828B"  # GCDS border default
    textcol <- "#333333" # GCDS text primary
    stripbg <- "#F1F2F3" # GCDS light background
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", colour = textcol),
        plot.subtitle = ggplot2::element_text(colour = textcol),
        plot.caption = ggplot2::element_text(colour = textcol),
        axis.title = ggplot2::element_text(face = "bold", colour = textcol),
        axis.text = ggplot2::element_text(colour = textcol),
        panel.grid.minor= ggplot2::element_blank(),
        panel.grid.major= ggplot2::element_line(colour = border, linewidth = 0.25),
        strip.text = ggplot2::element_text(face = "bold", colour = textcol, family = "Lato"),
        strip.background= ggplot2::element_rect(fill = stripbg, colour = border, linewidth = 0.5)
      )
  }
  scale_colour_goc <- function(...) ggplot2::scale_colour_manual(values = c("#26374A"), ...)
  scale_fill_goc   <- function(...) ggplot2::scale_fill_manual(values = c("#26374A"), ...)

  # ---------- Helpers / i18n ----------
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

  labels <- list(
    en = list(
      title = "Hourly FWI (NG‑CFFDRS)",
      upload_csv = "Upload weather CSV",
      csv_has_header = "CSV has header",
      csv_button_label = "Browse...",
      csv_place_holder = "No file selected",
      column_mapping = "Column mapping",
      time_zone = "Time zone",
      tz_fixed_one = "Specify one time zone for all rows",
      tz_auto_infer = "Infer a single time zone from latitude/longitude",
      tz_select = "Time zone",
      tz_offset_policy = "Offset for solar calculations",
      tz_offset_std = "Standard (ignore DST)",
      tz_offset_modal = "From data (modal; may include DST)",
      filter = "Filter",
      drop_rows_prior = "Drop rows prior to (local date):",
      drop_rows_help = "If set, rows with local date < this value are removed before hFWI().",
      init_ffmc = "Initial FFMC",
      init_dmc = "Initial DMC",
      init_dc = "Initial DC",
      run_hfwi = "Run hFWI()",
      download_results = "Download results (CSV)",
      tab_output = "Output",
      tab_plot = "Plot",
      tab_log = "Log",
      data_source = "Data source",
      data_src_results = "Results (hFWI output)",
      data_src_inputs = "Inputs (weather)",
      vars_to_plot = "Variables to plot (facetted)",
      vars_placeholder = "Select one or more numeric columns…",
      facets_per_row = "Facets per row",
      free_y = "Free y‑scale per facet",
      mapping_help = "Map your columns to what NG‑CFFDRS expects. Provide either a single Date‑Time column, or Year/Month/Day/Hour.",
      col_datetime = "Date‑Time (optional)",
      col_year = "Year",
      col_month = "Month",
      col_day = "Day",
      col_hour = "Hour (0–23)",
      col_temp = "Temperature (°C)",
      col_rh = "RH (%)",
      col_ws = "Wind (km/h)",
      col_rain = "Rain (mm)",
      col_lat = "Latitude (deg)",
      col_lon = "Longitude (deg)",
      modal_title      = "Notice!",
      modal_close      = "Dismiss",
      modal_body_html  = paste0(
        "<p>This small R Shiny application uses the NG FWI code from the public repository ",
        "<a href=\"https://github.com/nrcan-cfs-fire/cffdrs-ng/tree/main\" target=\"_blank\" rel=\"noopener noreferrer\">cffdrs-ng</a> ",
        "on GitHub to generate hourly Fire Weather Index (FWI2025) outputs. ",
        "Users can upload an hourly weather file and specify key inputs—such as start-up codes and start date—to calculate FWI2025 values.</p>",
        "<p>The app is still in development. Code updates, input handling, and output formatting may change as the FWI2025 system continues to evolve. ",
        "Feedback is welcome, and users should anticipate potential bugs and issues, as the code and application are under development. ",
        "<strong>This should not be utilized for operational purposes and is intended as an exploratory tool.</strong></p>",
        "<p><strong>This application uses Microsoft Copilot, an AI-powered conversational assistant based on the GPT‑5 model, designed to help with information, code generation, and productivity tasks. Version: Copilot (GPT‑5, August 2025).</strong></p>"
      ),
      plot_time_x = "Time",
      plot_sel_vars_over_time = "Selected variables over time",
      plot_var_over_time = "%s over time",
      dt_btn_copy = "Copy",
      dt_btn_csv = "CSV",
      dt_btn_excel = "Excel",
      dt_sSearch = "Search:",
      dt_sLength = "Show _MENU_ entries",
      dt_sInfo = "Showing _START_ to _END_ of _TOTAL_ entries",
      dt_sInfoEmpty= "Showing 0 to 0 of 0 entries",
      dt_sInfoFilt = "(filtered from _MAX_ total entries)",
      dt_sZero = "No matching records found",
      dt_sProc = "Processing...",
      dt_pag_first = "First",
      dt_pag_prev = "Previous",
      dt_pag_next = "Next",
      dt_pag_last = "Last",
      aliases_short = c(
        ffmc = "FFMC",
        dmc  = "DMC",
        dc   = "DC",
        isi  = "ISI",
        bui  = "BUI",
        fwi  = "FWI",
        dsr  = "DSR"
      ),
      aliases_long = c(
        ffmc = "Fine Fuel Moisture Code (FFMC)",
        dmc  = "Duff Moisture Code (DMC)",
        dc   = "Drought Code (DC)",
        isi  = "Initial Spread Index (ISI)",
        bui  = "Build‑Up Index (BUI)",
        fwi  = "Fire Weather Index (FWI)",
        dsr  = "Daily Severity Rating (DSR)"
      ),
      # New i18n strings
      skip_to_main       = "Skip to main content",
      aria_app_label     = "Hourly FWI application",
      aria_tabs_label    = "Primary output tabs",
      aria_run_label     = "Run hFWI",
      tz_not_inferred    = "Timezone not yet inferred.",
      err_ffmc_range     = "FFMC must be between 0 and 101.",
      err_dmc_range      = "DMC must be a non‑negative number.",
      err_dc_range       = "DC must be a non‑negative number.",
      err_non_numeric_cols = "These columns are not numeric: %s",
      err_tz_invalid     = "The selected time zone is not available on this system.",
      iana_prefix        = "IANA timezone:"
    ),
    fr = list(
      title = "IFF horaire (NG‑CFFDRS)",
      upload_csv = "Téléverser le fichier CSV météo",
      csv_has_header = "Le CSV comporte une ligne d’en‑tête",
      csv_button_label = "Parcourir...",
      csv_place_holder = "Aucun fichier sélectionné",
      column_mapping = "Correspondance des colonnes",
      time_zone = "Fuseau horaire",
      tz_fixed_one = "Utiliser un seul fuseau horaire pour toutes les lignes",
      tz_auto_infer = "Déduire un fuseau horaire à partir de la latitude/longitude",
      tz_select = "Fuseau horaire",
      tz_offset_policy = "Décalage pour les calculs solaires",
      tz_offset_std = "Standard (ignorer l’heure d’été)",
      tz_offset_modal = "À partir des données (modal; peut inclure l’heure d’été)",
      filter = "Filtre",
      drop_rows_prior = "Supprimer les lignes antérieures à (date locale) :",
      drop_rows_help = "Le cas échéant, les lignes dont la date locale est antérieure à cette valeur sont supprimées avant hFWI().",
      init_ffmc = "FFMC initial",
      init_dmc = "DMC initial",
      init_dc = "DC initial",
      run_hfwi = "Exécuter hFWI()",
      download_results = "Télécharger les résultats (CSV)",
      tab_output = "Résultats",
      tab_plot = "Graphique",
      tab_log = "Journal",
      data_source = "Source de données",
      data_src_results = "Résultats (sortie hFWI)",
      data_src_inputs = "Entrées (météo)",
      vars_to_plot = "Variables à tracer (par facettes)",
      vars_placeholder = "Sélectionner une ou plusieurs colonnes numériques…",
      facets_per_row = "Nombre de facettes par ligne",
      free_y = "Axe des y libre par facette",
      mapping_help = "Faites correspondre vos colonnes à ce qu’exige NG‑CFFDRS. Fournissez soit une colonne Date‑heure unique, soit Année/Mois/Jour/Heure.",
      col_datetime = "Date‑heure (facultatif)",
      col_year = "Année",
      col_month = "Mois",
      col_day = "Jour",
      col_hour = "Heure (0–23)",
      col_temp = "Température (°C)",
      col_rh = "HR (%)",
      col_ws = "Vent (km/h)",
      col_rain = "Pluie (mm)",
      col_lat = "Latitude (°)",
      col_lon = "Longitude (°)",
      modal_title      = "Avis!",
      modal_close      = "Fermer",
      modal_body_html  = paste0(
        "<p>Cette petite application R Shiny utilise le code PG-IFM du dépôt public ",
        "<a href=\"https://github.com/nrcan-cfs-fire/cffdrs-ng/tree/main\" target=\"_blank\" rel=\"noopener noreferrer\">cffdrs-ng</a> ",
        "sur GitHub pour générer des résultats d'indice horaire (IFM2025). ",
        "Les utilisateurs peuvent télécharger un fichier météorologique horaire et spécifier des intrants clés – tels que les codes de démarrage et la date de début – pour calculer les valeurs IFM2025.</p>",
        "<p>L'application est encore en cours de développement. Les mises à jour des codes, le traitement des intrants et le formatage des résultats peuvent changer au fur et à mesure que le système IFM2025 continue d'évoluer. ",
        "Les commentaires sont les bienvenus et les utilisateurs doivent anticiper les bogues et les problèmes potentiels, car le code et l'application sont en cours de développement. ",
        "<strong>Cette application ne doit pas être utilisée à des fins opérationnelles et est conçue comme un outil exploratoire.</strong></p>",
        "<p><strong>Cette application utilise Microsoft Copilot, un assistant conversationnel alimenté par l’IA basé sur le modèle GPT‑5, conçu pour fournir des informations, générer du code et améliorer la productivité. Version : Copilot (GPT‑5, août 2025).</strong></p>"
      ),
      plot_time_x = "Temps",
      plot_sel_vars_over_time = "Variables sélectionnées dans le temps",
      plot_var_over_time = "%s dans le temps",
      dt_btn_copy = "Copier",
      dt_btn_csv = "CSV",
      dt_btn_excel = "Excel",
      dt_sSearch = "Rechercher :",
      dt_sLength = "Afficher _MENU_ éléments",
      dt_sInfo = "Affichage de _START_ à _END_ sur _TOTAL_ éléments",
      dt_sInfoEmpty= "Affichage de 0 à 0 sur 0 élément",
      dt_sInfoFilt = "(filtré à partir de _MAX_ éléments au total)",
      dt_sZero = "Aucun enregistrement correspondant trouvé",
      dt_sProc = "Traitement...",
      dt_pag_first = "Premier",
      dt_pag_prev = "Précédent",
      dt_pag_next = "Suivant",
      dt_pag_last = "Dernier",
      aliases_short = c(
        ffmc = "FFMC",
        dmc  = "DMC",
        dc   = "DC",
        isi  = "IPI (ISI)",
        bui  = "BUI",
        fwi  = "IFM (FWI)",
        dsr  = "IGQ (DSR)"
      ),
      aliases_long = c(
        ffmc = "Indice d’humidité des combustibles fins (FFMC)",
        dmc  = "Indice d’humidité de la couche organique (DMC)",
        dc   = "Indice de sécheresse (DC)",
        isi  = "Indice de propagation initiale (IPI/ISI)",
        bui  = "Indice d’accumulation (BUI)",
        fwi  = "Indice forêt‑météo (IFM/FWI)",
        dsr  = "Indice de gravité quotidienne (IGQ/DSR)"
      ),
      # New i18n strings
      skip_to_main       = "Passer au contenu principal",
      aria_app_label     = "Application IFF horaire",
      aria_tabs_label    = "Onglets de sortie principaux",
      aria_run_label     = "Exécuter hFWI",
      tz_not_inferred    = "Fuseau horaire non encore déduit.",
      err_ffmc_range     = "FFMC doit être entre 0 et 101.",
      err_dmc_range      = "Le DMC doit être un nombre positif ou nul.",
      err_dc_range       = "Le DC doit être un nombre positif ou nul.",
      err_non_numeric_cols = "Ces colonnes ne sont pas numériques : %s",
      err_tz_invalid     = "Le fuseau horaire sélectionné n’est pas disponible sur ce système.",
      iana_prefix        = "Fuseau IANA :"
    )
  )

  lang <- reactiveVal("en")
  observe({
    qs <- parseQueryString(session$clientData$url_search)
    l <- tolower(qs[["lang"]]) %||% "en"
    if (l %in% c("en", "fr")) lang(l) else lang("en")
  })

  tr <- function(id) {
    L <- lang()
    out <- labels[[L]][[id]]
    if (is.null(out)) id else out
  }

  aliases_active <- function(type = c("short", "long")) {
    type <- match.arg(type)
    L <- lang()
    key <- if (type == "short") "aliases_short" else "aliases_long"
    out <- labels[[L]][[key]]
    if (is.null(out)) character(0) else out
  }
  label_for_col <- function(nm, type = c("short", "long")) {
    type <- match.arg(type)
    ali <- aliases_active(type)
    key <- tolower(nm)
    if (length(ali) && key %in% names(ali)) ali[[key]] else nm
  }
  labelize_cols <- function(cols, type = c("short", "long")) {
    type <- match.arg(type)
    setNames(cols, vapply(cols, label_for_col, character(1), type = type))
  }

  # Language toggle (instant, no reload)
  output$lang_toggle <- renderUI({
    cur <- lang()
    actionLink("toggle_lang",
               label = if (cur == "fr") "English" else "Français",
               class = "link-unstyled",
               `aria-label` = if (cur == "fr") "Switch to English" else "Passer en français")
  })
  observeEvent(input$toggle_lang, {
    lang(if (lang() == "fr") "en" else "fr")
    shiny::updateQueryString(paste0("?lang=", lang()), mode = "push")
  })

  # Initial labels/outputs
  output$app_title <- renderText(tr("title"))
  output$app_subtitle <- renderText("")
  output$skip_link_txt <- renderText(tr("skip_to_main"))

  # Keep <html lang> and key aria-labels synced with current language
  observeEvent(lang(), {
    session$sendCustomMessage('set-lang', list(lang = lang()))
    session$sendCustomMessage('set-aria-labels', list(
      app = tr('aria_app_label'),
      tabs = tr('aria_tabs_label'),
      run_label = tr('aria_run_label')
    ))
  }, ignoreInit = FALSE)

  # ---------- CSV read ----------
  raw_file <- reactive({
    req(input$csv)
    tryCatch(
      data.table::fread(
        input$csv$datapath,
        sep = ",",
        na.strings = c("", "NA", "NaN", "null"),
        header = isTRUE(input$has_header)
      ),
      error = function(e)
        utils::read.csv(
          input$csv$datapath,
          header = isTRUE(input$has_header),
          na.strings = c("", "NA", "NaN", "null"),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
    )
  })

  # Prefill start_date (unchanged from before)
  observeEvent(raw_file(), {
    df <- raw_file()
    datetime_col <- names(df)[grepl("datetime|timestamp", names(df), ignore.case = TRUE)][1]
    first_date <- NULL
    if (!is.na(datetime_col)) {
      first_date <- suppressWarnings(as.Date(df[[datetime_col]][1]))
    } else {
      ycol <- names(df)[grepl("^year$|^yr$", names(df), ignore.case = TRUE)][1]
      mcol <- names(df)[grepl("^month$|^mon$", names(df), ignore.case = TRUE)][1]
      dcol <- names(df)[grepl("^day$|^dy$", names(df), ignore.case = TRUE)][1]
      if (!any(is.na(c(ycol, mcol, dcol)))) {
        first_date <- as.Date(paste(df[[ycol]][1], df[[mcol]][1], df[[dcol]][1]),
                              format = "%Y %m %d")
      }
    }
    if (!is.null(first_date) && !is.na(first_date)) {
      updateDateInput(session, "start_date", value = first_date)
    }
  })

  # ---------- Mapping UI ----------
  find_col <- function(cols, keywords) {
    m <- cols[grepl(paste0("^", keywords, "$", collapse = "|"), cols, ignore.case = TRUE)]
    if (length(m) > 0) m[1] else ""
  }
  output$mapping_ui <- renderUI({
    L <- lang() # dependency
    df <- req(raw_file())
    cols <- names(df)
    tagList(
      helpText(tr("mapping_help")),
      selectInput(
        "col_datetime",
        tr("col_datetime"),
        choices = c("", cols),
        selected = find_col(cols, c("datetime","timestamp"))
      ),
      fluidRow(
        column(3, selectInput("col_year", tr("col_year"), choices = c("", cols),
                               selected = find_col(cols, c("year","yr","y")))),
        column(3, selectInput("col_month", tr("col_month"), choices = c("", cols),
                               selected = find_col(cols, c("month","mon","m")))),
        column(3, selectInput("col_day", tr("col_day"), choices = c("", cols),
                               selected = find_col(cols, c("day","dy","d")))),
        column(3, selectInput(
          "col_hour", tr("col_hour"), choices = c("", cols),
          selected = find_col(cols, c("hour","hr","h"))
        ))
      ),
      fluidRow(
        column(3, selectInput("col_temp", tr("col_temp"), choices = cols,
                               selected = find_col(cols, c("temp","temperature","t")))),
        column(3, selectInput("col_rh", tr("col_rh"), choices = c("", cols),
                               selected = find_col(cols, c("rh","relative humidity",
                                                           "relative.humidity","relative_humidity","humidity")))),
        column(3, selectInput("col_ws", tr("col_ws"), choices = c("", cols),
                               selected = find_col(cols, c("ws","windspeed","wind_speed",
                                                           "wind.speed","wind speed")))),
        column(3, selectInput("col_rain", tr("col_rain"), choices = c("", cols),
                               selected = find_col(cols, c("rain","precip","prec","precip_mm",
                                                           "prec_mm","rain_mm"))))
      ),
      fluidRow(
        column(6, selectInput("col_lat", tr("col_lat"), choices = c("", cols),
                               selected = find_col(cols, c("lat","latitude")))),
        column(6, selectInput("col_lon", tr("col_lon"), choices = c("", cols),
                               selected = find_col(cols, c("lon","long","longitude"))))
      )
    )
  })

  # ---------- Language-bound static labels ----------
  observeEvent(lang(), {
    output$lbl_upload_csv <- renderText(tr("upload_csv"))
    output$lbl_column_mapping <- renderText(tr("column_mapping"))
    output$csv_input_ui <- renderUI({
      fileInput(
        "csv",
        label = NULL,
        buttonLabel = tr("csv_button_label"),
        placeholder = tr("csv_place_holder"),
        accept = c(".csv", "text/csv")
      )
    })
    updateCheckboxInput(session, "has_header", label = tr("csv_has_header"))

    output$lbl_time_zone <- renderText(tr("time_zone"))
    updateRadioButtons(session, "tz_mode",
      label = NULL,
      choices = setNames(c("fixed","auto"),
                         c(tr("tz_fixed_one"), tr("tz_auto_infer"))),
      selected = input$tz_mode %||% "auto")
    updateSelectInput(session, "fixed_tz", label = tr("tz_select"))

    output$lbl_tz_offset_policy <- renderText(tr("tz_offset_policy"))
    updateRadioButtons(session, "tz_offset_policy",
      label = NULL,
      choices = setNames(c("std","modal"),
                         c(tr("tz_offset_std"), tr("tz_offset_modal"))),
      selected = input$tz_offset_policy %||% "std")

    output$lbl_filter <- renderText(tr("filter"))
    updateDateInput(session, "start_date", label = tr("drop_rows_prior"))
    output$txt_drop_rows_help <- renderText(tr("drop_rows_help"))

    updateNumericInput(session, "ffmc0", label = tr("init_ffmc"))
    updateNumericInput(session, "dmc0", label = tr("init_dmc"))
    updateNumericInput(session, "dc0", label = tr("init_dc"))

    output$run<-renderUI(actionButton("run",label = tr("run_hfwi"), class = "btn-primary",
                                       `aria-label` = tr("aria_run_label")))
    output$dl_ui <- renderUI(downloadButton("dl", tr("download_results")))

    output$tab_output_title <- renderText(tr("tab_output"))
    output$tab_plot_title <- renderText(tr("tab_plot"))
    output$tab_log_title <- renderText(tr("tab_log"))

    updateSelectInput(session, "plot_dataset",
      label = tr("data_source"),
      choices = setNames(c("results","inputs"),
                         c(tr("data_src_results"), tr("data_src_inputs"))),
      selected = input$plot_dataset %||% "results")
    updateSelectizeInput(session, "plot_y_multi",
      label = tr("vars_to_plot"),
      options = list(placeholder = tr("vars_placeholder")))
    updateNumericInput(session, "facet_ncol", label = tr("facets_per_row"))
    updateCheckboxInput(session, "facet_free_y", label = tr("free_y"))
  }, ignoreInit = FALSE)

  dt_i18n <- reactive({
    list(
      sSearch = tr("dt_sSearch"),
      sLengthMenu = tr("dt_sLength"),
      sInfo = tr("dt_sInfo"),
      sInfoEmpty = tr("dt_sInfoEmpty"),
      sInfoFiltered = tr("dt_sInfoFilt"),
      sZeroRecords = tr("dt_sZero"),
      sProcessing = tr("dt_sProc"),
      oPaginate = list(
        sFirst = tr("dt_pag_first"),
        sPrevious = tr("dt_pag_prev"),
        sNext = tr("dt_pag_next"),
        sLast = tr("dt_pag_last")
      )
    )
  })

  # ---- Time zone helpers ----
  parse_z_to_hours <- function(z_txt) {
    z_txt <- as.character(z_txt)
    z_txt <- z_txt[nzchar(z_txt)]
    if (!length(z_txt)) return(NA_real_)
    sgn <- ifelse(substr(z_txt, 1, 1) == "-", -1, 1)
    hh <- suppressWarnings(as.integer(substr(z_txt, 2, 3)))
    mm <- suppressWarnings(as.integer(substr(z_txt, 4, 5)))
    sgn * (hh + (mm %||% 0) / 60)
  }
  tz_standard_offset_hours <- function(tz, probe_date = "2025-01-15 12:00:00") {
    probe <- as.POSIXct(probe_date, tz = tz)
    parse_z_to_hours(format(probe, "%z"))
  }
  tz_modal_offset_hours <- function(datetimes) {
    z_txt <- format(datetimes, "%z")
    z_txt <- z_txt[nzchar(z_txt)]
    if (!length(z_txt)) stop("Could not infer modal offset: empty %z values.")
    z_mode <- names(which.max(table(z_txt)))
    list(offset = parse_z_to_hours(z_mode), z_mode = z_mode)
  }
  
  observe({
    showModal(modalDialog(
      title = tr("modal_title"),
      HTML(tr("modal_body_html")),  # <-- render your bilingual HTML body
      easyClose = TRUE,
      footer = modalButton(tr("modal_close")),
      size = "l"
    ))
  })
  

  tz_guess <- reactiveVal(NULL)
  observeEvent(input$tz_lookup_result, ignoreInit = TRUE, {
    if (is.character(input$tz_lookup_result) && nzchar(input$tz_lookup_result))
      tz_guess(input$tz_lookup_result)
  }, ignoreNULL = TRUE)
  output$tz_out <- renderPrint({
    tz <- tz_guess()
    if (is.null(tz)) tr("tz_not_inferred") else paste(tr("iana_prefix"), tz)
  })

  get_col <- function(df, nm) {
    if (isTruthy(nm) && nzchar(nm) && nm %in% names(df)) df[[nm]] else NULL
  }
  representative_latlon <- function(df) {
    lat <- lon <- NULL
    if (!missing(df) && !is.null(df)) {
      lat <- suppressWarnings(as.numeric(get_col(df, input$col_lat)))
      lon <- suppressWarnings(as.numeric(get_col(df, input$col_lon)))
      if (length(lat) && length(lon)) {
        la <- suppressWarnings(median(lat, na.rm = TRUE))
        lo <- suppressWarnings(median(lon, na.rm = TRUE))
        if (is.finite(la) && is.finite(lo)) return(c(lat = la, lon = lo))
      }
    }
    c(lat = NA_real_, lon = NA_real_)
  }
  observeEvent(list(input$tz_mode, input$run, input$csv), ignoreInit = FALSE, {
    if (identical(input$tz_mode, "auto")) {
      df <- try(raw_file(), silent = TRUE)
      if (!inherits(df, "try-error")) {
        coords <- representative_latlon(as.data.frame(df))
        if (is.finite(coords["lat"]) && is.finite(coords["lon"])) {
          session$sendCustomMessage("tz_lookup",
            list(lat = unname(coords["lat"]), lon = unname(coords["lon"])) )
        } else {
          if (is.character(input$tz_browser) && nzchar(input$tz_browser))
            tz_guess(input$tz_browser)
        }
      }
    }
  })

  # ---- Data shaping for model & plot ----
  shaped_input <- eventReactive(input$run, {
    validate(need(!is.null(raw_file()), "Upload a CSV first."))
    df <- tibble::as_tibble(raw_file())

    # required weather columns present
    needed <- c(input$col_temp, input$col_rh, input$col_ws, input$col_rain)
    validate(need(all(nzchar(needed)), "Please map temperature, RH, wind, and rain columns."))

    # must be numeric
    must_be_numeric <- c(input$col_temp, input$col_rh, input$col_ws, input$col_rain)
    bad <- Filter(function(nm) !is.numeric(df[[nm]]), must_be_numeric)
    validate(need(length(bad) == 0, sprintf(tr("err_non_numeric_cols"), paste(bad, collapse=", "))))

    # numeric input ranges
    validate(
      need(is.finite(input$ffmc0) && input$ffmc0 >= 0 && input$ffmc0 <= 101, tr("err_ffmc_range")),
      need(is.finite(input$dmc0)  && input$dmc0  >= 0, tr("err_dmc_range")),
      need(is.finite(input$dc0)   && input$dc0   >= 0, tr("err_dc_range"))
    )

    # timezone to use
    tz_use <- switch(input$tz_mode,
      "fixed" = input$fixed_tz,
      "auto" = {
        if (!is.null(tz_guess())) tz_guess()
        else if (is.character(input$tz_browser) && nzchar(input$tz_browser)) input$tz_browser
        else validate(need(FALSE, "Timezone not inferred yet—map lat/lon or use a fixed timezone."))
      }
    )
    validate(need(tz_use %in% OlsonNames(), tr("err_tz_invalid")))

    # Build datetime
    
    has_explicit_zone <- function(x) {
      if (is.null(x)) return(FALSE)
      x <- as.character(x)
      any(grepl("(Z$)|([\\+\\-]\\d{2}:?\\d{2}$)", x, perl = TRUE), na.rm = TRUE)
    }
    
    dt_col <- get_col(df, input$col_datetime)
    if (!is.null(dt_col)) {
      if (has_explicit_zone(dt_col)) {
        dt_utc <- lubridate::parse_date_time(
          dt_col,
          orders = c("Y-m-d H:M:S","Y-m-d H:M","Y/m/d H:M:S","Y/m/d H:M",
                     "d-m-Y H:M:S","d/m/Y H:M:S","m/d/Y H:M:S","m/d/Y H:M",
                     "Ymd HMS","Ymd HM","Ymd H"),
          tz = "UTC"
        )
        validate(need(!all(is.na(dt_utc)), "Could not parse your Date‑Time column (UTC/offset)."))
        dt_local <- lubridate::with_tz(dt_utc, tz = tz_use)
      } else {
        dt_local <- lubridate::parse_date_time(
          dt_col,
          orders = c("Y-m-d H:M:S","Y-m-d H:M","Y/m/d H:M:S","Y/m/d H:M",
                     "d-m-Y H:M:S","d/m/Y H:M:S","m/d/Y H:M:S","m/d/Y H:M",
                     "Ymd HMS","Ymd HM","Ymd H"),
          tz = tz_use
        )
        validate(need(!all(is.na(dt_local)), "Could not parse your Date‑Time column."))
      }
    } else {
      y <- get_col(df, input$col_year)
      m <- get_col(df, input$col_month)
      d <- get_col(df, input$col_day)
      h <- get_col(df, input$col_hour)
      validate(need(all(!is.null(c(y, m, d, h))), "Provide Date‑Time or Year/Month/Day/Hour."))
      dt_local <- lubridate::make_datetime(
        year = as.integer(y), month = as.integer(m), day = as.integer(d),
        hour = as.integer(h), tz = tz_use
      )
    }

    # Optional start-date filter (local)
    if (!is.null(input$start_date) && !is.na(input$start_date)) {
      keep <- as.Date(dt_local, tz = tz_use) >= as.Date(input$start_date)
      validate(need(any(keep), "All rows were filtered out by the start date."))
      df <- df[keep, , drop = FALSE]
      dt_local <- dt_local[keep]
      validate(need(nrow(df) > 0, "No rows remain after filtering; check your date filter or input data."))
    }

    # Offsets (hours)
    std_probe <- as.POSIXct("2025-01-15 12:00:00", tz = tz_use)
    std_z <- format(std_probe, "%z")
    std_h <- tz_standard_offset_hours(tz_use)
    mmodal <- tryCatch(tz_modal_offset_hours(dt_local),
                       error = function(e) list(offset = NA_real_, z_mode = NA_character_))
    modal_h <- mmodal$offset
    z_mode  <- mmodal$z_mode
    offset_hours <- if (identical(input$tz_offset_policy, "std")) std_h else modal_h
    if (!isTRUE(all.equal(offset_hours, round(offset_hours)))) {
      warning(sprintf("Time‑zone offset has minutes (%.2f h). Rounding to nearest hour for make_inputs().",
                      offset_hours))
      offset_hours <- round(offset_hours)
    }
    if (is.na(offset_hours) || abs(offset_hours) > 14) {
      stop(sprintf("Computed GMT offset (%.2f) seems invalid.", offset_hours))
    }

    wx <- tibble::tibble(
      datetime = dt_local,
      year  = lubridate::year(dt_local),
      month = lubridate::month(dt_local),
      day   = lubridate::day(dt_local),
      hour  = lubridate::hour(dt_local),
      temp  = as.numeric(get_col(df, input$col_temp)),
      rh    = as.numeric(get_col(df, input$col_rh)),
      ws    = as.numeric(get_col(df, input$col_ws)),
      rain  = as.numeric(get_col(df, input$col_rain)),
      lat   = suppressWarnings(as.numeric(get_col(df, input$col_lat))),
      long  = suppressWarnings(as.numeric(get_col(df, input$col_lon))),
      tz    = tz_use
    )
    validate(need(all(!is.na(wx$temp)), "Temperature has NA after parsing."))
    validate(need(all(!is.na(wx$rh)),   "RH has NA after parsing."))
    validate(need(all(!is.na(wx$ws)),   "Wind has NA after parsing."))
    validate(need(all(!is.na(wx$rain)), "Rain has NA after parsing."))

    # inputs <- make_inputs(data.table::as.data.table(wx), timezone = as.numeric(offset_hours))
    inputs <- wx

    list(
      inputs    = inputs,
      tz        = tz_use,
      tz_offset = offset_hours,
      start_date = input$start_date,
      n_rows     = nrow(wx),
      diag_std_z = std_z,
      diag_modal_z = z_mode
    )
  })

  # ---- Model run ----
  run_model <- reactive({
    req(shaped_input())
    si <- shaped_input()
    inputs <- si$inputs
    validate(need(exists("hFWI"), "hFWI() not found after sourcing NG_FWI.r"))
    fml <- tryCatch(formals(hFWI), error = function(e) NULL)
    out <- NULL
    try({
      if (!is.null(fml)) {
        argn <- names(fml)
        if (all(c("df_wx","timezone") %in% argn)) {
          out <- hFWI(df_wx = inputs, timezone = si$tz_offset,
                      ffmc_old = input$ffmc0, dmc_old = input$dmc0, dc_old = input$dc0)
        } else if ("inputs" %in% argn) {
          out <- hFWI(inputs = inputs, ffmc0 = input$ffmc0, dmc0 = input$dmc0, dc0 = input$dc0)
        } else if ("df" %in% argn) {
          out <- hFWI(df = inputs, ffmc0 = input$ffmc0, dmc0 = input$dmc0, dc0 = input$dc0)
        } else {
          if (length(argn) >= 5) {
            out <- hFWI(inputs, si$tz_offset, input$ffmc0, input$dmc0, input$dc0)
          } else {
            out <- hFWI(inputs, input$ffmc0, input$dmc0, input$dc0)
          }
        }
      } else {
        out <- try(hFWI(df_wx = inputs, timezone = si$tz_offset,
                        ffmc_old = input$ffmc0, dmc_old = input$dmc0, dc_old = input$dc0),
                   silent = TRUE)
        if (inherits(out, "try-error")) {
          out <- hFWI(inputs = inputs, ffmc0 = input$ffmc0, dmc0 = input$dmc0, dc0 = input$dc0)
        }
      }
    }, silent = TRUE)
    validate(need(!is.null(out), "hFWI() call failed; check the Log tab for details."))
    data.table::as.data.table(as.data.frame(out))
  })

  # ---- Plot data & choices ----
  data_for_plot <- reactive({
    req(shaped_input(), input$plot_dataset)
    df <- if (identical(input$plot_dataset, "inputs")) {
      as.data.frame(shaped_input()$inputs)
    } else {
      as.data.frame(run_model())
    }
    validate(need(nrow(df) > 0, "Selected dataset has no rows."))
    dt_candidates <- names(df)[grepl("datetime|timestamp|date|time", names(df), ignore.case = TRUE)]
    dt_col <- character(0)
    if (length(dt_candidates)) {
      typed <- dt_candidates[sapply(df[dt_candidates], function(x) inherits(x, c("POSIXt","Date")) || is.character(x))]
      dt_col <- if (length(typed)) typed[1] else dt_candidates[1]
    }
    if (!length(dt_col) && all(c("year","month","day","hour") %in% names(df))) {
      tz_use <- shaped_input()$tz %||% "UTC"
      df$datetime <- lubridate::make_datetime(
        year = as.integer(df$year), month = as.integer(df$month),
        day = as.integer(df$day), hour = as.integer(df$hour), tz = tz_use)
      dt_col <- "datetime"
    }
    validate(need(length(dt_col) == 1, "Couldn't find or construct a datetime/timestamp column."))
    ord <- try(order(df[[dt_col]]), silent = TRUE)
    if (!inherits(ord, "try-error")) df <- df[ord, , drop = FALSE]
    attr(df, "dt_col") <- dt_col
    df
  })

  populate_plot_choices <- function() {
    df <- data_for_plot()
    dt_col <- attr(df, "dt_col")
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    raw_choices <- setdiff(num_cols, dt_col)
    if (length(raw_choices) == 0L) {
      updateSelectizeInput(session, "plot_y_multi",
        choices = character(0), selected = character(0))
      return(invisible(NULL))
    }
    prev <- isolate(input$plot_y_multi) %||% character(0)
    still_valid <- intersect(prev, raw_choices)
    default_sel <- if (length(still_valid)) still_valid else head(raw_choices, min(3L, length(raw_choices)))
    named_choices <- labelize_cols(raw_choices, type = "short")
    updateSelectizeInput(session, "plot_y_multi",
      choices = named_choices, selected = default_sel
    )
  }
  observeEvent(list(data_for_plot(), input$plot_dataset), { populate_plot_choices() }, ignoreInit = FALSE)
  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "Plot")) populate_plot_choices()
  }, ignoreInit = FALSE)

  # ---- Render plot ----
  output$plot_ts <- renderPlot({
    df <- data_for_plot()
    dt_col <- attr(df, "dt_col")
    req(length(input$plot_y_multi) >= 1)
    yvars <- unique(input$plot_y_multi)
    keep_cols <- unique(c(dt_col, yvars))
    df_small <- df[, keep_cols, drop = FALSE]
    ord <- try(order(df_small[[dt_col]]), silent = TRUE)
    if (!inherits(ord, "try-error")) df_small <- df_small[ord, , drop = FALSE]
    long_df <- df_small |>
      tidyr::pivot_longer(cols = tidyselect::all_of(yvars),
                          names_to = "variable", values_to = "value") |>
      dplyr::filter(!is.na(.data$value))
    req(nrow(long_df) > 0)

    # Decimate points for very large series
    N <- nrow(long_df)
    if (N > 20000) {
      keep <- unique(floor(seq(1, N, length.out = 8000)))
      long_df <- long_df[keep, , drop = FALSE]
    }

    var_label_levels <- vapply(yvars, label_for_col, character(1), type = "short")
    long_df$var_label <- vapply(as.character(long_df$variable),
                                label_for_col, character(1), type = "short")
    long_df$var_label <- factor(long_df$var_label, levels = var_label_levels)

    ncol_facets <- {
      val <- input$facet_ncol
      if (is.null(val) || is.na(val) || val < 1) 1L else as.integer(val)
    }
    title_txt <- if (length(yvars) == 1)
      sprintf(tr("plot_var_over_time"), label_for_col(yvars[1], type = "short"))
    else
      tr("plot_sel_vars_over_time")

    p <- ggplot2::ggplot(long_df, ggplot2::aes(x = .data[[dt_col]], y = .data$value)) +
      ggplot2::geom_line(colour = "#26374A", linewidth = 0.6, na.rm = TRUE) +
      ggplot2::facet_wrap(~ var_label, ncol = ncol_facets,
                 scales = if (isTRUE(input$facet_free_y)) "free_y" else "fixed") +
      ggplot2::labs(x = tr("plot_time_x"), y = NULL, title = title_txt) +
      theme_goc()
    if (nrow(long_df) < 20000) {
      p <- p + ggplot2::geom_point(colour = "#26374A", size = 0.8, alpha = 0.7, na.rm = TRUE)
    }
    p
  })

  # ---- Table, Download, Log ----
  output$tbl <- DT::renderDT({
    req(run_model())
    df <- run_model()
    DT::datatable(
      df,
      escape = TRUE,
      filter = "top",
      extensions = c("Buttons","Scroller"),
      class = "display nowrap compact hover stripe gc-dt",
      options = list(
        pageLength = 25, scrollX = TRUE, deferRender = TRUE,
        scrollY = 500, scroller = TRUE,
        dom = "Bfrtip",
        buttons = list(
          list(extend = "copy",  text = tr("dt_btn_copy")),
          list(extend = "csv",   text = tr("dt_btn_csv")),
          list(extend = "excel", text = tr("dt_btn_excel"))
        ),
        processing = TRUE,
        language = dt_i18n()
      )
    )
  })

  safe_fwrite <- function(x, file) {
    tryCatch(data.table::fwrite(x, file),
             error = function(e) utils::write.csv(x, file, row.names = FALSE))
  }
  output$dl <- downloadHandler(
    filename = function() sprintf("hfwi_%s.csv", basename(input$csv$name %||% "results")),
    content  = function(file) safe_fwrite(run_model(), file),
    contentType = "text/csv"
  )

  output$log <- renderPrint({
    si <- shaped_input()
    cat("Rows read:", nrow(req(raw_file())), "
")
    if (!is.null(si$start_date) && !is.na(si$start_date)) {
      cat("Start-date filter (local):", as.character(si$start_date), "
")
    } else {
      cat("Start-date filter: (none)
")
    }
    cat("Rows after filtering:", si$n_rows, "
")
    cat("Time zone used:", si$tz, "
")
    cat("GMT offset (hours) passed to make_inputs():", si$tz_offset, "
")
    cat("Standard %z probe:", si$diag_std_z, "
")
    if (!is.null(si$diag_modal_z) && nzchar(si$diag_modal_z)) {
      cat("Modal %z:", si$diag_modal_z, "
")
    }
    cat("Initial codes: FFMC =", input$ffmc0, " DMC =", input$dmc0, " DC =", input$dc0, "
")
    cat("hFWI() formals:
")
    fml <- try(formals(hFWI), silent = TRUE)
    print(fml)
    argn <- if (inherits(fml, "try-error") || is.null(fml)) character(0) else names(fml)
    mapping <- if (all(c("df_wx","timezone") %in% argn)) "df_wx + timezone (NG)"
      else if ("inputs" %in% argn) "inputs (legacy)"
      else if ("df" %in% argn) "df (legacy)"
      else "positional fallback / unknown"
    cat("hFWI() call mapping -> ", mapping, "
")
    ng_commit <- tryCatch(readLines("ng/_ng_commit.txt", warn = FALSE)[1],
                          error = function(e) NA_character_)
    if (isTRUE(nzchar(ng_commit))) cat("cffdrs-ng commit:", ng_commit, "
")
  })
}
