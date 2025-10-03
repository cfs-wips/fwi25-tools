# R/modules/mod_i18n.R
# Centralized translations & language state as a module

# Labels moved here from R/i18n.R
i18n_labels <- list(
  en = list(
    title = "Hourly FWI (NG–CFFDRS)",
    upload_csv = "Upload weather CSV",
    csv_has_header = "CSV has header",
    csv_button_label = "Browse...",
    csv_place_holder = "No file selected",
    lat_label = "Latitude (°)",
    lon_label = "Longitude (°)",
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
    init_dmc  = "Initial DMC",
    init_dc   = "Initial DC",
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
    free_y = "Free y–scale per facet",
    mapping_help = "Map your columns to what NG–CFFDRS expects. Provide either a single Date–Time column, or Year/Month/Day/Hour.",
    col_datetime = "Date–Time (optional)",
    col_year  = "Year",
    col_month = "Month",
    col_day   = "Day",
    col_hour  = "Hour (0–23)",
    col_temp  = "Temperature (°C)",
    col_rh    = "RH (%)",
    col_ws    = "Wind (km/h)",
    col_rain  = "Rain (mm)",
    col_lat   = "Latitude (deg)",
    col_lon   = "Longitude (deg)",
    modal_title = "Notice!",
    modal_close = "Dismiss",
    modal_body_html = paste0(
      "<p>This small R Shiny application uses the NG FWI code from the public repository cffdrs-ng on GitHub to generate hourly FWI (IFM2025) results. Users can upload an hourly weather file and specify key inputs—such as starting codes and start date—to compute IFM2025 values.</p>",
      "<p>The app is under active development. Code updates, input handling, and results formatting may change as IFM2025 evolves. Feedback is welcome; expect bugs and issues. This app is not for operational use—it's an exploratory tool.</p>",
      "<p>This app uses Microsoft Copilot (GPT-5, Aug 2025) to assist with code and productivity.</p>"
    ),
    calc_fwi87 = "Calculate FWI87?",
    fwi25_results_title = "FWI25 Results (hourly)",
    fwi87_results_title = "FWI87 Results (daily)",
    legend_fwi25 = "FWI2025",
    legend_fwi87 = "FWI87",
    plot_time_x = "Time",
    plot_sel_vars_over_time = "Selected variables over time",
    plot_var_over_time = "%s over time",
    dt_btn_copy = "Copy",
    dt_btn_csv  = "CSV",
    dt_btn_excel= "Excel",
    dt_sSearch = "Search:",
    dt_sLength = "Show _MENU_ entries",
    dt_sInfo   = "Showing _START_ to _END_ of _TOTAL_ entries",
    dt_sInfoEmpty = "Showing 0 to 0 of 0 entries",
    dt_sInfoFilt  = "(filtered from _MAX_ total entries)",
    dt_sZero = "No matching records found",
    dt_sProc = "Processing...",
    dt_pag_first = "First",
    dt_pag_prev  = "Previous",
    dt_pag_next  = "Next",
    dt_pag_last  = "Last",
    aliases_short = c(
      ffmc = "FFMC", dmc = "DMC", dc = "DC", isi = "ISI", bui = "BUI", fwi = "FWI", dsr = "DSR"
    ),
    aliases_long = c(
      ffmc = "Fine Fuel Moisture Code (FFMC)", dmc = "Duff Moisture Code (DMC)", dc = "Drought Code (DC)",
      isi = "Initial Spread Index (ISI)", bui = "Buildup Index (BUI)", fwi = "Fire Weather Index (FWI)", dsr = "Daily Severity Rating (DSR)"
    ),
    # Accessibility & errors
    skip_to_main = "Skip to main content",
    aria_app_label = "Hourly FWI application",
    aria_tabs_label = "Primary output tabs",
    aria_run_label = "Run hFWI",
    tz_not_inferred = "Time zone not inferred yet.",
    err_ffmc_range = "FFMC must be between 0 and 101.",
    err_dmc_range  = "DMC must be a non-negative number.",
    err_dc_range   = "DC must be a non-negative number.",
    err_non_numeric_cols = "These columns are not numeric: %s",
    err_tz_invalid = "Selected time zone is not available on this system.",
    iana_prefix = "IANA time zone:"
  ),
  fr = list(
    title = "IFF horaire (NG‑CFFDRS)",
    upload_csv = "Téléverser le fichier CSV météo",
    csv_has_header = "Le CSV comporte une ligne d’en‑tête",
    csv_button_label = "Parcourir...",
    csv_place_holder = "Aucun fichier sélectionné",
    column_mapping = "Correspondance des colonnes",
    lat_label = "Latitude (°)",
    lon_label = "Longitude (°)",
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
    init_dmc  = "DMC initial",
    init_dc   = "DC initial",
    run_hfwi = "Exécuter hFWI()",
    download_results = "Télécharger les résultats (CSV)",
    tab_output = "Résultats",
    tab_plot   = "Graphique",
    tab_log    = "Journal",
    data_source = "Source de données",
    data_src_results = "Résultats (sortie hFWI)",
    data_src_inputs  = "Entrées (météo)",
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
    col_rh   = "HR (%)",
    col_ws   = "Vent (km/h)",
    col_rain = "Pluie (mm)",
    col_lat  = "Latitude (°)",
    col_lon  = "Longitude (°)",
    modal_title = "Avis!",
    modal_close = "Fermer",
    modal_body_html = paste0(
      "<p>Cette petite application R Shiny utilise le code PG‑IFM du dépôt public ",
      "<a href=\"https://github.com/nrcan-cfs-fire/cffdrs-ng/tree/main\" target=\"_blank\" rel=\"noopener noreferrer\">cffdrs‑ng</a> ",
      "sur GitHub pour générer des résultats d'indice horaire (IFM2025). ",
      "Les utilisateurs peuvent téléverser un fichier météorologique horaire et spécifier des intrants clés – tels que les codes de démarrage et la date de début – pour calculer les valeurs IFM2025.</p>",
      "<p>L'application est encore en cours de développement. Les mises à jour des codes, le traitement des intrants et le formatage des résultats peuvent changer au fur et à mesure que le système IFM2025 continue d'évoluer. ",
      "Les commentaires sont les bienvenus et les utilisateurs doivent anticiper les bogues et les problèmes potentiels, car le code et l'application sont en cours de développement. ",
      "<strong>Cette application ne doit pas être utilisée à des fins opérationnelles et est conçue comme un outil exploratoire.</strong></p>",
      "<p><strong>Cette application utilise Microsoft Copilot, un assistant conversationnel alimenté par l’IA basé sur le modèle GPT‑5, conçu pour fournir des informations, générer du code et améliorer la productivité. Version : Copilot (GPT‑5, août 2025).</strong></p>"
    ),
    calc_fwi87 = "Calculer IFM87?",
    fwi25_results_title = "Résultats IFM25 (horaire)",
    fwi87_results_title = "Résultats IFM87 (quotidiens)",
    legend_fwi25 = "IFM2025",
    legend_fwi87 = "IFM87",
    plot_time_x = "Temps",
    plot_sel_vars_over_time = "Variables sélectionnées dans le temps",
    plot_var_over_time = "%s dans le temps",
    dt_btn_copy = "Copier",
    dt_btn_csv  = "CSV",
    dt_btn_excel= "Excel",
    dt_sSearch = "Rechercher :",
    dt_sLength = "Afficher _MENU_ éléments",
    dt_sInfo   = "Affichage de _START_ à _END_ sur _TOTAL_ éléments",
    dt_sInfoEmpty = "Affichage de 0 à 0 sur 0 élément",
    dt_sInfoFilt  = "(filtré à partir de _MAX_ éléments au total)",
    dt_sZero = "Aucun enregistrement correspondant trouvé",
    dt_sProc = "Traitement...",
    dt_pag_first = "Premier",
    dt_pag_prev  = "Précédent",
    dt_pag_next  = "Suivant",
    dt_pag_last  = "Dernier",
    aliases_short = c(
      ffmc = "FFMC", dmc = "DMC", dc = "DC", isi = "IPI (ISI)", bui = "BUI", fwi = "IFM (FWI)", dsr = "IGQ (DSR)"
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
    # Accessibility & errors
    skip_to_main = "Passer au contenu principal",
    aria_app_label = "Application IFF horaire",
    aria_tabs_label = "Onglets de sortie principaux",
    aria_run_label = "Exécuter hFWI",
    tz_not_inferred = "Fuseau horaire non encore déduit.",
    err_ffmc_range = "FFMC doit être entre 0 et 101.",
    err_dmc_range  = "Le DMC doit être un nombre positif ou nul.",
    err_dc_range   = "Le DC doit être un nombre positif ou nul.",
    err_non_numeric_cols = "Ces colonnes ne sont pas numériques : %s",
    err_tz_invalid = "Le fuseau horaire sélectionné n’est pas disponible sur ce système.",
    iana_prefix = "Fuseau IANA :"
  )
)

tr_i18n <- function(key, lang = "en", ...) {
  if (!lang %in% names(i18n_labels)) lang <- "en"
  val <- i18n_labels[[lang]][[key]]
  if (is.null(val)) return(paste0("??", key))
  if (nargs() > 2) {
    dots <- list(...)
    if (is.character(val)) {
      for (nm in names(dots)) {
        val <- sub(paste0("{", nm, "}"), as.character(dots[[nm]]), val, fixed = TRUE)
      }
    }
  }
  val
}

get_i18n <- function(key, lang = "en"){
  if (!lang %in% names(i18n_labels)) lang <- "en"
  i18n_labels[[lang]][[key]]
}

# --- Module: i18n (header + language state) ---
mod_i18n_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Skip link
    tags$a(href = "#main-content", class = "skip-link", textOutput(ns("skip_link_txt"), inline = TRUE)),
    # Header
    tags$header(
      role = "banner",
      style = "background:#fff;border-bottom:1px solid var(--gcds-border-default,#7D828B);",
      tags$div(
        class = "gc-header",
        tags$a(
          class = "gc-header__brand",
          href = "https://www.canada.ca/en.html",
          `aria-label` = "Government of Canada home",
          tags$img(src = "goc_logo.svg", alt = "Government of Canada / Gouvernement du Canada", height = "28", style = "display:block")
        ),
        textOutput(ns("app_title"), container = function(...) h1(class = "gc-page-title gc-header__title", role = "heading", `aria-level` = "1", ...)),
        tags$nav(class = "gc-header__lang", `aria-label` = "Language", uiOutput(ns("lang_toggle")))
      )
    )
  )
}

mod_i18n_server <- function(id, session_title = TRUE){
  moduleServer(id, function(input, output, session){
    ns   <- session$ns
    lang <- reactiveVal("en")
    observe({
      qs <- parseQueryString(session$clientData$url_search)
      # robust fallback without %||%
      l  <- tolower(if (!is.null(qs[["lang"]])) qs[["lang"]] else "en")
      if (l %in% c("en","fr")) lang(l) else lang("en")
    })

    tr <- function(id, ...) tr_i18n(id, lang())
    aliases_active <- function(type = c("short","long")){
      type <- match.arg(type)
      L <- lang(); key <- if (type == "short") "aliases_short" else "aliases_long"
      out <- get_i18n(key, L); if (is.null(out)) character(0) else out
    }
    label_for_col <- function(nm, type = c("short","long")){
      type <- match.arg(type); ali <- aliases_active(type); key <- tolower(nm)
      if (length(ali) && key %in% names(ali)) ali[[key]] else nm
    }
    labelize_cols <- function(cols, type = c("short","long")){
      type <- match.arg(type)
      stats::setNames(cols, vapply(cols, label_for_col, character(1), type = type))
    }

    # Language toggle
    output$lang_toggle <- renderUI({
      cur <- lang()
      actionLink(ns("toggle_lang"), 
                 label = if (cur == "fr") "English" else "Français",
                 class = "link-unstyled",
                 `aria-label` = if (cur == "fr") "Switch to English" else "Passer en français")
    })
    observeEvent(input$toggle_lang, { 
      lang(if (lang() == "fr") "en" else "fr")
      updateQueryString(paste0("?lang=", lang()), mode = "push") 
    })

    # Static label outputs
    output$app_title <- renderText(tr("title"))
    output$skip_link_txt <- renderText(tr("skip_to_main"))

    # Sync custom messages
    observeEvent(lang(), {
      session$sendCustomMessage('set-aria-labels', list(
        app = tr('aria_app_label'),
        tabs = tr('aria_tabs_label'),
        run_label = tr('aria_run_label')
      ))
      if (isTRUE(session_title)) session$sendCustomMessage('set-title', tr('title'))
    }, ignoreInit = FALSE)

    # Datatable i18n
    dt_i18n <- reactive({
      list(
        sSearch = tr("dt_sSearch"), sLengthMenu = tr("dt_sLength"), sInfo = tr("dt_sInfo"),
        sInfoEmpty = tr("dt_sInfoEmpty"), sInfoFiltered= tr("dt_sInfoFilt"), sZeroRecords = tr("dt_sZero"),
        sProcessing = tr("dt_sProc"),
        oPaginate = list(sFirst = tr("dt_pag_first"), sPrevious= tr("dt_pag_prev"), sNext = tr("dt_pag_next"), sLast = tr("dt_pag_last"))
      )
    })

    return(list(
      lang = lang,
      tr = tr,
      aliases_active = aliases_active,
      label_for_col = label_for_col,
      labelize_cols = labelize_cols,
      dt_i18n = dt_i18n
    ))
  })
}
