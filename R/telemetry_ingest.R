# declare global variables to avoid R CMD check NOTEs
# these are used in dplyr/NSE contexts within read_otel_sqlite
utils::globalVariables(c("span_id", "value", "key"))

#' Get predefined telemetry sensitivity presets
#'
#' @description
#' Returns predefined threshold configurations for telemetry analysis with different
#' sensitivity levels. Use these presets with [bid_ingest_telemetry()] or
#' [bid_telemetry()] to easily adjust how aggressively the analysis identifies
#' UX friction points.
#'
#' **OpenTelemetry Compatibility**: These presets work with both shiny.telemetry
#' event data and Shiny 1.12+ OpenTelemetry span data. When using OTEL data,
#' spans are automatically converted to events for analysis.
#'
#' @param preset Character string specifying the sensitivity level:
#'   \describe{
#'     \item{strict}{Detects even minor issues - use for critical applications or new dashboards}
#'     \item{moderate}{Balanced default - appropriate for most applications (default)}
#'     \item{relaxed}{Only detects major issues - use for mature, stable dashboards}
#'   }
#'
#' @return Named list of threshold parameters suitable for passing to
#'   [bid_ingest_telemetry()] or [bid_telemetry()] thresholds parameter.
#'
#' @examples
#' # Get strict sensitivity thresholds
#' strict_thresholds <- bid_telemetry_presets("strict")
#'
#' # Use with telemetry analysis (works with both shiny.telemetry and OTEL)
#' \dontrun{
#' # Works with shiny.telemetry
#' issues <- bid_telemetry(
#'   "telemetry.sqlite",
#'   thresholds = bid_telemetry_presets("strict")
#' )
#'
#' # Works with Shiny OpenTelemetry (1.12+)
#' issues <- bid_telemetry(
#'   "otel_spans.json",
#'   thresholds = bid_telemetry_presets("strict")
#' )
#' }
#'
#' # Compare different presets
#' moderate <- bid_telemetry_presets("moderate")
#' relaxed <- bid_telemetry_presets("relaxed")
#'
#' @export
bid_telemetry_presets <- function(preset = c("moderate", "strict", "relaxed")) {
  preset <- match.arg(preset)

  presets <- list(
    strict = list(
      unused_input_threshold = 0.02, # flag if used by < 2% of sessions
      delay_threshold_secs = 20, # flag if > 20s to first action
      error_rate_threshold = 0.05, # flag if errors in > 5% of sessions
      navigation_threshold = 0.1, # flag if page visited by < 10%
      rapid_change_window = 15, # check 15s windows for confusion
      rapid_change_count = 4 # 4+ changes in window = confusion
    ),
    moderate = .default_telemetry_thresholds, # use centralized defaults
    relaxed = list(
      unused_input_threshold = 0.1, # flag if used by < 10% of sessions
      delay_threshold_secs = 60, # flag if > 60s to first action
      error_rate_threshold = 0.2, # flag if errors in > 20% of sessions
      navigation_threshold = 0.3, # flag if page visited by < 30%
      rapid_change_window = 5, # check 5s windows for confusion
      rapid_change_count = 7 # 7+ changes in window = confusion
    )
  )

  return(presets[[preset]])
}

#' Suggest alternative analytics solutions for static dashboards
#'
#' @description
#' Provides recommendations for analytics and telemetry solutions suitable for
#' static Quarto dashboards, where Shiny-based telemetry (shiny.telemetry or
#' OpenTelemetry) is not available. This function helps you choose the right
#' analytics tool based on your needs and constraints.
#'
#' **Important**: shiny.telemetry and Shiny OpenTelemetry only work with
#' `server: shiny` in Quarto YAML. For static Quarto dashboards (including
#' OJS-based dashboards), you need alternative web analytics solutions.
#'
#' @param dashboard_type Character string specifying the type of dashboard:
#'   \describe{
#'     \item{static}{Static HTML Quarto dashboard (default)}
#'     \item{ojs}{Quarto dashboard using Observable JS}
#'     \item{python}{Static dashboard with Python/Jupyter}
#'   }
#' @param privacy_preference Character string indicating privacy requirements:
#'   \describe{
#'     \item{gdpr_compliant}{Prioritize GDPR-compliant solutions (default)}
#'     \item{privacy_focused}{Emphasize user privacy and no tracking}
#'     \item{standard}{Standard analytics with typical tracking}
#'   }
#' @param budget Character string indicating budget constraints:
#'   \describe{
#'     \item{free}{Only free/open-source solutions}
#'     \item{low}{Low-cost solutions (< $10/month)}
#'     \item{flexible}{Any cost tier (default)}
#'   }
#' @param self_hosted Logical indicating whether self-hosted solutions are
#'   preferred (default: FALSE)
#'
#' @return A data frame with recommended analytics solutions, including:
#'   \item{solution}{Name of the analytics platform}
#'   \item{type}{Type of solution (privacy-focused, traditional, open-source)}
#'   \item{cost}{Cost tier (free, paid, freemium)}
#'   \item{self_hosted}{Whether self-hosting is available}
#'   \item{gdpr_compliant}{Whether the solution is GDPR compliant}
#'   \item{integration_method}{How to integrate (script tag, API, etc.)}
#'   \item{key_features}{Main features for UX analysis}
#'   \item{bidux_compatibility}{How well it works with BID framework}
#'   \item{docs_url}{Link to integration documentation}
#'
#' @section Integration Patterns:
#'
#' **For Static Quarto Dashboards:**
#'
#' 1. **Event Tracking** - Track user interactions with custom events:
#'    - Button clicks, filter changes, tab switches
#'    - Use JavaScript event listeners in Quarto
#'    - Send events to analytics platform via API
#'
#' 2. **Session Analysis** - Monitor user sessions:
#'    - Page views, time on page, bounce rate
#'    - User flow through dashboard sections
#'    - Identify drop-off points
#'
#' 3. **Custom Dimensions** - Track dashboard-specific metrics:
#'    - Selected filters, date ranges, visualization types
#'    - User cohorts, roles, or departments
#'    - Dashboard version or configuration
#'
#' **Example Integration (Plausible Analytics):**
#'
#' Add to your Quarto dashboard header:
#' ```html
#' <script defer data-domain="yourdomain.com"
#'   src="https://plausible.io/js/script.tagged-events.js"></script>
#' ```
#'
#' Track custom events in your dashboard JavaScript:
#' ```javascript
#' // Track filter change
#' document.getElementById('regionFilter').addEventListener('change', function(e) {
#'   plausible('Filter Changed', {props: {filter: 'region', value: e.target.value}});
#' });
#'
#' // Track visualization interaction
#' plotElement.on('plotly_click', function(data) {
#'   plausible('Chart Interaction', {props: {chart: 'sales_plot', action: 'click'}});
#' });
#' ```
#'
#' **Analyzing Results with BID Framework:**
#'
#' While these analytics tools won't automatically integrate with `bid_ingest_telemetry()`,
#' you can still apply BID framework principles:
#'
#' 1. **Notice** - Export analytics data, identify friction points manually
#' 2. **Interpret** - Use `bid_interpret()` with insights from analytics
#' 3. **Anticipate** - Apply `bid_anticipate()` to plan improvements
#' 4. **Structure** - Design improvements with `bid_structure()`
#' 5. **Validate** - Measure impact with before/after analytics comparison
#'
#' @examples
#' # Get recommendations for static Quarto dashboard with GDPR compliance
#' suggestions <- bid_suggest_analytics(
#'   dashboard_type = "static",
#'   privacy_preference = "gdpr_compliant"
#' )
#' print(suggestions)
#'
#' # Find free, privacy-focused solutions for OJS dashboard
#' privacy_options <- bid_suggest_analytics(
#'   dashboard_type = "ojs",
#'   privacy_preference = "privacy_focused",
#'   budget = "free"
#' )
#'
#' # Get self-hosted options
#' self_hosted <- bid_suggest_analytics(
#'   dashboard_type = "static",
#'   self_hosted = TRUE
#' )
#'
#' # View top recommendation
#' top_choice <- suggestions[1, ]
#' cat(sprintf("Recommended: %s\n", top_choice$solution))
#' cat(sprintf("Integration: %s\n", top_choice$integration_method))
#' cat(sprintf("Docs: %s\n", top_choice$docs_url))
#'
#' @export
bid_suggest_analytics <- function(
    dashboard_type = c("static", "ojs", "python"),
    privacy_preference = c("gdpr_compliant", "privacy_focused", "standard"),
    budget = c("flexible", "free", "low"),
    self_hosted = FALSE) {

  dashboard_type <- match.arg(dashboard_type)
  privacy_preference <- match.arg(privacy_preference)
  budget <- match.arg(budget)

  # Define comprehensive analytics solutions database
  all_solutions <- list(
    plausible = list(
      solution = "Plausible Analytics",
      type = "privacy-focused",
      cost = "paid",
      cost_monthly = 9,
      self_hosted = TRUE,
      gdpr_compliant = TRUE,
      cookieless = TRUE,
      integration_method = "Script tag with custom events API",
      key_features = c(
        "Cookieless tracking",
        "Custom event tracking",
        "Goal conversions",
        "Real-time dashboard",
        "No personal data collection"
      ),
      bidux_compatibility = "manual",
      compatibility_notes = "Export events manually for BID analysis",
      dashboard_support = c("static", "ojs", "python"),
      docs_url = "https://plausible.io/docs"
    ),
    fathom = list(
      solution = "Fathom Analytics",
      type = "privacy-focused",
      cost = "paid",
      cost_monthly = 14,
      self_hosted = FALSE,
      gdpr_compliant = TRUE,
      cookieless = TRUE,
      integration_method = "Script tag with event tracking",
      key_features = c(
        "Privacy-first tracking",
        "Event tracking",
        "Uptime monitoring",
        "Email reports",
        "GDPR/CCPA compliant"
      ),
      bidux_compatibility = "manual",
      compatibility_notes = "Export data via API for analysis",
      dashboard_support = c("static", "ojs", "python"),
      docs_url = "https://usefathom.com/docs"
    ),
    simple_analytics = list(
      solution = "Simple Analytics",
      type = "privacy-focused",
      cost = "paid",
      cost_monthly = 9,
      self_hosted = FALSE,
      gdpr_compliant = TRUE,
      cookieless = TRUE,
      integration_method = "Script tag with events API",
      key_features = c(
        "Privacy-friendly",
        "Automated events",
        "Custom events",
        "Bot detection",
        "API access"
      ),
      bidux_compatibility = "manual",
      compatibility_notes = "Use API to export for BID framework",
      dashboard_support = c("static", "ojs", "python"),
      docs_url = "https://docs.simpleanalytics.com/"
    ),
    posthog = list(
      solution = "PostHog",
      type = "product-analytics",
      cost = "freemium",
      cost_monthly = 0,
      self_hosted = TRUE,
      gdpr_compliant = TRUE,
      cookieless = FALSE,
      integration_method = "JavaScript SDK with comprehensive event tracking",
      key_features = c(
        "Product analytics",
        "Session recording",
        "Feature flags",
        "Funnel analysis",
        "Heatmaps",
        "User cohorts"
      ),
      bidux_compatibility = "good",
      compatibility_notes = "Rich event data can be exported for detailed BID analysis",
      dashboard_support = c("static", "ojs", "python"),
      docs_url = "https://posthog.com/docs"
    ),
    matomo = list(
      solution = "Matomo (formerly Piwik)",
      type = "open-source",
      cost = "free",
      cost_monthly = 0,
      self_hosted = TRUE,
      gdpr_compliant = TRUE,
      cookieless = TRUE,
      integration_method = "JavaScript tracking code with events API",
      key_features = c(
        "Full data ownership",
        "Event tracking",
        "Custom dimensions",
        "Heatmaps (plugin)",
        "Session recording (plugin)",
        "API access"
      ),
      bidux_compatibility = "good",
      compatibility_notes = "Export detailed event logs for BID analysis",
      dashboard_support = c("static", "ojs", "python"),
      docs_url = "https://matomo.org/docs/"
    ),
    umami = list(
      solution = "Umami",
      type = "open-source",
      cost = "free",
      cost_monthly = 0,
      self_hosted = TRUE,
      gdpr_compliant = TRUE,
      cookieless = TRUE,
      integration_method = "Script tag with event tracking",
      key_features = c(
        "Lightweight and fast",
        "Custom events",
        "Real-time data",
        "No cookies needed",
        "Easy self-hosting"
      ),
      bidux_compatibility = "manual",
      compatibility_notes = "Export events from database for analysis",
      dashboard_support = c("static", "ojs", "python"),
      docs_url = "https://umami.is/docs"
    ),
    google_analytics = list(
      solution = "Google Analytics 4",
      type = "traditional",
      cost = "free",
      cost_monthly = 0,
      self_hosted = FALSE,
      gdpr_compliant = FALSE,
      cookieless = FALSE,
      integration_method = "gtag.js with event tracking",
      key_features = c(
        "Comprehensive analytics",
        "Event tracking",
        "Custom dimensions",
        "Funnel analysis",
        "Integration with Google ecosystem",
        "BigQuery export"
      ),
      bidux_compatibility = "manual",
      compatibility_notes = "Requires GDPR consent; export via API or BigQuery",
      dashboard_support = c("static", "ojs", "python"),
      docs_url = "https://developers.google.com/analytics"
    ),
    heap = list(
      solution = "Heap Analytics",
      type = "product-analytics",
      cost = "freemium",
      cost_monthly = 0,
      self_hosted = FALSE,
      gdpr_compliant = TRUE,
      cookieless = FALSE,
      integration_method = "JavaScript snippet with autocapture",
      key_features = c(
        "Automatic event capture",
        "Retroactive analysis",
        "Session replay",
        "Funnel analysis",
        "User segmentation"
      ),
      bidux_compatibility = "good",
      compatibility_notes = "Rich autocaptured events ideal for UX analysis",
      dashboard_support = c("static", "ojs", "python"),
      docs_url = "https://developers.heap.io/docs"
    )
  )

  # Filter based on criteria
  filtered <- all_solutions

  # Filter by self-hosted preference
  if (self_hosted) {
    filtered <- Filter(function(x) x$self_hosted == TRUE, filtered)
  }

  # Filter by privacy preference
  if (privacy_preference == "privacy_focused") {
    filtered <- Filter(function(x) {
      x$type == "privacy-focused" && x$cookieless == TRUE
    }, filtered)
  } else if (privacy_preference == "gdpr_compliant") {
    filtered <- Filter(function(x) x$gdpr_compliant == TRUE, filtered)
  }

  # Filter by budget
  if (budget == "free") {
    filtered <- Filter(function(x) {
      x$cost == "free" || (x$cost == "freemium" && x$cost_monthly == 0)
    }, filtered)
  } else if (budget == "low") {
    filtered <- Filter(function(x) {
      x$cost == "free" ||
        (x$cost == "freemium" && x$cost_monthly == 0) ||
        (x$cost == "paid" && x$cost_monthly <= 10)
    }, filtered)
  }

  # Filter by dashboard type (all solutions support all types in this case)
  # This is a placeholder for future expansion
  filtered <- Filter(function(x) {
    dashboard_type %in% x$dashboard_support
  }, filtered)

  if (length(filtered) == 0) {
    cli::cli_warn(c(
      "No analytics solutions match your criteria",
      "i" = "Try relaxing constraints (e.g., budget or self_hosted)",
      "i" = "Use bid_suggest_analytics() with default parameters to see all options"
    ))
    return(data.frame(
      solution = character(0),
      type = character(0),
      cost = character(0),
      self_hosted = logical(0),
      gdpr_compliant = logical(0),
      integration_method = character(0),
      key_features = character(0),
      bidux_compatibility = character(0),
      docs_url = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Convert to data frame
  result <- do.call(rbind, lapply(names(filtered), function(name) {
    sol <- filtered[[name]]
    data.frame(
      solution = sol$solution,
      type = sol$type,
      cost = sol$cost,
      self_hosted = sol$self_hosted,
      gdpr_compliant = sol$gdpr_compliant,
      integration_method = sol$integration_method,
      key_features = paste(sol$key_features, collapse = "; "),
      bidux_compatibility = sol$bidux_compatibility,
      docs_url = sol$docs_url,
      stringsAsFactors = FALSE
    )
  }))

  # Sort by relevance (privacy-focused first if requested, then by compatibility)
  priority_order <- c("privacy-focused", "open-source", "product-analytics", "traditional")
  result$type_order <- match(result$type, priority_order)

  compatibility_order <- c("good", "manual")
  result$compat_order <- match(result$bidux_compatibility, compatibility_order)

  result <- result[order(result$type_order, result$compat_order), ]
  result$type_order <- NULL
  result$compat_order <- NULL
  rownames(result) <- NULL

  # Add helpful message
  cli::cli_alert_info(
    paste(
      "Found {nrow(result)} analytics solution{?s} matching your criteria",
      "for {dashboard_type} Quarto dashboards"
    )
  )

  if (nrow(result) > 0) {
    cli::cli_alert_success(
      paste(
        "Top recommendation: {result$solution[1]}",
        "({result$type[1]}, {result$cost[1]})"
      )
    )
    cli::cli_alert_info(
      paste(
        "See documentation for integration:",
        "{result$docs_url[1]}"
      )
    )
  }

  return(result)
}

#' Ingest telemetry data and identify UX friction points
#'
#' @description
#' This function ingests telemetry data from multiple sources and automatically
#' identifies potential UX issues, translating them into BID framework Notice stages.
#' It returns a hybrid object that is backward-compatible as a list of Notice stages
#' while also providing enhanced functionality with tidy tibble access and flags extraction.
#'
#' **Supported telemetry sources:**
#' - shiny.telemetry (SQLite or JSON)
#' - Shiny native OpenTelemetry (Shiny >= 1.12.0, OTLP JSON or SQLite)
#' - DBI database connections
#'
#' Format is automatically detected based on file structure and content.
#'
#' **OpenTelemetry Support**: For Shiny >= 1.12.0 applications using native
#' OpenTelemetry, pass the path to OTLP JSON exports or OTEL-formatted
#' SQLite databases. Spans are automatically converted to events for analysis.
#' See \code{vignette("otel-integration")} for complete setup guide.
#'
#' **Note:** For Quarto dashboards, shiny.telemetry only works when using
#' `server: shiny` in the Quarto YAML. Static Quarto dashboards and OJS-based
#' dashboards do not support shiny.telemetry. Consider alternative analytics
#' solutions (e.g., Plausible) for static dashboard usage tracking.
#'
#' @param source Either a file path to telemetry data or a DBI connection object.
#'   Supports:
#'   - SQLite databases (shiny.telemetry or OTEL format)
#'   - JSON files (shiny.telemetry logs or OTLP JSON exports)
#'   - DBI connections to databases with event or span tables
#'   When a connection is provided, it will not be closed by this function.
#'
#' @param format Optional format specification ("sqlite", "json", "otlp_json",
#'   "otel_sqlite"). If NULL (default), auto-detected from file extension and
#'   structure. OTLP formats are automatically detected when file contains
#'   OpenTelemetry span data.
#' @param events_table Optional data.frame specifying custom events table when
#'        reading from SQLite. Must have columns: event_id, timestamp,
#'        event_type, user_id. If NULL, auto-detects standard table names
#'        (event_data, events). Cannot be used with `table_name`.
#' @param table_name Optional character string specifying the table name to read
#'        from the database. If NULL (default), auto-detects standard table names
#'        (event_data, events). Cannot be used with `events_table`.
#' @param thresholds Optional list of threshold parameters:
#'        - unused_input_threshold: percentage of sessions below which input is
#'          considered unused (default: 0.05)
#'        - delay_threshold_secs: seconds of delay considered problematic
#'          (default: 30)
#'        - error_rate_threshold: percentage of sessions with errors considered
#'          problematic (default: 0.1)
#'        - navigation_threshold: percentage of sessions visiting a page below
#'          which it's considered underused (default: 0.2)
#'        - rapid_change_window: seconds within which multiple changes indicate
#'          confusion (default: 10)
#'        - rapid_change_count: number of changes within window to flag as
#'          confusion (default: 5)
#'
#' @return A hybrid object of class c("bid_issues", "list") containing bid_stage objects
#'         for each identified issue in the "Notice" stage. The object includes:
#'         \item{Legacy list}{Named list of bid_stage objects (e.g., "unused_input_region", "delayed_interaction")}
#'         \item{issues_tbl}{Attached tidy tibble with issue metadata}
#'         \item{flags}{Global telemetry flags as named list}
#'         \item{created_at}{Timestamp when object was created}
#'
#'         Use as_tibble() to access the tidy issues data, bid_flags() to extract flags,
#'         and legacy list access for backward compatibility.
#'
#' @examples
#' \dontrun{
#' # Works with shiny.telemetry SQLite
#' issues <- bid_ingest_telemetry("telemetry.sqlite")
#'
#' # Works with Shiny OpenTelemetry (1.12+)
#' issues <- bid_ingest_telemetry("otel_spans.json")
#'
#' # Use sensitivity presets for easier configuration
#' strict_issues <- bid_ingest_telemetry(
#'   "telemetry.sqlite",
#'   thresholds = bid_telemetry_presets("strict")
#' )
#'
#' # Analyze JSON log with custom thresholds
#' issues <- bid_ingest_telemetry(
#'   "telemetry.log",
#'   format = "json",
#'   thresholds = list(
#'     unused_input_threshold = 0.1,
#'     delay_threshold_secs = 60
#'   )
#' )
#'
#' # Use a DBI connection object directly
#' con <- DBI::dbConnect(RSQLite::SQLite(), "telemetry.sqlite")
#' issues <- bid_ingest_telemetry(con)
#' # Connection remains open for further use
#' DBI::dbDisconnect(con)
#'
#' # Specify custom table name
#' issues <- bid_ingest_telemetry(
#'   "telemetry.sqlite",
#'   table_name = "my_custom_events"
#' )
#'
#' # Same analysis workflow for both shiny.telemetry and OTEL
#' if (length(issues) > 0) {
#'   # Take first issue and continue with BID process
#'   interpret_result <- bid_interpret(
#'     previous_stage = issues[[1]],
#'     central_question = "How can we improve user engagement?"
#'   )
#' }
#' }
#'
#' @export
bid_ingest_telemetry <- function(
    source,
    format = NULL,
    events_table = NULL,
    table_name = NULL,
    thresholds = list()) {
  # check if source is a DBI connection or file path
  is_connection <- inherits(source, "DBIConnection")

  if (is_connection) {
    # validate connection is open
    if (!requireNamespace("DBI", quietly = TRUE)) {
      cli::cli_abort("Package 'DBI' is required to use connection objects")
    }
    if (!DBI::dbIsValid(source)) {
      cli::cli_abort("The provided database connection is not valid or has been closed")
    }
    # default to sqlite for connections
    if (is.null(format)) {
      format <- "sqlite"
    }
    # store path as NULL for messages
    path_for_message <- "<DBI connection>"
  } else {
    # treat as file path
    path <- source
    path_for_message <- path

    if (grepl("^file://", path, ignore.case = TRUE)) {
      cli::cli_abort(c(
        "file:// URLs are not supported",
        "i" = "Please provide a filesystem path instead",
        "x" = "Invalid: 'file:///path/to/file.json'",
        "v" = "Correct: '/path/to/file.json'"
      ))
    }

    # enhanced file validation
    if (!file.exists(path)) {
      cli::cli_abort("Telemetry file not found: {path}")
    }

    # check file size (prevent extremely large files)
    file_info <- file.info(path)
    if (is.na(file_info$size) || file_info$size > 100 * 1024 * 1024) { # 100MB limit
      cli::cli_abort("File size exceeds maximum limit (100MB) or cannot be accessed")
    }

    # validate file permissions
    if (!file.access(path, 4) == 0) { # check read permission
      cli::cli_abort("Cannot read file: {path}. Check file permissions.")
    }

    if (is.null(format)) {
      format <- detect_telemetry_format(path)
    }
  }

  # validate events_table and table_name are mutually exclusive
  if (!is.null(events_table) && !is.null(table_name)) {
    cli::cli_abort(standard_error_msg(
      "Cannot specify both 'events_table' and 'table_name' parameters",
      suggestions = c(
        "Use 'events_table' to provide a pre-loaded data.frame",
        "Use 'table_name' to specify which table to read from the database"
      )
    ))
  }

  # validate events_table parameter
  if (!is.null(events_table)) {
    validate_data_frame(
      events_table, "events_table",
      required_columns = c("event_id", "timestamp", "event_type", "user_id")
    )
  }

  # validate table_name parameter
  if (!is.null(table_name)) {
    if (!is.character(table_name) || length(table_name) != 1 || nchar(trimws(table_name)) == 0) {
      cli::cli_abort(standard_error_msg(
        "table_name must be a non-empty character string",
        context = glue::glue("You provided: {class(table_name)[1]}")
      ))
    }
  }

  # validate thresholds parameter
  if (!is.null(thresholds) && !is.list(thresholds)) {
    cli::cli_abort(standard_error_msg(
      "thresholds parameter must be a list or NULL",
      context = glue::glue("You provided: {class(thresholds)[1]}")
    ))
  }

  if (!format %in% c("sqlite", "json")) {
    cli::cli_abort("Format must be 'sqlite' or 'json', got: {format}")
  }

  # json format cannot use connection objects

  if (format == "json" && is_connection) {
    cli::cli_abort("DBI connections are only supported for SQLite format, not JSON")
  }

  # use centralized defaults from telemetry_analysis.R (single source of truth)
  thresholds <- modifyList(.default_telemetry_thresholds, thresholds)

  cli::cli_alert_info("Reading telemetry data from {format} source...")

  # determine source to pass to read_telemetry_data
  data_source <- if (is_connection) source else path

  events <- read_telemetry_data(data_source, format, events_table, table_name)

  if (nrow(events) == 0) {
    cli::cli_warn("No telemetry events found in {path_for_message}")
    return(list())
  }

  # get total sessions for pct calculations
  total_sessions <- get_total_sessions(events)
  cli::cli_alert_info(
    "Analyzing {nrow(events)} events from {total_sessions} sessions..."
  )

  notice_issues <- list()

  # find unused inputs
  unused_inputs <- find_unused_inputs(events, thresholds$unused_input_threshold)
  if (length(unused_inputs) > 0) {
    for (input_info in unused_inputs) {
      issue_key <- paste0(
        "unused_input_",
        gsub("[^a-zA-Z0-9]", "_", input_info$input_id)
      )
      notice_issues[[issue_key]] <- create_unused_input_notice(
        input_info,
        total_sessions,
        events
      )
    }
  }

  # find delayed interactions
  delay_info <- find_delayed_sessions(
    events,
    thresholds$delay_threshold_secs
  )
  if (!is.null(delay_info) && delay_info$has_issues) {
    notice_issues[["delayed_interaction"]] <- create_delay_notice(
      delay_info,
      total_sessions,
      thresholds$delay_threshold_secs,
      events
    )
  }

  # find error patterns
  error_patterns <- find_error_patterns(events, thresholds$error_rate_threshold)
  if (length(error_patterns) > 0) {
    for (i in seq_along(error_patterns)) {
      error_info <- error_patterns[[i]]
      issue_key <- paste0("error_", i)
      notice_issues[[issue_key]] <- create_error_notice(
        error_info,
        total_sessions,
        events
      )
    }
  }

  # find navigation drop-offs
  if ("navigation" %in% unique(events$event_type)) {
    navigation_issues <- find_navigation_dropoffs(
      events,
      thresholds$navigation_threshold
    )
    if (length(navigation_issues) > 0) {
      for (nav_info in navigation_issues) {
        issue_key <- paste0(
          "navigation_",
          gsub("[^a-zA-Z0-9]", "_", nav_info$page)
        )
        notice_issues[[issue_key]] <- create_navigation_notice(
          nav_info,
          total_sessions,
          events
        )
      }
    }
  }

  # find rapid change patterns (confusion indicators)
  confusion_patterns <- find_confusion_patterns(
    events,
    thresholds$rapid_change_window,
    thresholds$rapid_change_count
  )
  if (length(confusion_patterns) > 0) {
    for (i in seq_along(confusion_patterns)) {
      confusion_info <- confusion_patterns[[i]]
      issue_key <- paste0(
        "confusion_",
        gsub("[^a-zA-Z0-9]", "_", confusion_info$input_id)
      )
      notice_issues[[issue_key]] <- create_confusion_notice(
        confusion_info,
        total_sessions,
        events
      )
    }
  }

  # summary
  if (length(notice_issues) == 0) {
    cli::cli_alert_success(
      paste(
        "No significant UX issues identified from telemetry. All tracked",
        "inputs were used and no systematic problems detected."
      )
    )
  } else {
    cli::cli_alert_warning(
      paste(
        "Identified {length(notice_issues)} potential UX issue{?s} from",
        "telemetry analysis"
      )
    )
  }

  # create tidy issues tibble for new API
  issues_tbl <- .create_issues_tibble(notice_issues, total_sessions, events)

  # extract global telemetry flags
  flags <- .flags_from_issues(issues_tbl, events, thresholds)

  # validate that notice_issues is a proper list before creating hybrid object
  if (!is.list(notice_issues)) {
    cli::cli_abort(
      "Internal error: notice_issues must be a list for hybrid object creation"
    )
  }

  # validate that issues_tbl is a proper tibble
  if (!tibble::is_tibble(issues_tbl)) {
    cli::cli_abort(
      "Internal error: issues_tbl must be a tibble for hybrid object creation"
    )
  }

  # validate that flags is a proper list
  if (!is.list(flags)) {
    cli::cli_abort("Internal error: flags must be a list for hybrid object creation")
  }

  # create hybrid object with both legacy list and new attributes
  result <- structure(
    notice_issues,
    class = c("bid_issues", "list"),
    issues_tbl = issues_tbl,
    flags = flags,
    created_at = Sys.time()
  )

  return(result)
}

#' Auto-detect telemetry format from file extension
#' @param path File path
#' @return Format string ("sqlite" or "json")
#' @keywords internal
detect_telemetry_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("sqlite", "sqlite3", "db")) {
    return("sqlite")
  } else if (ext %in% c("json", "log", "txt")) {
    return("json")
  } else {
    cli::cli_abort(
      paste(
        "Cannot auto-detect format from extension '.{ext}'.",
        "Please specify format parameter."
      )
    )
  }
}

#' Read telemetry data from file or connection
#' @param source File path or DBI connection object
#' @param format Format ("sqlite" or "json")
#' @param events_table Optional custom events table for SQLite
#' @param table_name Optional table name for SQLite
#' @return Data frame of events
#' @keywords internal
read_telemetry_data <- function(source, format, events_table = NULL, table_name = NULL) {
  if (format == "sqlite") {
    return(read_telemetry_sqlite(source, events_table, table_name))
  } else if (format == "json") {
    return(read_telemetry_json(source))
  }
}

#' Read telemetry from SQLite database
#' @param source SQLite database path or DBI connection object
#' @param events_table Optional custom events table data.frame
#' @param table_name Optional character string specifying table name to read
#' @return Data frame of events
#' @keywords internal
read_telemetry_sqlite <- function(source, events_table = NULL, table_name = NULL) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    cli::cli_abort("Package 'DBI' is required to read SQLite telemetry data")
  }

  # determine if source is a connection or file path
  is_connection <- inherits(source, "DBIConnection")

  # for file paths, we also need RSQLite
  if (!is_connection && !requireNamespace("RSQLite", quietly = TRUE)) {
    cli::cli_abort(
      "Package 'RSQLite' is required to read SQLite telemetry data from file paths"
    )
  }

  # connection management based on ownership pattern:
  # - if we create the connection, we close it
  # - if connection is passed in, we leave it open
  con <- NULL
  we_opened_connection <- FALSE

  tryCatch(
    {
      if (is_connection) {
        con <- source
        # don't close connections we didn't open
        we_opened_connection <- FALSE
      } else {
        con <- DBI::dbConnect(RSQLite::SQLite(), source)
        we_opened_connection <- TRUE
      }

      # check if this is an otel database (has spans table)
      tables <- DBI::dbListTables(con)
      if ("spans" %in% tables && is.null(events_table) && is.null(table_name)) {
        # likely otel format - use otel reader
        cli::cli_alert_info("Detected OpenTelemetry SQLite format")
        events <- read_otel_sqlite(con)
        return(events)
      }

      # if custom events_table provided, use it directly
      if (!is.null(events_table)) {
        events <- events_table
        cli::cli_alert_info("Using provided events_table data.frame")
      } else {
        # determine table name to use
        if (!is.null(table_name)) {
          # user specified table name - verify it exists
          if (!table_name %in% tables) {
            cli::cli_abort(standard_error_msg(
              "Table '{table_name}' not found in database",
              context = glue::glue("Available tables: {paste(tables, collapse = ', ')}"),
              suggestions = "Check the table name or use events_table parameter to provide data directly"
            ))
          }
          event_table <- table_name
          cli::cli_alert_info("Using specified table: '{event_table}'")
        } else {
          # auto-detect table name
          # look for events table (common {shiny.telemetry} table name)
          event_table <- NULL
          if ("event_data" %in% tables) {
            event_table <- "event_data"
          } else if ("events" %in% tables) {
            event_table <- "events"
          } else if (length(tables) > 0) {
            # use first table if no standard name found
            event_table <- tables[1]
            cli::cli_warn(
              "No standard event table found, using '{event_table}'"
            )
          } else {
            cli::cli_abort(standard_error_msg(
              "No tables found in SQLite database",
              suggestions = c(
                "Ensure the database contains event data",
                "Provide events_table parameter with pre-loaded data",
                "Specify table_name parameter if using a custom table name"
              )
            ))
          }
        }

        events <- DBI::dbReadTable(con, event_table)
      }

      events <- normalize_telemetry_columns(events)
      return(events)
    },
    error = function(e) {
      cli::cli_abort("Error reading SQLite database: {e$message}")
    },
    finally = {
      # only close connection if we opened it
      if (we_opened_connection && !is.null(con)) {
        DBI::dbDisconnect(con)
      }
    }
  )
}

#' Read telemetry from JSON log file
#' @param path JSON log file path
#' @return Data frame of events
#' @keywords internal
read_telemetry_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cli::cli_abort("Package 'jsonlite' is required to read JSON telemetry data")
  }

  # guard against excessively large files to prevent memory exhaustion
  file_size_mb <- file.info(path)$size / 1024^2
  if (!is.na(file_size_mb) && file_size_mb > 100) {
    cli::cli_abort(c(
      "JSON file exceeds maximum size limit",
      "x" = "File size: {round(file_size_mb, 1)}MB (limit: 100MB)",
      "i" = "Consider splitting large telemetry exports into smaller files"
    ))
  }

  # check if this is an otel json file
  if (detect_otel_json(path)) {
    cli::cli_alert_info("Detected OpenTelemetry JSON format")
    return(read_otel_json(path))
  }

  tryCatch(
    {
      # try to read as JSON lines (one JSON object per line)
      lines <- readLines(path, warn = FALSE)
      lines <- lines[nchar(trimws(lines)) > 0]

      if (length(lines) == 0) {
        return(data.frame(
          timestamp = character(),
          session_id = character(),
          event_type = character(),
          stringsAsFactors = FALSE
        ))
      }

      # check if JSON array
      if (substr(trimws(lines[1]), 1, 1) == "[") {
        # if TRUE, parse as whole
        events <- jsonlite::fromJSON(
          paste(lines, collapse = "\n"),
          flatten = TRUE
        )
      } else {
        # if FALSE, try to parse each line as JSON
        events_list <- lapply(lines, function(line) {
          tryCatch(
            jsonlite::fromJSON(line, flatten = TRUE),
            error = function(e) NULL
          )
        })

        events_list <- events_list[!vapply(events_list, is.null, logical(1))]

        if (length(events_list) == 0) {
          cli::cli_abort("No valid JSON could be parsed from file")
        }

        # filter out events that don't have required fields
        required_fields <- c("timestamp", "session_id", "event_type")
        valid_events <- lapply(events_list, function(event) {
          if (is.list(event) && all(required_fields %in% names(event))) {
            return(event)
          }
          return(NULL)
        })

        valid_events <- valid_events[!vapply(valid_events, is.null, logical(1))]

        if (length(valid_events) == 0) {
          cli::cli_abort("No valid events found in JSON file")
        }

        events <- dplyr::bind_rows(valid_events)
      }

      if (!is.data.frame(events)) {
        events <- as.data.frame(events)
      }

      # if empty, return empty data frame with req columns
      if (nrow(events) == 0) {
        return(data.frame(
          timestamp = character(),
          session_id = character(),
          event_type = character(),
          stringsAsFactors = FALSE
        ))
      }

      # normalize column names
      events <- normalize_telemetry_columns(events)

      return(events)
    },
    error = function(e) {
      cli::cli_abort(c(
        "Error reading JSON file: {e$message}",
        "i" = "File: {path}",
        "i" = "Ensure the file contains valid JSON with required fields: timestamp, session_id, event_type"
      ))
    }
  )
}

#' Detect if JSON file contains OTLP (OpenTelemetry Protocol) data
#'
#' @description
#' Checks if a JSON file contains OpenTelemetry Protocol span data by looking
#' for the characteristic OTLP structure (resourceSpans, scopeSpans, spans).
#'
#' @param source_path Path to JSON file
#' @return Logical TRUE if OTLP format detected, FALSE otherwise
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' detect_otel_json("spans.json") # returns TRUE for otlp files
#' detect_otel_json("telemetry.json") # returns FALSE for shiny.telemetry files
#' }
detect_otel_json <- function(source_path) {
  tryCatch(
    {
      # parse json file
      json_data <- jsonlite::fromJSON(source_path, simplifyVector = FALSE)

      # check for otlp structure markers
      has_resource_spans <- "resourceSpans" %in% names(json_data)

      if (has_resource_spans) {
        # verify nested structure
        if (length(json_data$resourceSpans) > 0) {
          first_resource <- json_data$resourceSpans[[1]]
          has_scope_spans <- "scopeSpans" %in% names(first_resource)

          if (has_scope_spans && length(first_resource$scopeSpans) > 0) {
            first_scope <- first_resource$scopeSpans[[1]]
            has_spans <- "spans" %in% names(first_scope)
            return(has_spans)
          }
        }
      }

      return(FALSE)
    },
    error = function(e) {
      # if we can't parse, assume not otel format
      return(FALSE)
    }
  )
}

#' Check JSON nesting depth recursively
#'
#' @description
#' Validates that JSON data does not exceed a maximum nesting depth to prevent
#' stack overflow and resource exhaustion attacks.
#'
#' @param obj JSON object (list or other R object from `jsonlite::fromJSON`)
#' @param max_depth Maximum allowed nesting depth (default: 50)
#' @param current_depth Current recursion depth (internal use)
#' @return Logical `TRUE` if depth is acceptable, aborts with error if exceeded
#' @keywords internal
check_json_depth <- function(obj, max_depth = 50, current_depth = 1) {
  if (current_depth > max_depth) {
    cli::cli_abort(c(
      "JSON nesting depth exceeds security limit",
      "x" = "Maximum allowed depth: {max_depth} levels",
      "i" = "This file may be malformed or malicious",
      "i" = "Consider using trusted data sources only"
    ))
  }

  if (is.list(obj)) {
    for (element in obj) {
      check_json_depth(element, max_depth, current_depth + 1)
    }
  }

  return(TRUE)
}

#' Read OpenTelemetry JSON (OTLP) file
#'
#' @description
#' Reads OpenTelemetry Protocol (OTLP) JSON files containing span data from
#' Shiny 1.12+ applications. Extracts spans from the nested OTLP structure and
#' converts them to bidux event schema.
#'
#' @param path Path to OTLP JSON file
#' @return Data frame with bidux event schema (converted from spans)
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' events <- read_otel_json("otel_spans.json")
#' names(events)
#' # [1] "timestamp" "session_id" "event_type" "input_id" "value" "error_message"
#' # [7] "output_id" "navigation_id"
#' }
read_otel_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cli::cli_abort("Package 'jsonlite' is required to read OTLP JSON data")
  }

  # guard against excessively large files to prevent memory exhaustion
  file_size_mb <- file.info(path)$size / 1024^2
  if (!is.na(file_size_mb) && file_size_mb > 100) {
    cli::cli_abort(c(
      "JSON file exceeds maximum size limit",
      "x" = "File size: {round(file_size_mb, 1)}MB (limit: 100MB)",
      "i" = "Consider splitting large telemetry exports into smaller files"
    ))
  }

  tryCatch(
    {
      # parse otlp json structure
      json_data <- jsonlite::fromJSON(path, simplifyVector = FALSE)

      # validate JSON depth to prevent stack overflow attacks
      check_json_depth(json_data, max_depth = 50)

      # validate otlp structure
      if (!"resourceSpans" %in% names(json_data)) {
        cli::cli_abort(c(
          "Invalid OTLP JSON structure",
          "i" = "Expected top-level 'resourceSpans' field",
          "i" = "File: {path}"
        ))
      }

      # extract all spans from nested structure
      all_spans <- list()

      for (resource_span in json_data$resourceSpans) {
        if (!"scopeSpans" %in% names(resource_span)) {
          next
        }

        for (scope_span in resource_span$scopeSpans) {
          if (!"spans" %in% names(scope_span)) {
            next
          }

          # add spans from this scope
          all_spans <- c(all_spans, scope_span$spans)
        }
      }

      if (length(all_spans) == 0) {
        cli::cli_warn("No spans found in OTLP JSON file")
        return(data.frame(
          timestamp = character(),
          session_id = character(),
          event_type = character(),
          stringsAsFactors = FALSE
        ))
      }

      # convert list of spans to data frame
      spans_df <- dplyr::bind_rows(lapply(all_spans, function(span) {
        # flatten span attributes
        attrs_list <- list()
        if (!is.null(span$attributes)) {
          for (attr in span$attributes) {
            # handle key that might be a list (from auto_unbox = FALSE)
            key <- if (is.list(attr$key)) {
              as.character(attr$key[[1]])
            } else {
              as.character(attr$key)
            }
            # extract value from nested structure (also handle list-wrapped values)
            raw_value <- attr$value
            value <- if (!is.null(raw_value$stringValue)) {
              v <- raw_value$stringValue
              if (is.list(v)) v[[1]] else v
            } else if (!is.null(raw_value$intValue)) {
              v <- raw_value$intValue
              if (is.list(v)) v[[1]] else v
            } else if (!is.null(raw_value$doubleValue)) {
              v <- raw_value$doubleValue
              if (is.list(v)) v[[1]] else v
            } else if (!is.null(raw_value$boolValue)) {
              v <- raw_value$boolValue
              if (is.list(v)) v[[1]] else v
            } else {
              NA
            }
            attrs_list[[key]] <- value
          }
        }

        # create span record with attributes as nested list
        # ensure all ID fields are always character (not list) to avoid type mismatch
        # jsonlite may parse IDs as lists when unicode/special chars present
        trace_id <- if (is.null(span$traceId)) {
          NA_character_
        } else if (is.list(span$traceId)) {
          if (length(span$traceId) > 0) {
            as.character(span$traceId[[1]])
          } else {
            NA_character_
          }
        } else {
          as.character(span$traceId)
        }

        span_id <- if (is.null(span$spanId)) {
          NA_character_
        } else if (is.list(span$spanId)) {
          if (length(span$spanId) > 0) {
            as.character(span$spanId[[1]])
          } else {
            NA_character_
          }
        } else {
          as.character(span$spanId)
        }

        parent_span_id <- if (is.null(span$parentSpanId)) {
          NA_character_
        } else if (is.list(span$parentSpanId)) {
          if (length(span$parentSpanId) > 0) {
            as.character(span$parentSpanId[[1]])
          } else {
            NA_character_
          }
        } else {
          as.character(span$parentSpanId)
        }

        start_time <- if (is.null(span$startTimeUnixNano)) {
          NA_character_
        } else if (is.list(span$startTimeUnixNano)) {
          if (length(span$startTimeUnixNano) > 0) {
            as.character(span$startTimeUnixNano[[1]])
          } else {
            NA_character_
          }
        } else {
          as.character(span$startTimeUnixNano)
        }

        end_time <- if (is.null(span$endTimeUnixNano)) {
          NA_character_
        } else if (is.list(span$endTimeUnixNano)) {
          if (length(span$endTimeUnixNano) > 0) {
            as.character(span$endTimeUnixNano[[1]])
          } else {
            NA_character_
          }
        } else {
          as.character(span$endTimeUnixNano)
        }

        # handle name that might be a list
        span_name <- if (is.null(span$name)) {
          NA_character_
        } else if (is.list(span$name)) {
          if (length(span$name) > 0) as.character(span$name[[1]]) else NA_character_
        } else {
          as.character(span$name)
        }

        tibble::tibble(
          name = span_name,
          traceId = trace_id,
          spanId = span_id,
          parentSpanId = parent_span_id,
          startTimeUnixNano = start_time,
          endTimeUnixNano = end_time,
          attributes = list(attrs_list),
          events = list(span$events)
        )
      }))

      # convert spans to bidux event schema
      events <- convert_otel_spans_to_events(spans_df)

      return(events)
    },
    error = function(e) {
      cli::cli_abort(c(
        "Error reading OTLP JSON file: {e$message}",
        "i" = "File: {path}",
        "i" = "Ensure the file contains valid OTLP JSON structure"
      ))
    }
  )
}

#' Read OpenTelemetry SQLite database
#'
#' @description
#' Reads OpenTelemetry span data from SQLite databases that store OTEL traces.
#' Looks for standard OTEL table names (spans, span_events, span_attributes) and
#' joins them to reconstruct the span structure before converting to bidux events.
#'
#' @param source SQLite database path or DBI connection object
#' @return Data frame with bidux event schema (converted from spans)
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' events <- read_otel_sqlite("otel_traces.db")
#' names(events)
#' # [1] "timestamp" "session_id" "event_type" "input_id" "value" "error_message"
#' # [7] "output_id" "navigation_id"
#' }
read_otel_sqlite <- function(source) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    cli::cli_abort("Package 'DBI' is required to read OTEL SQLite data")
  }

  # determine if source is a connection or file path
  is_connection <- inherits(source, "DBIConnection")

  # for file paths, we also need rsqlite
  if (!is_connection && !requireNamespace("RSQLite", quietly = TRUE)) {
    cli::cli_abort(
      "Package 'RSQLite' is required to read OTEL SQLite data from file paths"
    )
  }

  con <- NULL
  we_opened_connection <- FALSE

  tryCatch(
    {
      if (is_connection) {
        con <- source
        we_opened_connection <- FALSE
      } else {
        con <- DBI::dbConnect(RSQLite::SQLite(), source)
        we_opened_connection <- TRUE
      }

      # check for otel table structure
      tables <- DBI::dbListTables(con)

      if (!"spans" %in% tables) {
        cli::cli_abort(c(
          "Database does not contain OTEL span data",
          "i" = "Expected 'spans' table not found",
          "i" = "Available tables: {paste(tables, collapse = ', ')}"
        ))
      }

      # read spans table
      spans <- DBI::dbReadTable(con, "spans")

      if (nrow(spans) == 0) {
        cli::cli_warn("No spans found in database")
        return(data.frame(
          timestamp = character(),
          session_id = character(),
          event_type = character(),
          stringsAsFactors = FALSE
        ))
      }

      # join with attributes if available
      if ("span_attributes" %in% tables) {
        attrs <- DBI::dbReadTable(con, "span_attributes")

        # pivot attributes to wide format
        if (nrow(attrs) > 0) {
          # Check for and warn about duplicate attribute keys
          dup_check <- attrs |>
            dplyr::group_by(span_id, key) |>
            dplyr::filter(dplyr::n() > 1)

          if (nrow(dup_check) > 0) {
            cli::cli_warn(c(
              "Duplicate attribute keys detected in OTEL data",
              "i" = "Keeping first occurrence of duplicate keys",
              "i" = "Affected spans: {length(unique(dup_check$span_id))} span(s)"
            ))
          }

          # Pivot with deduplication
          attrs_wide <- attrs |>
            dplyr::group_by(span_id, key) |>
            dplyr::slice(1) |>  # Keep first occurrence
            dplyr::group_by(span_id) |>
            dplyr::summarise(
              attributes = list(setNames(
                as.list(value),
                key
              )),
              .groups = "drop"
            )

          # join with spans (handle both camelCase and underscore column names)
          join_col <- if ("spanId" %in% names(spans)) "spanId" else "span_id"
          spans <- spans |>
            dplyr::left_join(attrs_wide, by = stats::setNames("span_id", join_col))
        }
      }

      # join with events if available
      if ("span_events" %in% tables) {
        span_events <- DBI::dbReadTable(con, "span_events")

        if (nrow(span_events) > 0) {
          # group events by span_id
          events_grouped <- span_events |>
            dplyr::group_by(span_id) |>
            dplyr::summarise(
              events = list(dplyr::pick(dplyr::everything())),
              .groups = "drop"
            )

          # join with spans (handle both camelCase and underscore column names)
          join_col <- if ("spanId" %in% names(spans)) "spanId" else "span_id"
          spans <- spans |>
            dplyr::left_join(events_grouped, by = stats::setNames("span_id", join_col))
        }
      }

      # ensure required columns exist
      if (!"name" %in% names(spans)) {
        spans$name <- NA_character_
      }
      if (!"startTimeUnixNano" %in% names(spans) && "start_time" %in% names(spans)) {
        # convert from timestamp to unix nano
        spans$startTimeUnixNano <- as.character(as.numeric(spans$start_time) * 1e9)
      }
      if (!"endTimeUnixNano" %in% names(spans) && "end_time" %in% names(spans)) {
        spans$endTimeUnixNano <- as.character(as.numeric(spans$end_time) * 1e9)
      }

      # convert spans to bidux event schema
      events <- convert_otel_spans_to_events(spans)

      return(events)
    },
    error = function(e) {
      cli::cli_abort("Error reading OTEL SQLite database: {e$message}")
    },
    finally = {
      # only close connection if we opened it
      if (we_opened_connection && !is.null(con)) {
        DBI::dbDisconnect(con)
      }
    }
  )
}

#' Normalize telemetry column names
#' @param events Raw events data frame
#' @return Normalized data frame
#' @keywords internal
normalize_telemetry_columns <- function(events) {
  if (is.list(events) && !is.data.frame(events)) {
    # case where events is still a list
    events <- dplyr::bind_rows(events)
  }

  if (!is.data.frame(events)) {
    cli::cli_abort("Events must be a data frame")
  }

  # common name mappings
  col_mappings <- list(
    timestamp = c("timestamp", "time", "datetime", "created_at"),
    session_id = c("session_id", "session", "sessionid", "session_token"),
    event_type = c("event_type", "type", "event", "action"),
    input_id = c("input_id", "input", "widget_id", "element_id"),
    value = c("value", "input_value", "data"),
    error_message = c("error_message", "message", "error", "detail"),
    output_id = c("output_id", "output", "target_id"),
    navigation_id = c("navigation_id", "page", "tab", "panel")
  )

  # normalize column names
  for (target_col in names(col_mappings)) {
    if (!target_col %in% names(events)) {
      for (alt_name in col_mappings[[target_col]]) {
        if (alt_name %in% names(events)) {
          names(events)[names(events) == alt_name] <- target_col
          break
        }
      }
    }
  }

  # required columns
  required_cols <- c("timestamp", "session_id", "event_type")

  if (all(required_cols %in% names(events))) {
    valid_rows <- complete.cases(events[, required_cols])

    for (col in required_cols) {
      if (is.character(events[[col]])) {
        valid_rows <- valid_rows & nchar(trimws(events[[col]])) > 0
      }
    }

    events <- events[valid_rows, ]

    if (nrow(events) == 0) {
      cli::cli_abort("No valid events found after filtering")
    }
  } else {
    missing_cols <- setdiff(required_cols, names(events))
    cli::cli_abort(
      "Required columns missing from telemetry data: {missing_cols}"
    )
  }

  if (is.character(events$timestamp)) {
    events$timestamp <- as.POSIXct(
      events$timestamp,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )

    if (any(is.na(events$timestamp))) {
      events$timestamp <- as.POSIXct(events$timestamp, tz = "UTC")
    }
  }

  events <- events[order(events$timestamp), ]

  return(events)
}
