# Advanced BID Workflows: Power User Techniques

``` r
library(bidux)
library(dplyr)
```

## Introduction: Beyond Basic BID Implementation

This vignette covers advanced techniques for experienced R users who
want to integrate the BID framework deeply into their data science and
dashboard development workflows.

**Target audience**: Data scientists, senior Shiny developers, and
UX-aware analysts who want to: - Automate BID workflow integration -
Scale UX analysis across multiple dashboards - Create custom behavioral
science extensions - Build data-driven UX improvement processes

## Advanced Pattern 1: Automated BID Analysis Pipeline

### The Challenge

You maintain 20+ dashboards across different business units. Manually
applying the BID framework to each is time-intensive.

### Solution: Systematic BID Pipeline

``` r
# Create a comprehensive BID analysis function
analyze_dashboard_ux <- function(
    dashboard_config,
    telemetry_path = NULL,
    telemetry_sensitivity = "moderate") {
  # Stage 1: Interpret (from configuration)
  interpret_stage <- bid_interpret(
    central_question = dashboard_config$central_question,
    data_story = dashboard_config$data_story,
    user_personas = dashboard_config$personas
  )

  # If telemetry exists, integrate it into the workflow
  if (!is.null(telemetry_path) && file.exists(telemetry_path)) {
    # Ingest telemetry with sensitivity preset (new in 0.3.2)
    # Choose "strict" for new dashboards, "moderate" for established ones,
    # or "relaxed" for mature, stable dashboards
    telemetry_issues <- bid_telemetry(
      telemetry_path,
      thresholds = bid_telemetry_presets(telemetry_sensitivity)
    )

    # Convert top issues to Notice stages
    notice_stages <- bid_notices(
      issues = telemetry_issues |>
        filter(severity %in% c("critical", "high")) |>
        slice_head(n = 3),
      previous_stage = interpret_stage
    )

    # Use the most critical issue as primary focus
    primary_notice <- notice_stages[[1]]
  } else {
    # Manual problem definition if no telemetry
    primary_notice <- bid_notice(
      previous_stage = interpret_stage,
      problem = dashboard_config$known_problems,
      evidence = dashboard_config$evidence
    )
  }

  # Stage 3: Anticipate with domain-specific biases
  anticipate_stage <- bid_anticipate(
    previous_stage = primary_notice,
    bias_mitigations = dashboard_config$bias_mitigations %||%
      get_domain_biases(dashboard_config$domain)
  )

  # Stage 4: Structure with telemetry flags if available
  structure_flags <- if (
    !is.null(telemetry_path) && file.exists(telemetry_path)
  ) {
    bid_flags(telemetry_issues)
  } else {
    NULL
  }

  structure_stage <- bid_structure(
    previous_stage = anticipate_stage,
    telemetry_flags = structure_flags
  )

  # Stage 5: Validate with domain-specific next steps
  validate_stage <- bid_validate(
    previous_stage = structure_stage,
  )

  return(validate_stage)
}

# Domain-specific bias patterns
get_domain_biases <- function(domain) {
  bias_patterns <- list(
    "finance" = list(
      loss_aversion = "Show both gains and losses clearly with proper context",
      anchoring = "Provide multiple reference points (budget, previous period, industry average)",
      confirmation_bias = "Include contrarian indicators and risk metrics"
    ),
    "marketing" = list(
      attribution_bias = "Show multi-touch attribution to avoid overvaluing last-click",
      survivorship_bias = "Include data on discontinued campaigns and failed experiments",
      framing = "Toggle between cost-per-acquisition and customer-lifetime-value views"
    ),
    "operations" = list(
      availability_bias = "Surface less-visible but important operational metrics",
      recency_bias = "Balance recent performance with longer-term trends",
      cognitive_load = "Use progressive disclosure for complex operational dashboards"
    )
  )

  return(
    bias_patterns[[domain]] %||%
      list(
        anchoring = "Provide appropriate reference points",
        framing = "Consider multiple perspectives on the same data",
        confirmation_bias = "Include challenging or contrarian data points"
      )
  )
}

# Batch analyze multiple dashboards
analyze_dashboard_portfolio <- function(dashboard_configs) {
  results <- map(dashboard_configs, analyze_dashboard_ux)
  names(results) <- map_chr(dashboard_configs, "name")

  # Generate portfolio-level insights
  portfolio_summary <- summarize_portfolio_ux(results)

  return(
    list(
      individual_analyses = results,
      portfolio_summary = portfolio_summary,
      improvement_priorities = rank_improvement_opportunities(results)
    )
  )
}

# Example usage
dashboard_portfolio <- list(
  list(
    name = "Executive Dashboard",
    domain = "finance",
    central_question = "How is the business performing this quarter?",
    data_story = list(
      hook = "Quarterly performance varies significantly across business units",
      context = "Board meeting preparation requires clear performance narrative",
      tension = "Current reports are too detailed for executive review",
      resolution = "Provide executive summary with drill-down capability"
    ),
    personas = list(
      list(
        name = "CEO",
        technical_level = "Basic",
        time_constraints = "5 minutes"
      ),
      list(
        name = "CFO",
        technical_level = "Intermediate",
        focus = "Financial metrics"
      )
    ),
    known_problems = "Information overload in current quarterly reviews",
    evidence = "Board meetings consistently run over time due to data interpretation"
  ),
  # Additional dashboard configurations...
)

# Run portfolio analysis
portfolio_results <- analyze_dashboard_portfolio(dashboard_portfolio)
```

### Telemetry Sensitivity Presets for Different Contexts

The
[`bid_telemetry_presets()`](https://jrwinget.github.io/bidux/reference/bid_telemetry_presets.md)
function (new in 0.3.2) provides pre-configured threshold sets that
eliminate manual threshold tuning. Choose the appropriate sensitivity
based on your dashboard’s maturity and criticality:

``` r
# STRICT: For critical applications or new dashboards
# Flags even minor issues (e.g., inputs used by < 2% of sessions)
critical_dashboard_analysis <- function(telemetry_path) {
  # Use strict thresholds for critical business dashboards
  issues <- bid_telemetry(
    telemetry_path,
    thresholds = bid_telemetry_presets("strict")
  )

  # Strict preset catches early warning signs
  # Example thresholds:
  #   - unused_input_threshold: 0.02 (2% usage)
  #   - delay_threshold_secs: 20 seconds
  #   - error_rate_threshold: 0.05 (5% of sessions)

  return(issues)
}

# MODERATE: Default balanced approach for most applications
standard_dashboard_analysis <- function(telemetry_path) {
  # Balanced sensitivity for established dashboards
  issues <- bid_telemetry(
    telemetry_path,
    thresholds = bid_telemetry_presets("moderate")
  )

  # Moderate preset provides good signal-to-noise ratio
  # Example thresholds:
  #   - unused_input_threshold: 0.05 (5% usage)
  #   - delay_threshold_secs: 30 seconds
  #   - error_rate_threshold: 0.1 (10% of sessions)

  return(issues)
}

# RELAXED: For mature, stable dashboards
mature_dashboard_analysis <- function(telemetry_path) {
  # Only flag major issues in stable production dashboards
  issues <- bid_telemetry(
    telemetry_path,
    thresholds = bid_telemetry_presets("relaxed")
  )

  # Relaxed preset focuses on severe problems
  # Example thresholds:
  #   - unused_input_threshold: 0.1 (10% usage)
  #   - delay_threshold_secs: 60 seconds
  #   - error_rate_threshold: 0.2 (20% of sessions)

  return(issues)
}

# Adaptive sensitivity based on dashboard lifecycle
adaptive_telemetry_analysis <- function(
    telemetry_path,
    dashboard_age_days,
    is_critical = FALSE) {
  # Choose sensitivity based on dashboard maturity and criticality
  sensitivity <- if (is_critical) {
    "strict"
  } else if (dashboard_age_days < 30) {
    "strict" # New dashboards need close monitoring
  } else if (dashboard_age_days < 180) {
    "moderate" # Maturing dashboards
  } else {
    "relaxed" # Stable, mature dashboards
  }

  issues <- bid_telemetry(
    telemetry_path,
    thresholds = bid_telemetry_presets(sensitivity)
  )

  cli::cli_alert_info(
    "Using {sensitivity} sensitivity for {dashboard_age_days}-day-old dashboard"
  )

  return(issues)
}

# Compare findings across different sensitivity levels
compare_sensitivity_levels <- function(telemetry_path) {
  strict_issues <- bid_telemetry(
    telemetry_path,
    thresholds = bid_telemetry_presets("strict")
  )

  moderate_issues <- bid_telemetry(
    telemetry_path,
    thresholds = bid_telemetry_presets("moderate")
  )

  relaxed_issues <- bid_telemetry(
    telemetry_path,
    thresholds = bid_telemetry_presets("relaxed")
  )

  # Compare issue counts at different sensitivity levels
  comparison <- data.frame(
    sensitivity = c("strict", "moderate", "relaxed"),
    total_issues = c(
      nrow(strict_issues),
      nrow(moderate_issues),
      nrow(relaxed_issues)
    ),
    critical_issues = c(
      nrow(filter(strict_issues, severity == "critical")),
      nrow(filter(moderate_issues, severity == "critical")),
      nrow(filter(relaxed_issues, severity == "critical"))
    )
  )

  return(comparison)
}
```

## Advanced Pattern 2: Custom Behavioral Science Extensions

### Extending the Concept Dictionary

``` r
# Add domain-specific behavioral concepts
add_custom_concepts <- function() {
  # Define custom concepts for your domain
  custom_finance_concepts <- tibble(
    concept = c(
      "Risk Perception Bias",
      "Mental Accounting",
      "Temporal Discounting"
    ),
    category = "Financial Psychology",
    description = c(
      "Tendency to perceive identical risks differently based on presentation context",
      "Treating money differently based on its source or intended use",
      "Overvaluing immediate rewards relative to future benefits"
    ),
    implementation_tips = c(
      "Present risks in multiple formats (percentages, frequencies, visual scales)",
      "Show total portfolio impact rather than individual position P&L",
      "Include time-based context and compound effect visualizations"
    ),
    shiny_components = c(
      "plotly for interactive risk visualization, bslib progress bars for probability",
      "DT tables with conditional formatting, reactable grouping features",
      "echarts4r timeline components, animated value transitions"
    )
  )

  # You could extend the package concept dictionary (advanced users only)
  # This would require package development workflow

  return(custom_finance_concepts)
}

# Create domain-specific BID analysis functions
analyze_financial_dashboard <- function(config, custom_concepts = NULL) {
  # Load custom concepts if provided
  if (!is.null(custom_concepts)) {
    # Use custom concepts in analysis
    relevant_concepts <- filter(
      custom_concepts,
      grepl(config$domain_keywords, concept, ignore.case = TRUE)
    )
  }

  # Apply standard BID workflow with custom extensions
  result <- analyze_dashboard_ux(config)

  # Add domain-specific analysis
  result$domain_insights <- generate_domain_insights(result, custom_concepts)
  result$specialized_suggestions <- get_domain_suggestions(
    result,
    config$domain
  )

  return(result)
}

# Generate domain-specific insights
generate_domain_insights <- function(bid_result, custom_concepts = NULL) {
  insights <- list()

  # Analyze layout choice against domain best practices
  layout <- bid_result$layout[1]

  if (layout == "dual_process") {
    insights$layout_analysis <- "Dual-process layout chosen. Good for financial dashboards requiring both summary and detailed analysis."
  }

  # Check for domain-specific bias considerations
  if (!is.null(custom_concepts)) {
    # Suggest additional bias mitigations based on custom concepts
    insights$additional_biases <- suggest_domain_biases(
      bid_result,
      custom_concepts
    )
  }

  return(insights)
}
```

## Advanced Pattern 3: A/B Testing for UX Improvements

### Systematic UX Experimentation

``` r
# Framework for testing UX improvements
design_ux_experiment <- function(current_design, proposed_design, hypothesis) {
  experiment_design <- list(
    hypothesis = hypothesis,
    primary_metrics = c(
      "time_to_first_interaction",
      "task_completion_rate",
      "user_satisfaction_score",
      "session_duration"
    ),
    secondary_metrics = c(
      "error_rate",
      "feature_adoption_rate",
      "return_visit_rate"
    ),
    variants = list(
      control = current_design,
      treatment = proposed_design
    ),
    sample_size_calculation = calculate_ux_sample_size(
      baseline_completion_rate = 0.65,
      minimum_detectable_effect = 0.10,
      power = 0.80,
      alpha = 0.05
    )
  )

  return(experiment_design)
}

# Calculate required sample size for UX experiments
calculate_ux_sample_size <- function(
    baseline_completion_rate,
    minimum_detectable_effect,
    power = 0.80,
    alpha = 0.05) {
  # Using power analysis for proportion tests
  p1 <- baseline_completion_rate
  p2 <- p1 + minimum_detectable_effect

  # Simplified calculation (use power.prop.test() for precise calculation)
  pooled_p <- (p1 + p2) / 2
  pooled_variance <- pooled_p * (1 - pooled_p)

  z_alpha <- qnorm(1 - alpha / 2)
  z_beta <- qnorm(power)

  n_per_group <- 2 * pooled_variance * (z_alpha + z_beta)^2 / (p2 - p1)^2

  return(
    list(
      n_per_group = ceiling(n_per_group),
      total_n = ceiling(2 * n_per_group),
      assumptions = list(
        baseline_rate = p1,
        target_rate = p2,
        effect_size = minimum_detectable_effect
      )
    )
  )
}

# Analyze UX experiment results
analyze_ux_experiment <- function(experiment_data, experiment_design) {
  # Primary analysis: task completion rate
  completion_test <- prop.test(
    x = c(
      sum(experiment_data$control$completed),
      sum(experiment_data$treatment$completed)
    ),
    n = c(nrow(experiment_data$control), nrow(experiment_data$treatment))
  )

  # Secondary analysis: time to completion
  time_test <- t.test(
    experiment_data$treatment$completion_time,
    experiment_data$control$completion_time,
    alternative = "less" # Hypothesis: treatment is faster
  )

  # Effect size calculation
  effect_size <- calculate_cohens_d(
    experiment_data$treatment$completion_time,
    experiment_data$control$completion_time
  )

  results <- list(
    completion_rate_test = completion_test,
    completion_time_test = time_test,
    effect_size = effect_size,
    practical_significance = assess_practical_significance(
      completion_test,
      time_test,
      effect_size
    ),
    recommendation = generate_experiment_recommendation(
      completion_test,
      time_test,
      effect_size
    )
  )

  return(results)
}

# Example: Test progressive disclosure vs. full information display
progressive_disclosure_experiment <- function() {
  # Current design: all information visible
  current_design <- list(
    name = "Full Information Display",
    description = "All metrics and filters visible simultaneously",
    implementation = "Traditional dashboard with all components loaded"
  )

  # Proposed design: progressive disclosure
  proposed_design <- list(
    name = "Progressive Disclosure",
    description = "Key metrics first, additional details on request",
    implementation = "Primary KPIs with 'Show Details' interactions"
  )

  # Hypothesis based on BID framework
  hypothesis <- "Progressive disclosure will reduce cognitive load and improve task completion rate for dashboard users (based on Cognitive Load Theory and Choice Overload research)"

  experiment <- design_ux_experiment(
    current_design = current_design,
    proposed_design = proposed_design,
    hypothesis = hypothesis
  )

  return(experiment)
}
```

## Advanced Pattern 4: Continuous UX Monitoring

### Dashboard Health Monitoring System

``` r
# Create UX health monitoring system
create_ux_monitoring_system <- function(
    dashboard_configs,
    telemetry_connections) {
  monitoring_system <- list(
    dashboards = dashboard_configs,
    telemetry_sources = telemetry_connections,
    health_checks = define_ux_health_checks(),
    alert_thresholds = define_alert_thresholds(),
    reporting_schedule = "weekly"
  )

  return(monitoring_system)
}

# Define UX health check metrics
define_ux_health_checks <- function() {
  list(
    cognitive_load_indicators = c(
      "session_abandonment_rate",
      "time_to_first_interaction",
      "filter_usage_distribution",
      "error_rate_by_component"
    ),
    user_success_metrics = c(
      "task_completion_rate",
      "time_to_insight",
      "feature_adoption_rate",
      "user_satisfaction_nps"
    ),
    behavioral_red_flags = c(
      "rapid_repeated_clicks",
      "excessive_back_navigation",
      "long_pause_before_action",
      "high_exit_rate_on_entry"
    )
  )
}

# Automated UX health reporting
generate_ux_health_report <- function(
    monitoring_system,
    time_period = "week",
    use_adaptive_sensitivity = TRUE) {
  health_data <- map(
    monitoring_system$telemetry_sources,
    function(source) {
      # Use adaptive sensitivity based on dashboard maturity
      if (use_adaptive_sensitivity && !is.null(source$dashboard_age_days)) {
        sensitivity <- if (source$is_critical %||% FALSE) {
          "strict"
        } else if (source$dashboard_age_days < 30) {
          "strict"
        } else if (source$dashboard_age_days < 180) {
          "moderate"
        } else {
          "relaxed"
        }
        thresholds <- bid_telemetry_presets(sensitivity)
      } else {
        # Default to moderate sensitivity
        thresholds <- bid_telemetry_presets("moderate")
        sensitivity <- "moderate"
      }

      issues <- bid_telemetry(
        source$path,
        thresholds = thresholds
      )

      health_scores <- calculate_ux_health_scores(issues)
      trend_analysis <- calculate_ux_trends(issues, source$historical_data)

      list(
        dashboard = source$dashboard_name,
        sensitivity_used = sensitivity,
        current_health = health_scores,
        trends = trend_analysis,
        recommendations = generate_health_recommendations(
          health_scores,
          trend_analysis
        )
      )
    }
  )

  # Portfolio-level insights
  portfolio_health <- aggregate_portfolio_health(health_data)

  # Generate executive summary
  executive_summary <- create_ux_executive_summary(portfolio_health)

  report <- list(
    period = time_period,
    executive_summary = executive_summary,
    dashboard_details = health_data,
    portfolio_trends = portfolio_health,
    action_items = prioritize_ux_improvements(health_data)
  )

  return(report)
}

# Calculate UX health scores
calculate_ux_health_scores <- function(telemetry_issues) {
  # Weight issues by severity and impact
  severity_weights <- c("critical" = 5, "high" = 3, "medium" = 2, "low" = 1)

  issue_impact <- telemetry_issues |>
    mutate(
      weighted_impact = case_when(
        severity == "critical" ~ 5,
        severity == "high" ~ 3,
        severity == "medium" ~ 2,
        TRUE ~ 1
      )
    ) |>
    summarize(
      total_issues = n(),
      weighted_impact_score = sum(weighted_impact),
      critical_issues = sum(severity == "critical"),
      .groups = "drop"
    )

  # Calculate health score (0-100, higher is better)
  health_score <- pmax(0, 100 - (issue_impact$weighted_impact_score * 2))

  health_rating <- case_when(
    health_score >= 85 ~ "Excellent",
    health_score >= 70 ~ "Good",
    health_score >= 55 ~ "Fair",
    TRUE ~ "Needs Attention"
  )

  return(
    list(
      score = health_score,
      rating = health_rating,
      issue_breakdown = issue_impact,
      primary_concerns = get_primary_concerns(telemetry_issues)
    )
  )
}

# Example implementation
monitor_dashboard_portfolio <- function() {
  # Set up monitoring for multiple dashboards
  portfolio_monitoring <- create_ux_monitoring_system(
    dashboard_configs = list(
      list(name = "Executive Dashboard", business_unit = "Corporate"),
      list(name = "Sales Analytics", business_unit = "Sales"),
      list(name = "Marketing Performance", business_unit = "Marketing")
    ),
    telemetry_connections = list(
      list(
        dashboard_name = "Executive Dashboard",
        path = "exec_dashboard_telemetry.sqlite",
        historical_data = "exec_dashboard_history.rds",
        dashboard_age_days = 365, # Mature dashboard
        is_critical = TRUE # Executive-facing = critical
      ),
      list(
        dashboard_name = "Sales Analytics",
        path = "sales_dashboard_telemetry.sqlite",
        historical_data = "sales_dashboard_history.rds",
        dashboard_age_days = 45, # Recently launched
        is_critical = FALSE
      ),
      list(
        dashboard_name = "Marketing Performance",
        path = "marketing_dashboard_telemetry.sqlite",
        historical_data = "marketing_dashboard_history.rds",
        dashboard_age_days = 15, # Brand new
        is_critical = FALSE
      )
    )
  )

  # Generate weekly health report with adaptive sensitivity
  # Executive Dashboard: uses "strict" (critical = TRUE)
  # Sales Analytics: uses "moderate" (45 days old)
  # Marketing Performance: uses "strict" (15 days old, new dashboard)
  weekly_report <- generate_ux_health_report(
    portfolio_monitoring,
    use_adaptive_sensitivity = TRUE
  )

  return(weekly_report)
}
```

## Advanced Pattern 5: Custom BID Stage Extensions

### Creating Domain-Specific BID Stages

``` r
# Create custom BID stage for specific domains
create_custom_bid_stage <- function(
    stage_name,
    stage_function,
    validation_rules) {
  # Example: Security-focused BID stage for sensitive data dashboards
  bid_security_stage <- function(
      previous_stage,
      security_requirements = NULL,
      compliance_framework = "GDPR",
      data_sensitivity_level = "medium") {
    validate_previous_stage(previous_stage, stage_name)

    # Security-specific analysis
    security_analysis <- assess_dashboard_security_ux(
      previous_stage = previous_stage,
      requirements = security_requirements,
      framework = compliance_framework,
      sensitivity = data_sensitivity_level
    )

    # Generate security-aware UX recommendations
    security_recommendations <- generate_security_ux_recommendations(
      security_analysis,
      previous_stage
    )

    # Create result tibble
    result_data <- tibble(
      stage = stage_name,
      security_level = data_sensitivity_level,
      compliance_framework = compliance_framework,
      security_recommendations = paste(
        security_recommendations,
        collapse = "; "
      ),
      previous_layout = safe_column_access(previous_stage, "layout"),
      timestamp = Sys.time()
    )

    # Return as bid_stage object
    return(bid_stage(stage_name, result_data))
  }

  return(bid_security_stage)
}

# Example: Accessibility-focused analysis using existing functions
create_accessibility_analysis <- function(
    previous_stage,
    wcag_level = "AA",
    assistive_tech_support = TRUE,
    target_disabilities = c("visual", "motor", "cognitive")) {
  # Use existing bid_concept function to get accessibility recommendations
  contrast_info <- bid_concept("Accessibility Contrast")
  keyboard_info <- bid_concept("Keyboard Navigation")
  screen_reader_info <- bid_concept("Screen Reader Compatibility")

  # Create basic accessibility recommendations using existing concepts
  accessibility_recommendations <- c(
    contrast_info$implementation_tips[1],
    keyboard_info$implementation_tips[1],
    screen_reader_info$implementation_tips[1]
  )

  # Create a summary rather than a bid_stage since this is just an example
  accessibility_analysis <- list(
    wcag_level = wcag_level,
    assistive_tech_support = assistive_tech_support,
    target_disabilities = target_disabilities,
    recommendations = accessibility_recommendations,
    concepts_referenced = c("Accessibility Contrast", "Keyboard Navigation", "Screen Reader Compatibility")
  )

  return(accessibility_analysis)
}

# Integration with main BID workflow
extended_bid_workflow <- function(config) {
  # Standard BID stages
  interpret_stage <- bid_interpret(
    central_question = config$central_question,
    data_story = config$data_story
  )

  notice_stage <- bid_notice(
    previous_stage = interpret_stage,
    problem = config$problem,
    evidence = config$evidence
  )

  anticipate_stage <- bid_anticipate(
    previous_stage = notice_stage,
    bias_mitigations = config$bias_mitigations
  )

  structure_stage <- bid_structure(previous_stage = anticipate_stage)

  # Custom accessibility analysis
  if (config$include_accessibility) {
    accessibility_analysis <- create_accessibility_analysis(
      previous_stage = structure_stage,
      wcag_level = config$accessibility_requirements$wcag_level
    )
    # Note: This analysis can inform the validate stage
    final_stage <- structure_stage
  } else {
    final_stage <- structure_stage
  }

  # Validation with all insights
  validate_stage <- bid_validate(
    previous_stage = final_stage,
    summary_panel = config$summary_panel,
    next_steps = config$next_steps
  )

  return(validate_stage)
}
```

## Integration with Existing Data Science Workflows

### Embedding BID in Data Science Projects

``` r
# Integrate BID into standard data science project structure
create_bid_project_template <- function(
    project_name,
    project_type = "dashboard") {
  project_structure <- list(
    "01-data-exploration/" = "Standard EDA and data validation",
    "02-user-research/" = "BID Stage 1 (Interpret) - user needs analysis",
    "03-problem-identification/" = "BID Stage 2 (Notice) - friction point analysis",
    "04-behavioral-analysis/" = "BID Stage 3 (Anticipate) - bias mitigation planning",
    "05-interface-design/" = "BID Stage 4 (Structure) - layout and UX design",
    "06-validation-testing/" = "BID Stage 5 (Validate) - user testing and iteration",
    "07-telemetry-analysis/" = "Post-deployment UX monitoring and improvement",
    "bid_analysis.R" = "Consolidated BID framework application",
    "ux_monitoring.R" = "Automated UX health monitoring",
    "README.md" = "Project documentation including BID insights"
  )

  return(project_structure)
}

# Template for BID-informed data science projects
bid_data_science_workflow <- function(project_config) {
  workflow <- list(
    # Phase 1: Data + User Understanding
    phase_1 = list(
      data_exploration = "Standard EDA process",
      user_research = bid_interpret(
        central_question = project_config$research_question,
        data_story = project_config$data_narrative,
        user_personas = project_config$stakeholders
      )
    ),

    # Phase 2: Problem Definition
    phase_2 = list(
      statistical_analysis = "Model building and validation",
      ux_problem_identification = bid_notice(
        previous_stage = workflow$phase_1$user_research,
        problem = project_config$interface_challenges,
        evidence = project_config$user_feedback
      )
    ),

    # Phase 3: Solution Design
    phase_3 = list(
      model_interpretation = "Feature importance and model explanation",
      behavioral_considerations = bid_anticipate(
        previous_stage = workflow$phase_2$ux_problem_identification,
        bias_mitigations = project_config$cognitive_considerations
      ),
      interface_structure = bid_structure(
        previous_stage = workflow$phase_3$behavioral_considerations
      )
    ),

    # Phase 4: Validation & Deployment
    phase_4 = list(
      model_validation = "Standard model performance validation",
      ux_validation = bid_validate(
        previous_stage = workflow$phase_3$interface_structure,
        summary_panel = project_config$success_criteria,
        next_steps = project_config$iteration_plan
      )
    )
  )

  return(workflow)
}
```

## Best Practices for Advanced Users

### 1. Systematic Documentation

``` r
# Helper function to extract BID stage summary using existing functions
extract_bid_summary <- function(bid_result) {
  if (inherits(bid_result, "bid_stage")) {
    # Single stage, extract key information
    stage_info <- list(
      stage = get_stage(bid_result),
      timestamp = bid_result$timestamp[1],
      key_fields = names(bid_result)[!names(bid_result) %in% c("stage", "timestamp")]
    )
    return(stage_info)
  } else if (is.list(bid_result)) {
    # Multiple stages, summarize each
    summary_list <- lapply(bid_result, function(stage) {
      if (inherits(stage, "bid_stage")) {
        list(
          stage = get_stage(stage),
          timestamp = stage$timestamp[1]
        )
      } else {
        list(stage = "unknown", timestamp = NA)
      }
    })
    return(summary_list)
  } else {
    return(list(error = "Unable to extract summary from provided object"))
  }
}

# Create comprehensive BID documentation
document_bid_decisions <- function(bid_result, project_context) {
  documentation <- list(
    project_overview = project_context,
    bid_stages_summary = extract_bid_summary(bid_result),
    # Use existing bid_report functionality instead of custom functions
    detailed_report = if (inherits(bid_result, "bid_stage")) {
      "Use bid_report(bid_result) for detailed documentation"
    } else {
      "Provide bid_stage object to generate detailed report"
    }
  )

  return(documentation)
}
```

### 2. Collaborative Workflows

``` r
# Helper function for consensus building using existing concepts
build_consensus_on_bid_decisions <- function(team_members) {
  # Use existing bid_concept to get collaboration guidance
  cooperation_info <- bid_concept("Cooperation & Coordination")

  consensus_framework <- list(
    team_size = length(team_members),
    collaboration_approach = cooperation_info$implementation_tips[1],
    recommended_process = c(
      "Review BID stages individually with each team member",
      "Identify areas of agreement and disagreement",
      "Use bid_concepts() to find relevant behavioral science principles",
      "Document final decisions with rationale"
    ),
    tools = "Use bid_report() to share findings across team"
  )

  return(consensus_framework)
}

# Enable team collaboration on BID analysis using existing functions
create_bid_collaboration_workflow <- function(team_members, project_config) {
  workflow <- list(
    team_composition = list(
      members = team_members,
      roles = c("UX Designer", "Data Analyst", "Product Manager", "Developer")
    ),
    consensus_building = build_consensus_on_bid_decisions(team_members),
    documentation_approach = "Use bid_report() for comprehensive documentation",
    concept_reference = "Use bid_concepts() to explore relevant principles together"
  )

  return(workflow)
}
```

### 3. Continuous Learning

``` r
# Build organizational BID knowledge
build_bid_knowledge_base <- function(completed_projects) {
  knowledge_base <- map_dfr(
    completed_projects,
    function(project) {
      extract_lessons_learned(project$bid_analysis, project$outcomes)
    }
  )

  # Identify patterns and best practices
  patterns <- identify_successful_patterns(knowledge_base)
  anti_patterns <- identify_problematic_patterns(knowledge_base)

  return(
    list(
      knowledge_base = knowledge_base,
      successful_patterns = patterns,
      anti_patterns = anti_patterns,
      recommendations = generate_org_recommendations(patterns, anti_patterns)
    )
  )
}
```

## Practical Example: End-to-End Workflow with Presets

Here’s a complete real-world example combining automated analysis with
telemetry presets:

``` r
# Real-world scenario: Quarterly UX review for multiple dashboards
quarterly_ux_review <- function() {
  # Dashboard portfolio with different maturity levels
  dashboards <- list(
    list(
      name = "C-Suite Executive Dashboard",
      telemetry_path = "data/exec_telemetry.sqlite",
      age_days = 450,
      is_critical = TRUE,
      central_question = "Are executives getting insights efficiently?",
      data_story = new_data_story(
        hook = "Board meetings consume excessive time on data interpretation",
        context = "Executive team needs faster decision support",
        tension = "Current dashboard has too many options",
        resolution = "Streamline to key metrics with progressive disclosure"
      )
    ),
    list(
      name = "Sales Team Analytics",
      telemetry_path = "data/sales_telemetry.sqlite",
      age_days = 60,
      is_critical = FALSE,
      central_question = "Why are sales reps abandoning the dashboard?",
      data_story = new_data_story(
        hook = "Sales dashboard usage dropped 40% in last month",
        context = "Recently redesigned with new features",
        tension = "Unclear if design or data quality issue",
        resolution = "Use telemetry to identify friction points"
      )
    ),
    list(
      name = "Marketing Campaign Tracker",
      telemetry_path = "data/marketing_telemetry.sqlite",
      age_days = 10,
      is_critical = FALSE,
      central_question = "Is the new campaign dashboard intuitive?",
      data_story = new_data_story(
        hook = "Brand new dashboard for campaign tracking",
        context = "Marketing team transitioning from Excel",
        tension = "Need to catch UX issues early",
        resolution = "Aggressive monitoring for first 30 days"
      )
    )
  )

  # Process each dashboard with appropriate sensitivity
  results <- lapply(dashboards, function(dashboard) {
    # Choose sensitivity based on criticality and maturity
    sensitivity <- if (dashboard$is_critical) {
      "strict"
    } else if (dashboard$age_days < 30) {
      "strict" # New dashboards
    } else if (dashboard$age_days < 180) {
      "moderate" # Maturing
    } else {
      "relaxed" # Stable
    }

    cli::cli_h2("Analyzing: {dashboard$name}")
    cli::cli_alert_info(
      "Dashboard age: {dashboard$age_days} days | Sensitivity: {sensitivity}"
    )

    # Run telemetry analysis with appropriate preset
    issues <- bid_telemetry(
      dashboard$telemetry_path,
      thresholds = bid_telemetry_presets(sensitivity)
    )

    if (nrow(issues) == 0) {
      cli::cli_alert_success("No significant UX issues detected")
      return(NULL)
    }

    # Run full BID pipeline on top issues
    interpret_stage <- bid_interpret(
      central_question = dashboard$central_question,
      data_story = dashboard$data_story
    )

    # Convert critical issues to Notice stages
    critical_issues <- issues |>
      filter(severity %in% c("critical", "high")) |>
      slice_head(n = 3)

    if (nrow(critical_issues) > 0) {
      notices <- bid_notices(
        issues = critical_issues,
        previous_stage = interpret_stage
      )

      # Work through BID stages for primary issue
      primary_notice <- notices[[1]]

      anticipate_stage <- bid_anticipate(
        previous_stage = primary_notice
      )

      # Use telemetry flags to inform structure
      flags <- bid_flags(issues)
      structure_stage <- bid_structure(
        previous_stage = anticipate_stage,
        telemetry_flags = flags
      )

      validate_stage <- bid_validate(
        previous_stage = structure_stage
      )

      return(
        list(
          dashboard = dashboard$name,
          sensitivity = sensitivity,
          total_issues = nrow(issues),
          critical_issues = nrow(critical_issues),
          bid_analysis = validate_stage,
          recommendations = extract_recommendations(validate_stage)
        )
      )
    }

    return(NULL)
  })

  # Filter out NULL results
  results <- results[!sapply(results, is.null)]

  # Generate executive summary
  cli::cli_h1("Quarterly UX Review Summary")
  cli::cli_alert_info("Analyzed {length(dashboards)} dashboards")
  cli::cli_alert_warning(
    "{length(results)} dashboards have critical UX issues requiring attention"
  )

  return(results)
}

# Helper to extract recommendations from validate stage
extract_recommendations <- function(validate_stage) {
  # This is a simplified example - customize based on your needs
  if (inherits(validate_stage, "bid_stage")) {
    suggestions <- safe_column_access(validate_stage, "suggestions")
    if (!is.null(suggestions)) {
      return(suggestions)
    }
  }
  return("See full BID analysis for recommendations")
}
```

## Conclusion

These advanced patterns enable you to:

1.  **Automate BID analysis** across dashboard portfolios with
    intelligent sensitivity tuning
2.  **Extend the framework** with domain-specific concepts and stages
3.  **Integrate UX testing** with the same rigor as statistical analysis
4.  **Monitor UX health** continuously with data-driven alerts and
    adaptive thresholds
5.  **Embed behavioral science** systematically into data science
    workflows
6.  **Scale telemetry analysis** using presets that adapt to dashboard
    maturity and criticality

**Key principle for advanced users**: Apply the same systematic,
evidence-based approach you use for data analysis to user experience
design. The BID framework provides the structure—your analytical skills
provide the rigor.

**New in v0.3.2**: The
[`bid_telemetry_presets()`](https://jrwinget.github.io/bidux/reference/bid_telemetry_presets.md)
function eliminates the need for manual threshold tuning, making it
easier to scale UX analysis across multiple dashboards with varying
maturity levels.

For questions about advanced implementations, custom extensions, or
enterprise deployment patterns, consider contributing to the package
development at
[github.com/jrwinget/bidux](https://github.com/jrwinget/bidux).

Remember: Advanced techniques are most effective when they serve your
users’ cognitive needs, not just technical capabilities. Always validate
complex implementations with real user testing.
