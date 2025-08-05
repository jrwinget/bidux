# Variables used in dplyr operations
utils::globalVariables(
  c(
    "concept", # used in bid_concept() filtering
    "relevance", # used in bid_suggest_components() ordering

    # Telemetry analysis variables
    "session_id", # used in find_delayed_sessions and find_error_patterns
    "event_type", # used in find_delayed_sessions
    "error_message", # used in find_error_patterns
    "output_id", # used in find_error_patterns
    "sessions_affected", # used in find_error_patterns
    "session_rate", # used in find_error_patterns
    "count", # used in find_error_patterns
    "page_id", # used in find_navigation_dropoffs
    "first_visit", # used in find_navigation_dropoffs
    "last_visit", # used in find_navigation_dropoffs
    "session_count", # used in find_navigation_dropoffs
    "dropoff_rate", # used in find_navigation_dropoffs
    "change_count", # used in find_confusion_patterns
    "avg_changes", # used in find_confusion_patterns
    "usage_rate", # used in find_unused_inputs

    # Accessibility validation variables
    "screen_reader", # used in validate_accessibility_parameter
    "keyboard_navigation", # used in validate_accessibility_parameter
    "color_contrast", # used in validate_accessibility_parameter
    "text_size", # used in validate_accessibility_parameter
    "alternative_text", # used in validate_accessibility_parameter
    "focus_indicators", # used in validate_accessibility_parameter
    "semantic_markup", # used in validate_accessibility_parameter
    "aria_labels", # used in validate_accessibility_parameter

    # Concept matching variables
    "category", # used in concept matching functions
    "description", # used in concept descriptions
    "implementation_tip", # used in concept implementation tips
    "bias_type", # used in bias mapping functions
    "mitigation_strategy", # used in bias mitigation functions
    "confidence", # used in theory mappings
    "keywords", # used in theory mappings
    "theory", # used in theory mappings and suggestions
    "layout", # used in layout mappings
    "components", # used in component suggestions

    # Stage validation variables
    "stage", # used in stage validation
    "problem", # used in problem validation
    "evidence", # used in evidence validation
    "target_audience", # used in audience validation
    "central_question", # used in question validation
    "hook", # used in story hook validation
    "context", # used in story context validation
    "tension", # used in story tension validation
    "resolution", # used in story resolution validation
    "user_personas", # used in persona validation
    "concepts", # used in concept validation
    "accessibility", # used in accessibility validation
    "bias_mitigations", # used in bias mitigation validation
    "interaction_principles", # used in interaction principle validation

    # Component suggestion variables
    "framework", # used in component framework filtering
    "stage_relevance", # used in stage-specific component scoring
    "layout_score", # used in layout-specific component scoring
    "concept_score", # used in concept-specific component scoring
    "bias_score", # used in bias-specific component scoring
    "total_score", # used in final component scoring
    "component_name", # used in component identification
    "component_type", # used in component type filtering
    "component_description", # used in component descriptions

    # Additional telemetry variables
    "navigation_id", # used in find_navigation_dropoffs
    "unique_sessions", # used in find_navigation_dropoffs
    "visit_rate", # used in find_navigation_dropoffs
    "input_id", # used in find_unused_inputs
    "sessions_used" # used in find_unused_inputs
  )
)

# Import required functions from base packages
#' @importFrom stats complete.cases
NULL
