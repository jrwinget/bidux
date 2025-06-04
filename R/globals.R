# Variables used in dplyr operations
utils::globalVariables(
  c(
    "concept", # used in bid_concept() filtering
    "relevance" # used in bid_suggest_components() ordering
  )
)
