## Test environments

-   local: R 4.5.2 on Ubuntu 24.04
-   GitHub Actions (ubuntu-latest): R-devel, R-release, R-oldrel-1
-   GitHub Actions (windows-latest): R-release
-   GitHub Actions (macOS-latest): R-release
-   win-builder: R-devel, R-release
-   R-hub: Windows Server, Ubuntu, macOS

## R CMD check results

0 errors | 0 warnings | 0 notes

Examples and vignettes run quickly; no long-running examples are included.

## Release summary (0.4.0)

This is a minor version update from 0.3.3 to 0.4.0. Key changes include:

-   **OpenTelemetry (OTEL) integration** for Shiny >= 1.12.0: `bid_ingest_telemetry()` and `bid_telemetry()` now support native Shiny OTEL span data. New exported utilities: `read_otel_json()`, `read_otel_sqlite()`, `convert_otel_spans_to_events()`, `detect_otel_json()`.
-   **New `bid_suggest_analytics()`** function recommends alternative analytics solutions for static Quarto dashboards where shiny.telemetry is not available.
-   **API modernization**: Removed the deprecated nested `data_story` format (`variables`/`relationships` parameters) from `new_data_story()`. Removed layout auto-selection from `bid_structure()`. Removed layout-specific bias mitigations from `bid_anticipate()`. See `vignette("api-modernization")` for migration guidance.

## Breaking changes

-   `new_data_story()`: `variables` and `relationships` parameters removed. Use the flat API: `new_data_story(hook, context, tension, resolution)`.
-   `bid_structure()`: layout auto-selection and `layout` field removed from results.
-   `bid_anticipate()`: layout-dependent bias mappings removed.

## Compatibility

-   `memoise` added to Imports (used for internal data caching in `data_concepts.R`)
-   `otel` and `otelsdk` remain in Suggests (optional OTEL support)
-   Reverse dependencies: none on CRAN

## Additional policy notes

-   All URLs use HTTPS and were verified at build time
-   No non-ASCII or unusual encodings in Rd files
-   No references to external resources are required at runtime
