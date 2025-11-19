## Test environments

-   local: R 4.5.2 on Ubuntu 24.04
-   GitHub Actions (ubuntu-latest): R-devel, R-release, R-oldrel-1
-   GitHub Actions (windows-latest): R-release
-   GitHub Actions (macOS-latest): R-release
-   win-builder: R-devel, R-release
-   macOS: R-release

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Examples and vignettes run quickly; no long-running examples are included.

## Release summary (0.3.3)

This release adds DBI connection support for telemetry ingestion, a new quick suggestions function, and Quarto dashboard compatibility documentation. Key improvements include:

-   **New features**
    -   `bid_quick_suggest()` for immediate UX suggestions without the full 5-stage workflow
    -   DBI connection support in `bid_ingest_telemetry()` and `bid_telemetry()` with custom `table_name` parameter
    -   Flattened tibble format for Structure stage suggestions (`$suggestions_tbl`)
-   **Documentation**
    -   Quarto dashboard compatibility notes across package documentation
    -   Enhanced concept citations and accuracy in bid_concepts_data.csv
-   **Dependencies**
    -   Added withr to Suggests (used in tests)

This release maintains backward compatibility with all existing APIs.

## Rationale for version

`0.3.3` is a feature release following 0.3.2, adding DBI connection support for flexible telemetry ingestion and a streamlined `bid_quick_suggest()` function for rapid UX guidance. Documentation updates clarify compatibility with both Shiny and Quarto dashboards.

## Compatibility

-   One test dependency added (withr to Suggests)
-   No breaking changes to existing APIs
-   Reverse dependencies: none on CRAN

## Additional policy notes

-   All URLs use HTTPS and were verified at build time
-   No non-ASCII or unusual encodings in Rd files
-   No references to external resources are required at runtime
