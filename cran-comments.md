## Test environments
* local: R 4.5.1 on Ubuntu 24.04
* GitHub Actions (ubuntu-latest): R-devel, R-release, R-oldrel-1
* GitHub Actions (windows-latest): R-release
* GitHub Actions (macOS-latest): R-release
* win-builder: R-devel, R-release
* macOS: R-release

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Examples and vignettes run quickly; no long-running examples are included.

## Release summary (0.3.1)
This is a follow-up release to 0.3.0 (accepted 2025-08-29).
It fixes functional bugs introduced during the 0.3.0 refactor, restores proper stage handling, and expands test coverage. It also adds stable telemetry objects, bridge functions, and documentation updates. Highlights (see NEWS.md for full details):

**Major fixes**
* Corrected stage progression and `previous_stage` validation to enforce the proper BID order (**Interpret → Notice → Anticipate → Structure → Validate**).
* Fixed `bid_anticipate()` to generate concept-specific bias mitigations (instead of generic ones).
* Improved handling of `previous_stage` when supplied as a `data.frame`, preventing errors in downstream functions.
* Resolved migration issues by adding `stage_number_previous` attributes and migration notices.

**Improvements**
* Hybrid telemetry objects: `bid_ingest_telemetry()` now returns `bid_issues` hybrids (list + tidy methods) for backward compatibility and modern workflows.
* New tidy telemetry API: `bid_telemetry()` returns organized `bid_issues_tbl` tibbles for seamless dplyr use.
* Bridge functions (`bid_notice_issue()`, `bid_notices()`, `bid_address()`, `bid_pipeline()`) connect telemetry to BID stages.
* `bid_structure()` accepts telemetry-informed flags to adapt layout and suggestion scoring.
* `bid_validate()` supports provenance tracking via `telemetry_refs`.
* Expanded test coverage across telemetry methods, suggestion rules, and edge cases.

**Deprecations**
* Layout auto-selection in `bid_structure()` and layout-specific bias mitigations in `bid_anticipate()` are deprecated (removal in 0.4.0). Current code continues to work with warnings.

**Documentation**
* Vignettes updated with new telemetry API examples, corrected stage numbering, and migration notes.
* README refreshed with new package hex logo.

## Rationale for version
`0.3.1` is a patch release following 0.3.0, needed to fix regressions that broke functionality in user workflows. It also introduces stable telemetry objects, improves migration support, and adds comprehensive test coverage to ensure long-term robustness.

## Compatibility
* No new dependencies added since 0.3.0.
* Reverse dependencies: none on CRAN.

## Additional policy notes
* All URLs use HTTPS and were verified at build time.
* No non-ASCII or unusual encodings in Rd files.
* No references to external resources are required at runtime.
