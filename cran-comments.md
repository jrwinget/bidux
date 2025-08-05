## Test environments
* local: R 4.5.1 on Ubuntu 24.04
* GitHub Actions (ubuntu-latest): R-devel, R-release, R-oldrel-1
* GitHub Actions (windows-latest): R-release
* GitHub Actions (macOS-latest): R-release
* win-builder: R-devel, R-release
* macOS: R-release

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Release summary (0.2.0)
* **New telemetry feature:** Add `bid_ingest_telemetry()` to ingest `{shiny.telemetry}` data (SQLite/JSON), apply configurable thresholds, detect UX friction patterns (unused inputs, delayed interactions, error clusters, navigation drop-offs, confusion), and generate BID notices and reports. Includes robust input validation, format auto-detection, and clearer CLI summaries.
* **Vignette & docs:** New telemetry-integration vignette; README reorganized (overview, features, install, concise quick start, telemetry section); adds CRAN downloads badge; CONTRIBUTING guide and Code of Conduct added; standardized issue templates.
* **Reliability & robustness:** Safer JSON handling (arrays and JSON-lines), column normalization, stricter existence/format checks; safer subsetting (`drop = FALSE`) and indexing (`seq_len()`); improved defaults/threshold merging and user messages.
* **API/UX quality:** Refactored `print.bid_stage()` into modular helpers with stage-specific display rules; unified external CSV/data loading via `load_external_data()`; consolidated suggestion generation and parameter validation helpers for consistent behavior.
* **Codebase consistency:** Standardized file naming (dash-separated) and centralized telemetry logic into `telemetry.R`; moved shared helpers into `utils.R`; style/formatting normalized across functions for readability and maintainability.

## Compatibility
* **Breaking changes:** None expected. Existing exported APIs are preserved; telemetry is additive. Internal refactors (file moves, modularization) do not change function names or return types.
* **New dependencies:** Adds `DBI` and `RSQLite` to `Imports` for telemetry backends; `stats` (via `importFrom(stats, complete.cases)`).
* **Dependency policy:** Relaxed version constraints for `cli`, `dplyr`, and `jsonlite`; expanded `NAMESPACE` with explicit imports.
* **Behavioral notes:** Telemetry readers enforce stricter validation/normalization and may emit clearer warnings for malformed inputs; percentage formatting in some summaries standardized. No changes to object classes or public argument contracts.
