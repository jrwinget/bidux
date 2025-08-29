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

## Resubmission note
* This is a resubmission following feedback from 2025-08-18.
* All relative links were replaced with absolute HTTPS URLs to resolve the "invalid file URIs" issue.
* Documentation has been rebuilt to ensure links and references render correctly across platforms.

## Release summary (0.3.0)
This release updates the BID workflow, removes a deprecated argument, and adds automated layout selection and improved suggestions. Highlights (see NEWS for details and a migration guide):

**Breaking / API changes**
* **Stage order:** BID sequence updated to **Interpret → Notice → Anticipate → Structure → Validate** for a more natural workflow to be presented at posit::conf(2025).
* **`bid_structure()` argument removal:** The `layout` parameter was removed; layout is now selected automatically via deterministic heuristics based on prior stages. A helpful error is thrown if `layout` is supplied.
* **Field name normalization:** Several output field names were standardized (e.g., `previous_question` → `previous_central_question`, `previous_story_hook` → `previous_hook`, `user_personas` → `personas` in `bid_interpret` output). These changes affect returned object fields, not function signatures.

**Deprecations (with warnings)**
* `target_audience` in `bid_notice()` (use `data_story`/`user_personas` in `bid_interpret()` instead). Scheduled for removal in 0.4.0.
* `interaction_principles` in `bid_anticipate()` (replaced by `include_accessibility`). Scheduled for removal in 0.4.0.

**New features / improvements**
* Automatic layout inference in `bid_structure()`; returns ranked, concept-grouped UI/UX suggestions with rationale and relevance scores.
* Accessibility-focused bias mitigations in `bid_anticipate()` via `include_accessibility = TRUE`.
* `bid_validate()` gains `include_exp_design`, `include_telemetry`, and `include_empower_tools` flags.
* Improved context propagation across stages; clearer CLI messages.
* Comprehensive tests covering heuristic branches, deprecation warnings, error handling, and telemetry-aware scoring.

A concise migration guide with old/new code is included in `NEWS.md` under "MIGRATION GUIDE".

## Rationale for version
`0.3.0` reflects a **backward-incompatible** removal of the `layout` parameter in `bid_structure()` and standardized field names. The prior submission (`0.2.0`) was not accepted to CRAN; this resubmission supersedes it and documents all changes clearly in NEWS and docs.

## Compatibility
* New dependencies: **none** beyond those introduced for 0.2.0 (DBI, RSQLite; `importFrom(stats, complete.cases)`).
* Reverse dependencies: none on CRAN.

## Additional policy notes
* All URLs use HTTPS and were verified at build time.
* No non-ASCII or unusual encodings in Rd files.
* No references to external resources are required at runtime.
