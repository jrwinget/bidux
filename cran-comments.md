## Test environments
* local: R 4.5.1 on Ubuntu 24.04
* GitHub Actions (ubuntu-latest): R-devel, R-release, R-oldrel-1
* GitHub Actions (windows-latest): R-release
* GitHub Actions (macOS-latest): R-release
* win-builder: R-devel, R-release
* macOS: R-release

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Resubmission note
* This is a resubmission following feedback from 2025-08-18.
* Replaced relative links with absolute URLs to resolve "invalid file URIs".

## Release summary (0.3.0)
* Breaking change: `bid_structure()` removes `layout`. The function now chooses a layout automatically based on inputs. The NEWS file documents migration tips and an example to update existing code.
* Docs: README reorganized; telemetry vignette polished; absolute links used throughout.
* Robustness: minor internal refactors to reduce duplication and standardize helpers.
* Version increased at submission.

## Compatibility
* New dependencies: none beyond those introduced in 0.2.0 (DBI, RSQLite; importFrom(stats, complete.cases)).
* Reverse dependencies: none known.
