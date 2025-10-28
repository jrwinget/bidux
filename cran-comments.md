## Test environments

-   local: R 4.5.1 on Ubuntu 24.04
-   GitHub Actions (ubuntu-latest): R-devel, R-release, R-oldrel-1
-   GitHub Actions (windows-latest): R-release
-   GitHub Actions (macOS-latest): R-release
-   win-builder: R-devel, R-release
-   macOS: R-release

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Examples and vignettes run quickly; no long-running examples are included.

## Release summary (0.3.2)

This is a maintenance release following 0.3.1, focused on improving API ergonomics and reducing package footprint. Key improvements include a flattened data_story API for better user experience, telemetry sensitivity presets for easier configuration, and removal of unused dependencies. Highlights (see NEWS.md for full details):

-   **New features**
    -   Flattened `new_data_story()` API with `hook`, `context`, `tension`, `resolution` arguments (more intuitive than nested format)
    -   `bid_telemetry_presets()` for pre-configured sensitivity levels (strict/moderate/relaxed)
-   **Improvements**
    -   Removed unused dependencies (purrr, stringr); smaller install size and less dependency bloat
    -   Extracted telemetry magic numbers to named constants for better maintainability
    -   Improved documentation showing recommended data.frame API for personas
    -   Full backward compatibility with deprecation warnings for legacy formats
-   **Bug fixes**
    -   Removed duplicate utility functions that were defined in multiple files

This release maintains 100% backward compatibility while providing clearer, more pragmatic APIs for new users.

## Rationale for version

`0.3.2` is a maintenance release following 0.3.1, focused on API improvements and package optimization. It simplifies the data_story API based on user feedback, adds telemetry configuration presets, and reduces package size by removing unused dependencies. All changes maintain 100% backward compatibility with clear deprecation paths.

## Compatibility

-   Two dependencies removed (purrr, stringr) - package now lighter and faster
-   No new dependencies added
-   Reverse dependencies: none on CRAN

## Additional policy notes

-   All URLs use HTTPS and were verified at build time.
-   No non-ASCII or unusual encodings in Rd files.
-   No references to external resources are required at runtime.
