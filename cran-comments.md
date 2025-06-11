## Test environments
* local: R 4.5.0 on Ubuntu 24.04
* GitHub Actions (ubuntu-latest): R-devel, R-release, R-oldrel-1
* GitHub Actions (windows-latest): R-release  
* GitHub Actions (macOS-latest): R-release
* win-builder: R-devel, R-release
* macOS: R-release

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Resubmission
This is a resubmission. In this version I have:

* Added single quotes around package names names in DESCRIPTION file (i.e., 'shiny') as requested
* Added \value tags to all exported methods (.Rd files) with detailed documentation explaining the structure of output (class) and what the output means
* Replaced \dontrun{} with if(interactive()){} in function examples for interactive functions
* Improved test coverage and fixes errors in bid_report and bid_validate, enhancing format handling and support for edge cases
* Simplified test dependencies by consolidating library calls

## Comments
This is a new package submission. The package provides the BID (Behavior Insight Design) framework for integrating behavioral science concepts into Shiny UI/UX design.
