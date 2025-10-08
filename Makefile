# Makefile for bidux R package development
# Provides convenient targets for formatting, linting, and testing

.PHONY: help format format-check lint test check-all install clean

# Default target: show help
help:
	@echo "╔════════════════════════════════════════════════════════════════╗"
	@echo "║  bidux R Package - Development Makefile                       ║"
	@echo "╚════════════════════════════════════════════════════════════════╝"
	@echo ""
	@echo "Available targets:"
	@echo "  make format        - Auto-format code with air and styler"
	@echo "  make format-check  - Check formatting without changes (CI mode)"
	@echo "  make lint          - Run lintr code quality checks"
	@echo "  make test          - Run all package tests"
	@echo "  make check-all     - Run all CI checks (format + lint + test)"
	@echo "  make install       - Install package with devtools"
	@echo "  make clean         - Clean build artifacts"
	@echo ""

# Format code with air (fast) and styler (complementary)
format:
	@echo "🔧 Formatting with air..."
	@air format .
	@echo "✓ air formatting complete"
	@echo ""
	@echo "🔧 Formatting with styler..."
	@Rscript -e 'styler::style_pkg()'
	@echo "✓ styler formatting complete"

# Check formatting (CI mode - no changes)
format-check:
	@echo "📋 Checking formatting with air..."
	@air format . --check
	@echo "✓ air formatting check passed"
	@echo ""
	@echo "📋 Checking formatting with styler..."
	@Rscript -e 'results <- styler::style_pkg(dry = "on"); \
	  if (length(results$$changed) > 0) { \
	    cat("\n❌ Styler found formatting issues in:\n"); \
	    for (file in results$$changed) cat("  -", file, "\n"); \
	    stop("Run make format to fix."); \
	  } else { \
	    cat("✓ styler formatting check passed\n"); \
	  }'

# Run lintr code quality checks
lint:
	@echo "📋 Running lintr code quality checks..."
	@Rscript -e 'lints <- lintr::lint_package(); \
	  print(lints); \
	  if (length(lints) > 0) { \
	    cat("\n❌ Found", length(lints), "linting issues\n"); \
	    quit(status = 1); \
	  } else { \
	    cat("\n✓ All lintr checks passed\n"); \
	  }'

# Run package tests
test:
	@echo "🧪 Running package tests..."
	@Rscript -e 'devtools::test()'

# Run all CI checks locally
check-all: format-check lint test
	@echo ""
	@echo "╔════════════════════════════════════════════╗"
	@echo "║  ✓ All checks passed                      ║"
	@echo "╚════════════════════════════════════════════╝"

# Install package
install:
	@echo "📦 Installing bidux package..."
	@Rscript -e 'devtools::install()'
	@echo "✓ Installation complete"

# Clean build artifacts
clean:
	@echo "🧹 Cleaning build artifacts..."
	@rm -rf src/*.o src/*.so src/*.dll
	@rm -rf *.tar.gz
	@rm -rf .Rcheck
	@rm -rf man/*.Rd~
	@echo "✓ Clean complete"
