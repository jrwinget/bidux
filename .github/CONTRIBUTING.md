# Contributing to `{bidux}`

First off, thank you for your interest in making `{bidux}` better! Whether you’re filing a bug report, suggesting a new feature, improving documentation, or submitting code, your contributions are what make this project great.

---

## 📜 Code of Conduct

All contributors must abide by our [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). Please read it before participating.

---

## 🐛 Reporting Bugs

1. Search for existing issues to see if it’s already been reported.
2. If not, open a new issue using the **Bug Report** template:
   [https://github.com/jrwinget/bidux/issues/new?template=01\_bug-report.yml](https://github.com/jrwinget/bidux/issues/new?template=01_bug-report.yml)
3. Fill in all sections, including reproducible examples and session info.

---

## ✨ Suggesting Enhancements

1. Browse open feature requests to avoid duplicates.
2. Open a new issue with the **Feature Request** template:
   [https://github.com/jrwinget/bidux/issues/new?template=02\_feature-request.yml](https://github.com/jrwinget/bidux/issues/new?template=02_feature-request.yml)
3. Describe the use case, proposed API, and any relevant links or mockups.

---

## 📖 Documentation Improvements

We rely on clear examples and comprehensive docs. To propose improvements:

1. Create an issue with the **Documentation** template:
   [https://github.com/jrwinget/bidux/issues/new?template=03\_documentation-improvement.yml](https://github.com/jrwinget/bidux/issues/new?template=03_documentation-improvement.yml)
2. Submit a PR with updated `.Rmd`, vignettes, or help files.
3. Use `\donttest{}` for long-running examples, and ensure all code in examples runs without external dependencies.

---

## 🗂 Other Issue Templates

* **[♿ Accessibility Issue](https://github.com/jrwinget/bidux/issues/new?template=04_accessibility-issue.yml)** - Report or fix accessibility barriers
* **[🧠 Theory Suggestions](https://github.com/jrwinget/bidux/issues/new?template=05_theory-suggestion.yml)** - Propose new concepts or theories
* **[✨ Shiny Integration Requests](https://github.com/jrwinget/bidux/issues/new?template=06_shiny-integration-request.yml)** - Improve integration with Shiny packages

---

## 👩‍💻 Pull Request Workflow

1. Fork the `jrwinget/bidux` repository.
2. Create a branch named `feature/your-feature` or `bugfix/issue-123`.
3. Install dependencies:

   ```r
   pak::pak(desc::desc_get_deps()$package)
   ```
4. Load the package and run tests:

   ```r
   devtools::load_all()
   devtools::test()
   ```
5. Write clear, descriptive commit messages.
6. Document new functions with roxygen2 and run:

   ```r
   devtools::document()
   ```
7. Check the package as if for CRAN:

   ```r
   devtools::check()
   ```
8. Push your branch and open a PR against `main`.

**CI checks** will run automatically. Address any failures before requesting a review.

---

## 🖋 Code Style Guidelines

We follow the tidyverse style conventions:

* Use **snake\_case** for function and variable names.
* Avoid the magrittr pipe (`|>`); prefer the native pipe (`|>`).
* Adhere to the [tidyverse style guide](https://style.tidyverse.org/).
* Run `styler::style_pkg()` and `lintr::lint_package()` to catch formatting issues.

---

## 🧪 Testing

* Place tests in `tests/testthat/` using the **testthat** framework.
* Aim for coverage across edge cases and expected failure modes.
* Use `skip_on_cran()` or `skip_on_ci()` for tests that require external resources.

---

## 📚 Documentation & Vignettes

* Vignettes live in `vignettes/`.
* Use R Markdown with examples that illustrate real-world workflows.
* Link new features to existing vignettes or create a dedicated one.

---

## 🧠 BID Framework Considerations

When contributing features or examples, keep the following in mind:

* **Scientific Accuracy**: Represent concepts and theories correctly.
* **Accessibility**: Ensure components and outputs meet WCAG standards.
* **User Experience**: Strive for clear, intuitive interfaces.
* **Evidence-Based**: Cite reputable sources when introducing new theories.

---

## 💬 Community & Discussions

For general questions, brainstorming, or design discussions, use [GitHub Discussions](https://github.com/jrwinget/bidux/discussions).

---

## ⚖️ License

By contributing, you agree that your contributions will be licensed under the project’s MIT License.

---

**Thank you for helping `{bidux}` become stronger and more user-friendly!**
