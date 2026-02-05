# Suggest alternative analytics solutions for static dashboards

Provides recommendations for analytics and telemetry solutions suitable
for static Quarto dashboards, where Shiny-based telemetry
(shiny.telemetry or OpenTelemetry) is not available. This function helps
you choose the right analytics tool based on your needs and constraints.

**Important**: shiny.telemetry and Shiny OpenTelemetry only work with
`server: shiny` in Quarto YAML. For static Quarto dashboards (including
OJS-based dashboards), you need alternative web analytics solutions.

## Usage

``` r
bid_suggest_analytics(
  dashboard_type = c("static", "ojs", "python"),
  privacy_preference = c("gdpr_compliant", "privacy_focused", "standard"),
  budget = c("flexible", "free", "low"),
  self_hosted = FALSE
)
```

## Arguments

- dashboard_type:

  Character string specifying the type of dashboard:

  static

  :   Static HTML Quarto dashboard (default)

  ojs

  :   Quarto dashboard using Observable JS

  python

  :   Static dashboard with Python/Jupyter

- privacy_preference:

  Character string indicating privacy requirements:

  gdpr_compliant

  :   Prioritize GDPR-compliant solutions (default)

  privacy_focused

  :   Emphasize user privacy and no tracking

  standard

  :   Standard analytics with typical tracking

- budget:

  Character string indicating budget constraints:

  free

  :   Only free/open-source solutions

  low

  :   Low-cost solutions (\< \$10/month)

  flexible

  :   Any cost tier (default)

- self_hosted:

  Logical indicating whether self-hosted solutions are preferred
  (default: FALSE)

## Value

A data frame with recommended analytics solutions, including:

- solution:

  Name of the analytics platform

- type:

  Type of solution (privacy-focused, traditional, open-source)

- cost:

  Cost tier (free, paid, freemium)

- self_hosted:

  Whether self-hosting is available

- gdpr_compliant:

  Whether the solution is GDPR compliant

- integration_method:

  How to integrate (script tag, API, etc.)

- key_features:

  Main features for UX analysis

- bidux_compatibility:

  How well it works with BID framework

- docs_url:

  Link to integration documentation

## Integration Patterns

**For Static Quarto Dashboards:**

1.  **Event Tracking** - Track user interactions with custom events:

    - Button clicks, filter changes, tab switches

    - Use JavaScript event listeners in Quarto

    - Send events to analytics platform via API

2.  **Session Analysis** - Monitor user sessions:

    - Page views, time on page, bounce rate

    - User flow through dashboard sections

    - Identify drop-off points

3.  **Custom Dimensions** - Track dashboard-specific metrics:

    - Selected filters, date ranges, visualization types

    - User cohorts, roles, or departments

    - Dashboard version or configuration

**Example Integration (Plausible Analytics):**

Add to your Quarto dashboard header:

    <script defer data-domain="yourdomain.com"
      src="https://plausible.io/js/script.tagged-events.js"></script>

Track custom events in your dashboard JavaScript:

    // Track filter change
    document.getElementById('regionFilter').addEventListener('change', function(e) {
      plausible('Filter Changed', {props: {filter: 'region', value: e.target.value}});
    });

    // Track visualization interaction
    plotElement.on('plotly_click', function(data) {
      plausible('Chart Interaction', {props: {chart: 'sales_plot', action: 'click'}});
    });

**Analyzing Results with BID Framework:**

While these analytics tools won't automatically integrate with
[`bid_ingest_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_ingest_telemetry.md),
you can still apply BID framework principles:

1.  **Notice** - Export analytics data, identify friction points
    manually

2.  **Interpret** - Use
    [`bid_interpret()`](https://jrwinget.github.io/bidux/reference/bid_interpret.md)
    with insights from analytics

3.  **Anticipate** - Apply
    [`bid_anticipate()`](https://jrwinget.github.io/bidux/reference/bid_anticipate.md)
    to plan improvements

4.  **Structure** - Design improvements with
    [`bid_structure()`](https://jrwinget.github.io/bidux/reference/bid_structure.md)

5.  **Validate** - Measure impact with before/after analytics comparison

## Examples

``` r
# Get recommendations for static Quarto dashboard with GDPR compliance
suggestions <- bid_suggest_analytics(
  dashboard_type = "static",
  privacy_preference = "gdpr_compliant"
)
#> ℹ Found 7 analytics solutions matching your criteria for static Quarto dashboards
#> ✔ Top recommendation: Plausible Analytics (privacy-focused, paid)
#> ℹ See documentation for integration: https://plausible.io/docs
print(suggestions)
#>                  solution              type     cost self_hosted gdpr_compliant
#> 1     Plausible Analytics   privacy-focused     paid        TRUE           TRUE
#> 2        Fathom Analytics   privacy-focused     paid       FALSE           TRUE
#> 3        Simple Analytics   privacy-focused     paid       FALSE           TRUE
#> 4 Matomo (formerly Piwik)       open-source     free        TRUE           TRUE
#> 5                   Umami       open-source     free        TRUE           TRUE
#> 6                 PostHog product-analytics freemium        TRUE           TRUE
#> 7          Heap Analytics product-analytics freemium       FALSE           TRUE
#>                                 integration_method
#> 1                Script tag with custom events API
#> 2                   Script tag with event tracking
#> 3                       Script tag with events API
#> 4         JavaScript tracking code with events API
#> 5                   Script tag with event tracking
#> 6 JavaScript SDK with comprehensive event tracking
#> 7              JavaScript snippet with autocapture
#>                                                                                                        key_features
#> 1    Cookieless tracking; Custom event tracking; Goal conversions; Real-time dashboard; No personal data collection
#> 2                     Privacy-first tracking; Event tracking; Uptime monitoring; Email reports; GDPR/CCPA compliant
#> 3                                      Privacy-friendly; Automated events; Custom events; Bot detection; API access
#> 4 Full data ownership; Event tracking; Custom dimensions; Heatmaps (plugin); Session recording (plugin); API access
#> 5                         Lightweight and fast; Custom events; Real-time data; No cookies needed; Easy self-hosting
#> 6                      Product analytics; Session recording; Feature flags; Funnel analysis; Heatmaps; User cohorts
#> 7                 Automatic event capture; Retroactive analysis; Session replay; Funnel analysis; User segmentation
#>   bidux_compatibility                          docs_url
#> 1              manual         https://plausible.io/docs
#> 2              manual        https://usefathom.com/docs
#> 3              manual https://docs.simpleanalytics.com/
#> 4                good          https://matomo.org/docs/
#> 5              manual             https://umami.is/docs
#> 6                good          https://posthog.com/docs
#> 7                good   https://developers.heap.io/docs

# Find free, privacy-focused solutions for OJS dashboard
privacy_options <- bid_suggest_analytics(
  dashboard_type = "ojs",
  privacy_preference = "privacy_focused",
  budget = "free"
)
#> Warning: No analytics solutions match your criteria
#> ℹ Try relaxing constraints (e.g., budget or self_hosted)
#> ℹ Use bid_suggest_analytics() with default parameters to see all options

# Get self-hosted options
self_hosted <- bid_suggest_analytics(
  dashboard_type = "static",
  self_hosted = TRUE
)
#> ℹ Found 4 analytics solutions matching your criteria for static Quarto dashboards
#> ✔ Top recommendation: Plausible Analytics (privacy-focused, paid)
#> ℹ See documentation for integration: https://plausible.io/docs

# View top recommendation
top_choice <- suggestions[1, ]
cat(sprintf("Recommended: %s\n", top_choice$solution))
#> Recommended: Plausible Analytics
cat(sprintf("Integration: %s\n", top_choice$integration_method))
#> Integration: Script tag with custom events API
cat(sprintf("Docs: %s\n", top_choice$docs_url))
#> Docs: https://plausible.io/docs
```
