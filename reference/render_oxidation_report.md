# Render Oxidation Report

Generate a comprehensive Quarto report from oxidation results.

## Usage

``` r
render_oxidation_report(
  results,
  output_file = "oxidation_report.html",
  template = NULL,
  title = "Substrate Oxidation Analysis",
  author = Sys.info()["user"],
  include_stats = TRUE,
  include_tables = TRUE,
  include_methods = TRUE
)
```

## Arguments

- results:

  An OxidationResults object

- output_file:

  Output file path (e.g., "report.html")

- template:

  Path to custom Quarto template (NULL = default)

- title:

  Report title

- author:

  Report author

- include_stats:

  Include statistical analysis

- include_tables:

  Include summary tables

- include_methods:

  Include methods section

## Value

Path to rendered report
