# Render Quality Report

Generate a comprehensive data quality report from validation results.

## Usage

``` r
render_quality_report(
  study,
  validation = NULL,
  output_file = "quality_report.html",
  template = NULL,
  title = "Data Quality Report",
  author = Sys.info()["user"],
  include_steady_state = TRUE,
  include_recommendations = TRUE,
  include_raw_data = FALSE,
  validation_thresholds = NULL,
  open = TRUE
)
```

## Arguments

- study:

  An OxidationStudy object to validate and report on.

- validation:

  Optional pre-computed ValidationResult object. If NULL, validation
  will be performed automatically.

- output_file:

  Output file path (default: "quality_report.html").

- template:

  Path to custom Quarto template (NULL = default).

- title:

  Report title.

- author:

  Report author (default: system user).

- include_steady_state:

  Logical, include steady-state analysis (default: TRUE).

- include_recommendations:

  Logical, include recommendations section (default: TRUE).

- include_raw_data:

  Logical, include raw data tables in appendix (default: FALSE).

- validation_thresholds:

  Optional custom thresholds for validation.

- open:

  Logical, open report in browser after rendering (default: TRUE).

## Value

Path to rendered report (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic quality report
study <- oxidation_study(calorimetry = calo_data)
render_quality_report(study)

# With custom output file
render_quality_report(study, output_file = "my_quality_report.html")

# With pre-computed validation
validation <- validate_study(study)
render_quality_report(study, validation = validation)
} # }
```
