# Plot Data Quality Overview

Creates a multi-panel dashboard showing validation results summary.

## Usage

``` r
plot_data_quality(
  validation,
  show_categories = TRUE,
  show_severity = TRUE,
  show_variables = TRUE,
  max_variables = 10
)
```

## Arguments

- validation:

  A ValidationResult object from validate_study() or component
  validators.

- show_categories:

  Logical, whether to show issues by category (default: TRUE).

- show_severity:

  Logical, whether to show issues by severity (default: TRUE).

- show_variables:

  Logical, whether to show most affected variables (default: TRUE).

- max_variables:

  Maximum number of variables to show (default: 10).

## Value

A patchwork-combined ggplot2 object

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- validate_study(study)
plot_data_quality(validation)
} # }
```
