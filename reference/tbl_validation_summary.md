# Validation Summary Table

Create a formatted summary table of validation results.

## Usage

``` r
tbl_validation_summary(
  validation,
  group_by = c("category", "severity", "variable"),
  show_details = FALSE,
  max_rows = 20
)
```

## Arguments

- validation:

  A ValidationResult object from validate_study() or component
  validators.

- group_by:

  How to group issues: "category" (default), "severity", or "variable".

- show_details:

  Logical, show detailed message column (default: FALSE).

- max_rows:

  Maximum rows to display (default: 20).

## Value

A gt table object

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- validate_study(study)
tbl_validation_summary(validation)
tbl_validation_summary(validation, group_by = "severity")
} # }
```
