# Outlier Summary Table

Create a detailed table of detected outliers from validation results.

## Usage

``` r
tbl_outliers(
  validation,
  max_rows = 50,
  show_values = TRUE,
  show_thresholds = TRUE
)
```

## Arguments

- validation:

  A ValidationResult object containing outlier information.

- max_rows:

  Maximum rows to display (default: 50).

- show_values:

  Logical, show actual outlier values (default: TRUE).

- show_thresholds:

  Logical, show threshold information (default: TRUE).

## Value

A gt table object

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- validate_calorimetry(calo_data)
tbl_outliers(validation)
} # }
```
