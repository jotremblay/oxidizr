# Data Completeness Table

Create a table showing data completeness by subject or variable.

## Usage

``` r
tbl_data_completeness(
  data,
  by = c("variable", "subject"),
  variables = NULL,
  id_col = "id",
  threshold = 95,
  decimals = 1
)
```

## Arguments

- data:

  A data frame, CalorimetryData, or OxidationStudy object.

- by:

  How to organize the table: "variable" (default) or "subject".

- variables:

  Character vector of variables to include (default: auto-detect).

- id_col:

  Subject ID column name (default: "id").

- threshold:

  Threshold percentage for highlighting incomplete data (default: 95).

- decimals:

  Number of decimal places (default: 1).

## Value

A gt table object

## Examples

``` r
if (FALSE) { # \dontrun{
tbl_data_completeness(calo_data)
tbl_data_completeness(calo_data, by = "subject")
} # }
```
