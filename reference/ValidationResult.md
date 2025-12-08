# Validation Result Class

S7 class for storing data validation findings from quality checks.

## Arguments

- issues:

  A tibble with validation issues found. Columns include: check_id,
  category, variable, severity, message, n_affected, pct_affected,
  subject_ids, time_points, values, threshold, reference.

- passed:

  Logical indicating if all critical checks passed (no errors)

- severity_summary:

  Named list with counts by severity level (error, warning, info)

- data_summary:

  Tibble with data completeness metrics by variable

- recommendations:

  Character vector of prioritized recommendations

- timestamp:

  POSIXct timestamp when validation was performed

## Value

A ValidationResult S7 object

## Examples

``` r
if (FALSE) { # \dontrun{
# Typically created by validate_study() or validate_calorimetry()
validation <- validate_study(study)
print(validation)
} # }
```
