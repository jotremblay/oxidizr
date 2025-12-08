# Validate Isotope Data

Check 13C isotope enrichment data against physiological ranges and for
data quality issues.

## Usage

``` r
validate_isotopes(
  isotopes,
  thresholds = NULL,
  check_enrichment_order = TRUE,
  check_missing = TRUE
)
```

## Arguments

- isotopes:

  An IsotopeData object

- thresholds:

  Custom thresholds (NULL = use isotope_thresholds)

- check_enrichment_order:

  Logical; verify Rexp is between Rref and Rexo

- check_missing:

  Logical; check for missing data

## Value

A ValidationResult S7 object
