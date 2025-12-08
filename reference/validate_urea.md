# Validate Urea Data

Check urea concentration data against physiological ranges.

## Usage

``` r
validate_urea(urea, thresholds = NULL)
```

## Arguments

- urea:

  A UreaData object

- thresholds:

  Custom thresholds (NULL = use urea_thresholds)

## Value

A ValidationResult S7 object
