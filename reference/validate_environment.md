# Validate Environment Data

Check environmental conditions against expected ranges.

## Usage

``` r
validate_environment(environment, thresholds = NULL)
```

## Arguments

- environment:

  An EnvironmentData object

- thresholds:

  Custom thresholds (NULL = use environment_thresholds)

## Value

A ValidationResult S7 object
