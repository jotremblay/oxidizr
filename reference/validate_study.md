# Validate Oxidation Study

Comprehensive validation of an OxidationStudy object including all data
components and cross-dataset consistency.

## Usage

``` r
validate_study(
  study,
  strict = FALSE,
  thresholds = NULL,
  checks = NULL,
  verbose = TRUE
)
```

## Arguments

- study:

  An OxidationStudy object

- strict:

  Logical; if TRUE, errors halt execution; if FALSE, issues are warnings

- thresholds:

  Named list of custom thresholds for each component (NULL = use
  defaults)

- checks:

  Character vector of checks to run (NULL = all available)

- verbose:

  Logical; print progress messages

## Value

A ValidationResult S7 object

## Examples

``` r
if (FALSE) { # \dontrun{
study <- oxidation_study(calorimetry = calo_data, isotopes = iso_data)
validation <- validate_study(study)
print(validation)

# With strict mode
validation <- validate_study(study, strict = TRUE)
} # }
```
