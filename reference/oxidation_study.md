# Create Oxidation Study Object

Convenience function to create an OxidationStudy object from component
data.

## Usage

``` r
oxidation_study(
  calorimetry,
  isotopes = NULL,
  urea = NULL,
  environment = NULL,
  subjects = NULL,
  protocols = NULL,
  ...
)
```

## Arguments

- calorimetry:

  A CalorimetryData object or data frame

- isotopes:

  An IsotopeData object (optional)

- urea:

  A UreaData object (optional)

- environment:

  An EnvironmentData object (optional)

- subjects:

  A SubjectData object or data frame (optional)

- protocols:

  Character vector or factor of protocol names (optional)

- ...:

  Additional arguments for CalorimetryData if data frame provided

## Value

An OxidationStudy S7 object
