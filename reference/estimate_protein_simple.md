# Estimate Protein Oxidation (Simple Method)

Simple estimation of protein oxidation assuming a fixed contribution to
total energy expenditure (typically 5-15% during exercise).

## Usage

``` r
estimate_protein_simple(
  total_energy,
  protein_percent = 0.05,
  protein_factor = 4.7
)
```

## Arguments

- total_energy:

  Total energy expenditure (kcal/min or kcal)

- protein_percent:

  Assumed protein contribution (default: 0.05 = 5%)

- protein_factor:

  Energy per gram protein (default: 4.70 kcal/g)

## Value

Estimated protein oxidation (g/min or g)
