# Calculate Protein Oxidation from Urea

Estimate protein oxidation rate from total urea nitrogen loss. Uses the
conversion factor of 2.915 (grams protein per gram urea).

## Usage

``` r
calc_protein_oxidation(
  urea_loss,
  exercise_duration = 120,
  conversion_factor = 2.915,
  id_col = "id"
)
```

## Arguments

- urea_loss:

  A data frame from calc_urea_loss (or with urea_mass_loss column)

- exercise_duration:

  Exercise duration in minutes (default: 120)

- conversion_factor:

  Protein/urea conversion factor (default: 2.915)

- id_col:

  Name of the ID column

## Value

A tibble with protein oxidation rates (g/min)

## Details

The conversion factor of 2.915 is derived from the work of Telmosse
(2022), which states that 1 gram of urea excreted is equivalent to 0.343
grams of protein oxidized. This value is based on the protein formula
(`C89H148.5O32N23.5`) described in Institute of Medicine (2005).

- Nitrogen content of proteins (~16%)

- Nitrogen content of urea (46.6%)

- Complete oxidation of amino acids

protein_ox = urea_mass \* 2.915 / exercise_duration
