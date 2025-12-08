# Calculate Energy Yield

Convert substrate oxidation rates (g/min) to energy yield (kcal/min).

## Usage

``` r
calc_energy_yield(
  oxidation,
  cho_factor = 3.72,
  fat_factor = 9.44,
  protein_factor = 4.7
)
```

## Arguments

- oxidation:

  A data frame with oxidation rates (from calc_substrate_oxidation)

- cho_factor:

  Energy per gram CHO (default: 3.72 kcal/g)

- fat_factor:

  Energy per gram fat (default: 9.44 kcal/g)

- protein_factor:

  Energy per gram protein (default: 4.70 kcal/g)

## Value

A tibble with energy yield columns added

## Details

Default energy factors:

- Carbohydrate: 3.72 kcal/g (average of glucose, glycogen)

- Fat: 9.44 kcal/g (average triglyceride)

- Protein: 4.70 kcal/g (average amino acid mixture)

These values can be customized for specific substrates.
