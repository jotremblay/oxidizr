# Calculate Substrate Oxidation

Calculate carbohydrate and fat oxidation rates from indirect calorimetry
using stoichiometric equations.

## Usage

``` r
calc_substrate_oxidation(
  calo,
  protein_ox = NULL,
  coefficients = stoich_coefficients
)
```

## Arguments

- calo:

  A CalorimetryData object

- protein_ox:

  Optional protein oxidation data (from calc_protein_oxidation)

- coefficients:

  Stoichiometric coefficients (default: stoich_coefficients)

## Value

A tibble with CHO and fat oxidation rates (g/min)

## Details

Uses equations commonly attributed to Jequier et al. and Frayn, with
specific coefficients parameterized as presented in Telmosse (2022):

Non-protein calculations:

- VO2np = VO2 - (protein_ox \* 1.0075)

- VCO2np = VCO2 - (protein_ox \* 0.8443)

Substrate oxidation:

- CHO (g/min) = 4.618 \* VCO2np - 3.279 \* VO2np

- Fat (g/min) = -1.712 \* VCO2np + 1.712 \* VO2np

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic calculation (assuming negligible protein contribution)
ox <- calc_substrate_oxidation(calo_data)

# With protein correction
protein <- calc_protein_oxidation(urea_loss)
ox <- calc_substrate_oxidation(calo_data, protein_ox = protein)
} # }
```
