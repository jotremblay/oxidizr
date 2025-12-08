# Calculate Total Energy Expenditure

Calculate total energy expenditure from VO2 using the Weir equation.

## Usage

``` r
calc_total_energy(vo2, vco2, method = c("weir", "brockway"))
```

## Arguments

- vo2:

  Oxygen consumption (L/min)

- vco2:

  Carbon dioxide production (L/min)

- method:

  Calculation method: "weir" or "brockway" (default: "weir")

## Value

Energy expenditure (kcal/min)

## Details

Weir equation (1949): EE (kcal/min) = 3.941 \* VO2 + 1.106 \* VCO2

Brockway equation (1987): EE (kcal/min) = 3.869 \* VO2 + 1.195 \* VCO2
