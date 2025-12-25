# Calculate Sweat Loss

Estimate sweat loss from mass balance considering fluid intake and
respiratory gas/water losses.

## Usage

``` r
calc_sweat_loss(
  subjects,
  calo,
  environment,
  exercise_duration = 120,
  baseline_water_ml = 2000,
  ve_vo2_ratio = 25
)
```

## Arguments

- subjects:

  A SubjectData object or data frame with mass/fluid data

- calo:

  A CalorimetryData object for respiratory calculations

- environment:

  An EnvironmentData object for vapor pressure calculations

- exercise_duration:

  Exercise duration in minutes (default: 120)

- baseline_water_ml:

  Baseline water intake in mL added to fluid balance calculation
  (default: 2000). Set to 0 if baseline water is already included in the
  water_ingested column of SubjectData.

- ve_vo2_ratio:

  Ventilation to VO2 ratio used to estimate minute ventilation when VE
  data is not available (default: 25). Typical range during moderate
  exercise is 20-30.

## Value

A tibble with sweat loss estimates (kg)
