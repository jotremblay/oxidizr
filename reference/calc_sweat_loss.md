# Calculate Sweat Loss

Estimate sweat loss from mass balance considering fluid intake and
respiratory gas/water losses.

## Usage

``` r
calc_sweat_loss(subjects, calo, environment, exercise_duration = 120)
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

## Value

A tibble with sweat loss estimates (kg)
