# oxidizr (development version)

## Changes to results

* `stoich_coefficients` is now computed from balanced reactions when the
  package is built, following Telmosse (2022): glucose and the TAG-type
  C53.4H99.2O6 at 22.414 L/mol (Livesey & Elia, 1988). CHO = 4.6211 VCO2 -
  3.2815 VO2 and fat = 1.7137 (VO2 - VCO2), against 4.618, 3.279 and 1.712
  before. Fat oxidation increases by 0.1%. CHO oxidation changes by less than
  0.1% above an RER of 0.75; the relative change is larger when CHO
  oxidation is close to zero (-0.5% at RER 0.72).
* Protein gas exchange is computed from the general values of Livesey & Elia
  (1988, Table 6): 1.0103 L O2/g and 0.8436 L CO2/g (was 1.0075 and 0.8443).
  The urea factor is 2.9154 g protein per g urea (was 2.915), and urea mmol
  are converted to grams with 60.056 g/mol (was 60).
* The CO2 yield of glucose in the 13C equations (`co2_cho_ratio`) is 0.7465
  L/g, from the same glucose reaction (was 0.747).
* `calc_sweat_loss()` now removes the urine passed between the two weighings,
  and the default of `baseline_water_ml` is 0 mL (was 2000 mL).
* `tar_oxidation_analysis()` uses the same mass balance as `calc_sweat_loss()`
  and the urea factor 2.9154 (was 2.9). Its new `baseline_water_ml` argument
  keeps the 2000 mL of the study protocol.

## Bug fixes

* `calc_substrate_oxidation()` joins protein oxidation by subject and
  protocol. Before, with one protein value per subject and protocol, each
  calorimetry row was duplicated and received the values of every protocol.
  It now stops if a key matches more than one protein row.
* `calc_sweat_loss()`, `calc_urea_loss()` and `calc_protein_oxidation()` keep
  the protocol column, so each protocol uses its own gas exchange and urine.
* `analyze_oxidation()` gains `exercise_duration` and passes it to the sweat
  and protein steps, which used 120 min whatever the study.
* `calc_urea_loss()` reads the urine column named in
  `subjects@fluid_cols$urine` instead of a fixed `urine` column, joins sweat
  loss on the subject ID even when SubjectData and UreaData name it
  differently, and no longer fails when both tables hold a `urine_vol` column.
* When the urine volume is only in the UreaData, `analyze_oxidation()` now
  uses it in the sweat mass balance too, so the urine is not counted as
  sweat.
* The protocol column keeps its type (for example, factor levels) through
  the new joins.

## New features

* `gas_yield()` returns the L O2 used and L CO2 produced per gram of any
  CcHhOo substrate, for any molar volume.
* New vignette, `vignette("stoichiometry")`, with the reactions, constants and
  the reasoning and sources of Telmosse (2022) for each choice.
* `energy_factors`, `stoich_coefficients` and the function defaults now come
  from one table of substrates, with the source of each value.

# oxidizr 0.1.0

* Initial release
* S7 classes for calorimetry data management:

  - `CalorimetryData` for VO2, VCO2, RER measurements
  - `IsotopeData` for 13C enrichment data
  - `UreaData` for protein oxidation inputs
  - `EnvironmentData` for environmental conditions
  - `SubjectData` for subject characteristics
  - `OxidationStudy` as a container combining all data types
  - `OxidationResults` for analysis outputs

* Substrate oxidation calculations (CHO, FAT, Protein) using stoichiometric equations
* 13C isotope tracer analysis for exogenous CHO oxidation
* Publication-ready tables via gt and Excel export
* ggplot2-based visualization functions
* Quarto report template for automated analysis reports
* Targets pipeline integration helpers
