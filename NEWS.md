# oxidizr (development version)

## Default constants and net rates

* **Results change.** The default constants now follow the review of indirect
  calorimetry and 13C tracing that oxidizr accompanies:
  - fat: dioleoyl-palmitoyl-glycerol C55H102O6 (Livesey & Elia, 1988,
    Table 4; Elia & Livesey, 1992, Table 1), RQ 0.7097. CHO = 4.6143 VCO2 -
    3.2746 VO2 and fat = 1.7041 (VO2 - VCO2), against 4.6211, 3.2815 and
    1.7137 with the TAG-type, which remains available (`lipid = "tag_type"`).
  - protein: Kleiber's (1961) standard protein without sulfur,
    C100H159N26O32, oxidized to urea: 1.0443 L O2/g, 0.8715 L CO2/g (RQ
    0.835) and 2.8660 g protein per g urea, against 1.0103, 0.8436 and 2.9154
    with the general values of Livesey & Elia (1988), which remain available
    (`protein = "livesey_elia"`).
  - The molar volume (22.414 L/mol) and energy values (IOM, 2005) are
    unchanged.
* New options `oxidation_constants(lipid = "tripalmitin")` (Kleiber, 1961,
  Table 5.6) and `protein = "kleiber"`.
* **Results change.** Oxidation rates are no longer set to 0 when negative.
  `calc_substrate_oxidation()`, `calc_cho_partition()`, `calc_exogenous_cho()`
  and the targets helpers return net rates: a negative fat oxidation above a
  nonprotein RER of 1 estimates net lipogenesis, and truncation at 0 biased
  group means upward.

## Bug fixes

* `calc_exogenous_cho()` and the targets helpers turned missing enrichment
  values into 0 (`pmax(x, 0, na.rm = TRUE)`); they now stay `NA`.

## Constants, uncertainty and summaries

* `oxidation_constants()` builds the constant set from documented options for
  the lipid (TAG-type, dioleoyl-palmitoyl-glycerol, palmitoyl-stearoyl-oleoyl-
  glycerol, mean fatty acid), the molar volume (22.414, 22.4, real gases), the
  protein factors and the energy per gram. Each option names its source.
  `analyze_oxidation(constants = )` passes the set to every step.
* `analyze_oxidation()` propagates measurement uncertainty
  (`oxidation_uncertainty()`): first order (GUM) by default, Monte Carlo as an
  option. Results gain `<var>_u` and its systematic part `<var>_u_sys`, and the
  new `uncertainty` slot holds each contribution. By default only what the data
  show is propagated: replicate SDs with the VO2-VCO2 replicate correlation
  (`aggregate = TRUE`), and the control-group SD of Rref (random only for
  subjects who did the control trial; random plus a systematic group-mean part
  for subjects who did not).
* `uncertainty_budget()` ranks the sources of uncertainty of one variable;
  `compare_constants()` shows the results under other constant sets.
* `calc_rref(by_subject = TRUE)` and `analyze_oxidation(rref_by_subject = TRUE)`
  take Rref from each subject's own control trial (crossover studies). A
  subject with no own control value gets an NA exogenous CHO, with a warning.
* `calc_rref()`'s `n` now counts non-missing values.
* `summary()` of OxidationResults uses the subject as the unit, and includes
  `protein_ox`.
* The report template states the constants that the analysis used.
* `aggregate_calorimetry()` keeps the SD and number of replicates.
* `summarize_oxidation()` gives mean, SD, SE and t-based CI with the subject as
  the unit. `summarize_energy()`, `tbl_oxidation_summary()`,
  `tbl_energy_contribution()`, the time-course plots and `quick_summary()` use
  it; the tables and plots take `error = "sd"`, `"se"` or `"ci"` and `level`.
  **Results change:** the tables and `quick_summary()` used to pool every
  subject x time row, so their n and SD were per row.
* New vignette, `vignette("uncertainty")`.

## Bug fixes (crossover designs)

* `calc_exogenous_cho()`, `calc_plasma_cho()` and `calc_cho_partition()` join
  the isotope data by subject, time and protocol. Before, with one subject in
  several protocols, rows were duplicated across protocols.
* The 13C steps now stop with a clear message when the calorimetry data have
  several rows for one subject, protocol and time (use `aggregate = TRUE`).

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
