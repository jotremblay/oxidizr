# Tests for isotope tracer functions

# -----------------------------------------------------------------------------
# Test Data Setup
# -----------------------------------------------------------------------------

create_test_isotope_data <- function() {
  rexp_df <- data.frame(
    id = rep(1:3, each = 4),
    time = rep(c(30, 60, 90, 120), 3),
    protocol = rep(c("CHO", "CHO", "PLA"), each = 4),
    rexp = c(
      -22, -18, -14, -10,   # Subject 1 (CHO) - enrichment increases over time
      -23, -19, -15, -11,   # Subject 2 (CHO)
      -26, -26, -26, -26    # Subject 3 (PLA) - baseline enrichment
    )
  )

  rexo_df <- data.frame(
    protocol = c("CHO", "PLA"),
    rexo = c(100, -26)  # CHO substrate enriched, PLA is natural
  )

  rref_df <- data.frame(
    time = c(30, 60, 90, 120),
    rref = rep(-26, 4)  # Baseline enrichment
  )

  rpla_df <- data.frame(
    id = rep(1:2, each = 4),
    time = rep(c(30, 60, 90, 120), 2),
    protocol = rep("CHO", 8),
    rpla = c(
      -20, -15, -10, -5,  # Subject 1 plasma enrichment
      -21, -16, -11, -6   # Subject 2 plasma enrichment
    )
  )

  IsotopeData(
    rexp = rexp_df,
    rexo = rexo_df,
    rref = rref_df,
    rpla = rpla_df,
    id_col = "id",
    time_col = "time",
    protocol_col = "protocol",
    rexp_col = "rexp",
    rexo_col = "rexo",
    rpla_col = "rpla"
  )
}

create_test_calorimetry_for_iso <- function() {
  df <- data.frame(
    id = rep(1:3, each = 4),
    time = rep(c(30, 60, 90, 120), 3),
    protocol = rep(c("CHO", "CHO", "PLA"), each = 4),
    vo2 = c(
      2.5, 2.6, 2.5, 2.4,
      2.4, 2.5, 2.4, 2.3,
      2.5, 2.5, 2.5, 2.4
    ),
    vco2 = c(
      2.3, 2.4, 2.3, 2.2,
      2.2, 2.3, 2.2, 2.1,
      2.3, 2.3, 2.3, 2.2
    )
  )

  CalorimetryData(
    data = df,
    protocol_col = "protocol"
  )
}

# -----------------------------------------------------------------------------
# calc_rref Tests
# -----------------------------------------------------------------------------

test_that("calc_rref returns correct structure", {
  iso <- create_test_isotope_data()
  rref <- calc_rref(iso, control_protocol = "PLA")

  expect_s3_class(rref, "tbl_df")
  expect_true("rref" %in% names(rref))
  expect_true("time" %in% names(rref))
})

test_that("calc_rref calculates baseline enrichment correctly", {
  iso <- create_test_isotope_data()
  rref <- calc_rref(iso, control_protocol = "PLA")

  # PLA subject has constant -26 enrichment

  expect_equal(unique(rref$rref), -26)
})

test_that("calc_rref works with by_time = FALSE", {
  iso <- create_test_isotope_data()
  rref <- calc_rref(iso, control_protocol = "PLA", by_time = FALSE)

  # Should return single row
  expect_equal(nrow(rref), 1)
  expect_equal(rref$rref, -26)
})

test_that("calc_rref errors on non-existent protocol", {
  iso <- create_test_isotope_data()

  expect_error(
    calc_rref(iso, control_protocol = "NONEXISTENT"),
    "No data found"
  )
})

test_that("calc_rref validates IsotopeData input", {
  expect_error(
    calc_rref(data.frame(x = 1), control_protocol = "PLA"),
    "IsotopeData"
  )
})

# -----------------------------------------------------------------------------
# calc_exogenous_cho Tests
# -----------------------------------------------------------------------------

test_that("calc_exogenous_cho returns correct structure", {
  calo <- create_test_calorimetry_for_iso()
  iso <- create_test_isotope_data()
  rref <- calc_rref(iso, control_protocol = "PLA")

  cho_exo <- calc_exogenous_cho(calo, iso, rref)

  expect_s3_class(cho_exo, "tbl_df")
  expect_true("cho_exo" %in% names(cho_exo))
  expect_true("rexp" %in% names(cho_exo))
  expect_true("rexo" %in% names(cho_exo))
  expect_true("rref" %in% names(cho_exo))
})

test_that("calc_exogenous_cho produces non-negative values",
{
  calo <- create_test_calorimetry_for_iso()
  iso <- create_test_isotope_data()
  rref <- calc_rref(iso, control_protocol = "PLA")

  cho_exo <- calc_exogenous_cho(calo, iso, rref)

  # All cho_exo should be >= 0
  expect_true(all(cho_exo$cho_exo >= 0, na.rm = TRUE))
})

test_that("calc_exogenous_cho increases with enrichment", {
  calo <- create_test_calorimetry_for_iso()
  iso <- create_test_isotope_data()
  rref <- calc_rref(iso, control_protocol = "PLA")

  cho_exo <- calc_exogenous_cho(calo, iso, rref)

  # For CHO subjects, exogenous oxidation should increase over time
  # as Rexp becomes more enriched
  subj1 <- cho_exo |> dplyr::filter(id == 1)
  # Later time points should have higher exo oxidation
  expect_gt(subj1$cho_exo[4], subj1$cho_exo[1])
})

test_that("calc_exogenous_cho accepts numeric rref", {
  calo <- create_test_calorimetry_for_iso()
  iso <- create_test_isotope_data()

  cho_exo <- calc_exogenous_cho(calo, iso, rref = -26)

  expect_s3_class(cho_exo, "tbl_df")
  expect_true(all(cho_exo$rref == -26))
})

test_that("calc_exogenous_cho validates inputs", {
  calo <- create_test_calorimetry_for_iso()
  iso <- create_test_isotope_data()

  expect_error(
    calc_exogenous_cho(data.frame(x = 1), iso, -26),
    "CalorimetryData"
  )

  expect_error(
    calc_exogenous_cho(calo, data.frame(x = 1), -26),
    "IsotopeData"
  )
})

# -----------------------------------------------------------------------------
# calc_fexo Tests
# -----------------------------------------------------------------------------

test_that("calc_fexo returns correct structure", {
  iso <- create_test_isotope_data()
  rref <- -26

  fexo <- calc_fexo(iso, rref)

  expect_s3_class(fexo, "tbl_df")
  expect_true("fexo" %in% names(fexo))
})

test_that("calc_fexo calculates percentage correctly", {
  iso <- create_test_isotope_data()
  rref <- -26

  fexo <- calc_fexo(iso, rref)

  # Fexo should be between 0 and 100%
  expect_true(all(fexo$fexo >= 0 & fexo$fexo <= 100))
})

test_that("calc_fexo validates IsotopeData", {
  expect_error(
    calc_fexo(data.frame(x = 1), -26),
    "IsotopeData"
  )
})

test_that("calc_fexo errors when rpla is missing", {
  # Create IsotopeData without rpla
  rexp_df <- data.frame(
    id = 1:3,
    time = c(30, 60, 90),
    rexp = c(-25, -20, -15)
  )
  rexo_df <- data.frame(
    protocol = "CHO",
    rexo = 100
  )

  iso_no_rpla <- IsotopeData(
    rexp = rexp_df,
    rexo = rexo_df,
    rpla = NULL
  )

  expect_error(
    calc_fexo(iso_no_rpla, -26),
    "rpla.*required"
  )
})

# -----------------------------------------------------------------------------
# calc_plasma_cho Tests
# -----------------------------------------------------------------------------

test_that("calc_plasma_cho returns correct structure", {
  calo <- create_test_calorimetry_for_iso()
  iso <- create_test_isotope_data()
  rref <- -26

  # Filter to only subjects with rpla data
  calo_filtered <- CalorimetryData(
    data = calo@data |> dplyr::filter(id %in% c(1, 2)),
    protocol_col = "protocol"
  )

  cho_pla <- calc_plasma_cho(calo_filtered, iso, rref)

  expect_s3_class(cho_pla, "tbl_df")
  expect_true("cho_pla" %in% names(cho_pla))
})

test_that("calc_plasma_cho validates inputs", {
  calo <- create_test_calorimetry_for_iso()
  iso <- create_test_isotope_data()

  expect_error(
    calc_plasma_cho(data.frame(x = 1), iso, -26),
    "CalorimetryData"
  )

  expect_error(
    calc_plasma_cho(calo, data.frame(x = 1), -26),
    "IsotopeData"
  )
})

test_that("calc_plasma_cho errors when rpla is missing", {
  calo <- create_test_calorimetry_for_iso()

  rexp_df <- data.frame(
    id = 1:3,
    time = c(30, 60, 90),
    rexp = c(-25, -20, -15)
  )
  rexo_df <- data.frame(
    protocol = "CHO",
    rexo = 100
  )

  iso_no_rpla <- IsotopeData(
    rexp = rexp_df,
    rexo = rexo_df,
    rpla = NULL
  )

  expect_error(
    calc_plasma_cho(calo, iso_no_rpla, -26),
    "rpla.*required"
  )
})
