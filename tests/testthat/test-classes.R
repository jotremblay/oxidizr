# Tests for S7 classes

test_that("CalorimetryData can be created", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    time = c(30, 60, 30, 60),
    vo2 = c(2.5, 2.6, 2.4, 2.5),
    vco2 = c(2.3, 2.4, 2.2, 2.3)
  )

  calo <- CalorimetryData(data = df)

  expect_s3_class(calo, "oxidizr::CalorimetryData")
  expect_equal(nrow(calo@data), 4)
  expect_equal(calo@vo2_unit, "L/min")
})

test_that("CalorimetryData validates required columns", {
  df <- data.frame(
    id = c(1, 2),
    time = c(30, 60)
    # Missing vo2 and vco2
  )

  expect_error(CalorimetryData(data = df), "Missing required columns")
})

test_that("CalorimetryData validates non-negative values", {
  df <- data.frame(
    id = c(1, 2),
    time = c(30, 60),
    vo2 = c(2.5, -1.0),  # Negative value
    vco2 = c(2.3, 2.4)
  )

  expect_error(CalorimetryData(data = df), "non-negative")
})

test_that("IsotopeData can be created", {
  rexp_df <- data.frame(
    id = 1:3,
    time = c(30, 60, 90),
    rexp = c(-25, -20, -15)
  )
  rexo_df <- data.frame(
    protocol = c("A", "B"),
    rexo = c(75, 80)
  )
  rref_df <- data.frame(
    time = c(30, 60, 90),
    rref = c(-26, -26, -26)
  )

  iso <- IsotopeData(rexp = rexp_df, rexo = rexo_df, rref = rref_df)

  expect_s3_class(iso, "oxidizr::IsotopeData")
})

test_that("OxidationStudy can be created", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    time = c(30, 60, 30, 60),
    vo2 = c(2.5, 2.6, 2.4, 2.5),
    vco2 = c(2.3, 2.4, 2.2, 2.3)
  )

  calo <- CalorimetryData(data = df)
  study <- OxidationStudy(calorimetry = calo)

  expect_s3_class(study, "oxidizr::OxidationStudy")
  # isotopes might be NULL or empty IsotopeData depending on S7 version/behavior
  if (!is.null(study@isotopes)) {
    expect_s3_class(study@isotopes, "oxidizr::IsotopeData")
  }
})

test_that("OxidationResults can be created", {
  rates_df <- data.frame(
    id = 1:4,
    time = c(30, 60, 30, 60),
    cho_total = c(2.0, 2.1, 1.9, 2.0),
    fat_total = c(0.5, 0.4, 0.6, 0.5)
  )

  results <- OxidationResults(oxidation_rates = rates_df)

  expect_s3_class(results, "oxidizr::OxidationResults")
  # energy_contributions might be NULL or empty data.frame
  if (!is.null(results@energy_contributions)) {
    expect_s3_class(results@energy_contributions, "data.frame")
  }
})
