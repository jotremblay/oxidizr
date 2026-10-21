# Tests for summarize_oxidation() and the tables, plots and summaries that
# use it (subject as the unit of analysis)

create_summary_test_results <- function() {
  df <- data.frame(
    id = rep(1:4, each = 4),
    time = rep(c(30, 60, 90, 120), 4),
    protocol = rep(c("CHO", "CHO", "PLA", "PLA"), each = 4),
    vo2 = c(2.5, 2.6, 2.5, 2.4, 2.4, 2.5, 2.4, 2.3,
            2.5, 2.5, 2.5, 2.4, 2.4, 2.4, 2.4, 2.3),
    vco2 = c(2.3, 2.4, 2.3, 2.2, 2.2, 2.3, 2.2, 2.1,
             2.3, 2.3, 2.3, 2.2, 2.2, 2.2, 2.2, 2.1)
  )
  study <- OxidationStudy(
    calorimetry = CalorimetryData(data = df, protocol_col = "protocol"),
    isotopes = NULL,
    urea = NULL,
    environment = NULL,
    subjects = NULL
  )
  suppressMessages(analyze_oxidation(study, validate = FALSE))
}

has_errorbar <- function(p) {
  any(vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1)))
}

# -----------------------------------------------------------------------------
# summarize_oxidation()
# -----------------------------------------------------------------------------

test_that("the subject is the unit: n counts subjects, mean of subject means", {
  # Subject 1 has three rows, subject 2 has one
  df <- data.frame(id = c(1, 1, 1, 2), cho_total = c(1, 2, 3, 6))

  s <- summarize_oxidation(df)

  expect_equal(s$n, 2)
  expect_equal(s$cho_total_mean, mean(c(2, 6)))  # not the row mean, 3
  expect_equal(s$cho_total_sd, sd(c(2, 6)))
})

test_that("by groups are kept and subjects are averaged within them", {
  df <- data.frame(
    id = c(1, 1, 2, 2, 1, 2),
    protocol = c("A", "A", "A", "A", "B", "B"),
    fat_total = c(0.2, 0.4, 0.5, 0.7, 0.1, 0.3)
  )

  s <- summarize_oxidation(df, by = "protocol")

  expect_equal(s$protocol, c("A", "B"))
  expect_equal(s$n, c(2, 2))
  expect_equal(s$fat_total_mean, c(mean(c(0.3, 0.6)), mean(c(0.1, 0.3))))
})

test_that("SE and CI match hand computation with qt()", {
  x <- c(0.3, 0.5, 0.4, 0.6, 0.2)
  df <- data.frame(id = 1:5, fat_total = x)
  se <- sd(x) / sqrt(5)

  s <- summarize_oxidation(df)

  expect_equal(s$fat_total_se, se)
  expect_equal(s$fat_total_ci_low, mean(x) - qt(0.975, df = 4) * se)
  expect_equal(s$fat_total_ci_high, mean(x) + qt(0.975, df = 4) * se)
  expect_equal(error_halfwidth(s, "fat_total", "sd"), sd(x))
  expect_equal(error_halfwidth(s, "fat_total", "se"), se)
  expect_equal(error_halfwidth(s, "fat_total", "ci"), qt(0.975, df = 4) * se)
})

test_that("level sets the CI coverage", {
  x <- c(0.3, 0.5, 0.4, 0.6, 0.2)
  df <- data.frame(id = 1:5, fat_total = x)
  se <- sd(x) / sqrt(5)

  s <- summarize_oxidation(df, level = 0.90)

  expect_equal(s$fat_total_ci_low, mean(x) - qt(0.95, df = 4) * se)
  expect_equal(s$fat_total_ci_high, mean(x) + qt(0.95, df = 4) * se)
  expect_error(summarize_oxidation(df, level = 95), "level")
})

test_that("missing values reduce k, not n", {
  x <- c(1, 2, NA, 4)
  df <- data.frame(id = 1:4, cho_total = x)

  s <- summarize_oxidation(df)

  expect_equal(s$n, 4)
  expect_equal(s$cho_total_se, sd(c(1, 2, 4)) / sqrt(3))
  expect_equal(s$cho_total_ci_high, mean(c(1, 2, 4)) + qt(0.975, 2) * s$cho_total_se)
})

test_that("CI is NA with a single unit", {
  df <- data.frame(id = c(1, 1), cho_total = c(1, 2))

  s <- summarize_oxidation(df)

  expect_equal(s$n, 1)
  expect_equal(s$cho_total_mean, 1.5)
  expect_true(is.na(s$cho_total_ci_low))
  expect_true(is.na(s$cho_total_ci_high))
})

test_that("systematic uncertainty is averaged, not divided by sqrt(n)", {
  df <- data.frame(
    id = c(1, 1, 2, 3, 4),
    cho_total = c(1, 1.2, 1.5, 2, 1.8),
    cho_total_u = 0.05,
    cho_total_u_sys = c(0.1, 0.1, 0.2, 0.3, 0.4)
  )

  s <- summarize_oxidation(df)

  # Mean of subject means (0.1, 0.2, 0.3, 0.4); no division by sqrt(4)
  expect_equal(s$cho_total_u_sys, 0.25)
  # Uncertainty columns are never summarised as variables
  expect_false(any(grepl("_u_(sys_)?(mean|sd|se)$", names(s))))
})

test_that("default vars cover oxidation, e_ and pct_ columns only", {
  df <- data.frame(
    id = 1:3, cho_total = 1:3, rer = c(0.9, 0.92, 0.95),
    e_total = 4:6, pct_fat = 7:9, other = 1:3, e_total_u = 0.1
  )

  s <- summarize_oxidation(df)

  expect_true(all(c("cho_total_mean", "rer_mean", "e_total_mean",
                    "pct_fat_mean") %in% names(s)))
  expect_false(any(grepl("^other|^id_|_u_mean", names(s))))
})

test_that("rows are the unit, with a message, when there is no subject column", {
  df <- data.frame(cho_total = c(1, 2, 3, 6))

  expect_message(s <- summarize_oxidation(df), "row")
  expect_equal(s$n, 4)
  expect_equal(s$cho_total_mean, 3)

  expect_message(
    s2 <- summarize_oxidation(data.frame(id = c(1, 1), cho_total = 1:2), unit = NULL),
    "row"
  )
  expect_equal(s2$n, 2)
})

test_that("summary_unit finds the subject column", {
  results <- create_summary_test_results()

  expect_equal(summary_unit(results, results@oxidation_rates), "id")
  expect_equal(summary_unit(NULL, data.frame(id = 1)), "id")
  expect_null(summary_unit(NULL, data.frame(x = 1)))
})

# -----------------------------------------------------------------------------
# Tables, plots and quick_summary()
# -----------------------------------------------------------------------------

test_that("tables accept error = 'ci' and name the statistic", {
  results <- create_summary_test_results()

  tbl <- tbl_oxidation_summary(results, by = "protocol", error = "ci")
  expect_s3_class(tbl, "gt_tbl")
  html <- gt::as_raw_html(tbl)
  expect_match(html, "Mean (95% CI)", fixed = TRUE)
  expect_match(html, " to ", fixed = TRUE)

  tbl <- tbl_energy_contribution(results, by = "protocol", error = "ci", level = 0.9)
  expect_s3_class(tbl, "gt_tbl")
  expect_match(gt::as_raw_html(tbl), "Mean (90% CI)", fixed = TRUE)

  html <- gt::as_raw_html(tbl_oxidation_summary(results, error = "se"))
  expect_match(html, "Mean ± SE; n = number of subjects", fixed = TRUE)
})

test_that("tables count subjects, not rows", {
  results <- create_summary_test_results()

  tbl <- tbl_oxidation_summary(results, by = "protocol")

  expect_equal(tbl[["_data"]]$n, c(2, 2))
})

test_that("tables add a systematic column when u_sys is present", {
  df <- data.frame(id = 1:3, cho_total = c(1, 2, 3), cho_total_u_sys = 0.1)

  html <- gt::as_raw_html(tbl_oxidation_summary(df))

  expect_match(html, "± systematic", fixed = TRUE)
})

test_that("plots accept error_type = 'ci' and draw error bars", {
  results <- create_summary_test_results()

  plots <- list(
    plot_oxidation_timecourse(results, by = "protocol", error_type = "ci"),
    plot_rer_timecourse(results, error_type = "ci"),
    plot_vo2_timecourse(results, by = "protocol", error_type = "ci", level = 0.9),
    plot_energy_contribution(results, error_type = "ci")
  )

  for (p in plots) {
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
    expect_true(has_errorbar(p))
  }
})

test_that("plot error bars are the CI half-width across subjects", {
  results <- create_summary_test_results()

  p <- plot_rer_timecourse(results, error_type = "ci")

  # 4 subjects per time point, so df = 3
  expect_equal(p$data$n, rep(4, 4))
  expect_equal(p$data$error, qt(0.975, df = 3) * p$data$rer_se)
})

test_that("quick_summary reports subjects", {
  results <- create_summary_test_results()

  msgs <- capture_messages(quick_summary(results))

  expect_true(any(grepl("n = 4 subjects", msgs)))
})
