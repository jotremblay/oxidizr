#' Input Uncertainties to Propagate
#'
#' State the standard uncertainty of each input of [analyze_oxidation()]. The
#' analysis then reports, for each computed rate, its standard uncertainty and
#' the part of it that is systematic. See `vignette("uncertainty")`.
#'
#' Each source is either `"data"`, a number, or 0 (not propagated):
#'
#' - Gas exchange and protein are relative standard uncertainties (0.01 = 1%).
#' - Isotope enrichments are absolute standard uncertainties, in the units of
#'   the IsotopeData (for example per mil PDB).
#'
#' `"data"` takes the uncertainty from the data when the data hold it, and
#' skips the source otherwise:
#'
#' - `vo2`, `vco2`: SD / sqrt(n) of the replicates of each mean, from
#'   [aggregate_calorimetry()] (`analyze_oxidation(aggregate = TRUE)`). When
#'   both come from the data, their replicate correlation is kept (breathing
#'   moves both gases): the source `vo2` moves VO2 and the correlated part of
#'   VCO2, and `vco2` the rest of VCO2. A mean of one replicate has no SD, so
#'   its row gets an NA uncertainty.
#' - `rref`: the SD of the control trials ([calc_rref()]), with each row
#'   getting the control-group mean. If the row's subject did the control
#'   trial (crossover), the mean contains that subject's own background: the
#'   error is SD * sqrt(1 - 1/n), random (source `rref_subject`), and nothing
#'   is shared. If not (parallel groups), the error is SD, random
#'   (`rref_subject`), plus SD / sqrt(n), the uncertainty of the group mean,
#'   shared by all subjects (`rref_mean`, systematic). With
#'   `rref_by_subject = TRUE` there is no group SD, so give a number (the
#'   within-subject variation of the background, random by default).
#'
#' A **random** error differs from one observation to the next; across
#' subjects it averages out, and the SD of the results already contains it.
#' A **systematic** error (calibration, the drink enrichment, the group-mean
#' Rref of a parallel control group) shifts every subject in the same
#' direction; the SD, SE and CI across subjects do not show it, so report it
#' apart (`<var>_u_sys`).
#'
#' In the linear method, a source moves all rows together. Each row's
#' uncertainty is still right. Only values that combine rows (the subject-mean
#' VO2 of the sweat balance) treat the row errors as correlated; the effect on
#' protein oxidation is negligible.
#'
#' @param vo2,vco2 Relative standard uncertainty of each VO2 and VCO2 value, or
#'   `"data"` (default).
#' @param volume Relative standard uncertainty shared by VO2 and VCO2 (for
#'   example a flow or volume calibration error). It moves both gases
#'   together, so its effect on CHO and fat is much smaller than that of
#'   independent errors. Default: 0.
#' @param rexp Absolute standard uncertainty of the expired CO2 enrichment of
#'   the trials with exogenous substrate (the analysis precision). Default: 0.
#' @param rexo Absolute standard uncertainty of the enrichment of the ingested
#'   substrate. Default: 0.
#' @param rpla Absolute standard uncertainty of the plasma glucose enrichment.
#'   Default: 0.
#' @param rref Absolute standard uncertainty of the background enrichment, or
#'   `"data"` (default).
#' @param protein Relative standard uncertainty of protein oxidation (applied
#'   to the urea concentrations). Default: 0.
#' @param method `"linear"` (default): first-order propagation (GUM). Each
#'   source moves its input by one standard uncertainty and the calculations
#'   run again; the change is the contribution of that source. It is exact for
#'   the linear CHO and fat equations and gives [uncertainty_budget()].
#'   `"monte_carlo"`: the calculations run `n_draws` times with normal random
#'   inputs (GUM Supplement 1), for the random and the systematic sources
#'   apart. Use it when a result is not linear in its inputs, for example
#'   exogenous CHO when Rexo - Rref is small. Call [set.seed()] first to repeat
#'   a result.
#' @param n_draws Number of Monte Carlo draws (default: 1000).
#' @param kind Named list that changes the kind ("random" or "systematic") of a
#'   source. Defaults: `volume` and `rexo` are systematic, and a numeric `rref`
#'   is systematic (random with `rref_by_subject = TRUE`); the others are
#'   random. With `rref = "data"`, the design sets the kinds, as described
#'   above.
#'
#' @return An object of class `oxidation_uncertainty`.
#' @export
#'
#' @seealso [analyze_oxidation()], [uncertainty_budget()], `vignette("uncertainty")`
#'
#' @examples
#' # Only what the data show (the default of analyze_oxidation())
#' oxidation_uncertainty()
#'
#' # Stated values: 1% on each gas, 0.1 per mil on Rexp, 1 per mil on Rexo
#' oxidation_uncertainty(vo2 = 0.01, vco2 = 0.01, rexp = 0.1, rexo = 1)
oxidation_uncertainty <- function(vo2 = "data", vco2 = "data", volume = 0,
                                  rexp = 0, rexo = 0, rpla = 0,
                                  rref = "data", protein = 0,
                                  method = c("linear", "monte_carlo"),
                                  n_draws = 1000,
                                  kind = list()) {
  method <- match.arg(method)
  values <- list(vo2 = vo2, vco2 = vco2, volume = volume, rexp = rexp,
                 rexo = rexo, rpla = rpla, rref = rref, protein = protein)
  for (nm in names(values)) {
    x <- values[[nm]]
    ok <- (identical(x, "data") && nm %in% c("vo2", "vco2", "rref")) ||
      (is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0)
    if (!ok) {
      cli::cli_abort("{.arg {nm}} must be a non-negative number{if (nm %in% c('vo2', 'vco2', 'rref')) ' or \"data\"'}.")
    }
  }
  if (identical(rref, "data") && "rref" %in% names(kind)) {
    cli::cli_warn(c(
      "{.code kind$rref} is not used with {.code rref = \"data\"}.",
      "i" = "From the data, the design sets the kinds (see {.fn oxidation_uncertainty})."
    ))
  }
  kinds <- utils::modifyList(
    list(vo2 = "random", vco2 = "random", volume = "systematic", rexp = "random",
         rexo = "systematic", rpla = "random", rref = "systematic", protein = "random"),
    kind
  )
  if (!all(unlist(kinds) %in% c("random", "systematic")) || !all(names(kinds) %in% names(values))) {
    cli::cli_abort("{.arg kind} must name sources of {.fn oxidation_uncertainty} with \"random\" or \"systematic\".")
  }
  if (!is.numeric(n_draws) || n_draws < 2) {
    cli::cli_abort("{.arg n_draws} must be a number of at least 2.")
  }
  structure(list(values = values, kind = kinds, kind_set = names(kind),
                 method = method, n_draws = n_draws),
            class = "oxidation_uncertainty")
}

#' @export
print.oxidation_uncertainty <- function(x, ...) {
  cat("<oxidation_uncertainty>", x$method, if (x$method == "monte_carlo") paste(x$n_draws, "draws"), "\n")
  units <- c(vo2 = "relative", vco2 = "relative", volume = "relative", rexp = "absolute",
             rexo = "absolute", rpla = "absolute", rref = "absolute", protein = "relative")
  for (nm in names(x$values)) {
    v <- x$values[[nm]]
    shown <- if (identical(v, "data")) "from the data" else if (v == 0) "not propagated" else
      paste(format(v), units[[nm]])
    kind <- if (nm == "rref" && identical(v, "data")) "set by the design" else x$kind[[nm]]
    cat(sprintf("  %-8s %-18s %s\n", nm, shown, kind))
  }
  invisible(x)
}

# The sources to propagate: each one says how to move its input by z standard
# uncertainties, in the study (apply) or in the Rref table (rref).
uncertainty_sources <- function(study, u, base, control_protocol, rref_by_subject) {
  v <- u$values
  sources <- list()
  add <- function(name, kind, input_u, apply = function(st, z) st, rref = function(r, z) r) {
    sources[[name]] <<- list(name = name, kind = kind, input_u = input_u, apply = apply, rref = rref)
  }
  calo_df <- study@calorimetry@data

  # Gas exchange. Moved values below 0 are set to 0 (Monte Carlo draws).
  vo2_col <- study@calorimetry@vo2_col
  vco2_col <- study@calorimetry@vco2_col
  move_gases <- function(u_vo2, u_vco2) {
    force(u_vo2)
    force(u_vco2)
    function(st, z) {
      d <- st@calorimetry@data
      d[[vo2_col]] <- pmax(d[[vo2_col]] + z * u_vo2, 0)
      d[[vco2_col]] <- pmax(d[[vco2_col]] + z * u_vco2, 0)
      st@calorimetry@data <- d
      st
    }
  }
  # "data": SD / sqrt(n) of the replicates of each mean; NA with one replicate
  data_u <- function(col) {
    sd_col <- paste0(col, "_sd")
    n_col <- paste0(col, "_n")
    if (!all(c(sd_col, n_col) %in% names(calo_df))) return(NULL)
    calo_df[[sd_col]] / sqrt(calo_df[[n_col]])
  }
  gas_u <- function(gas, col) {
    if (identical(v[[gas]], "data")) data_u(col) else if (v[[gas]] > 0) v[[gas]] * calo_df[[col]]
  }
  u_vo2 <- gas_u("vo2", vo2_col)
  u_vco2 <- gas_u("vco2", vco2_col)
  # Replicate errors of VO2 and VCO2 are correlated (breathing moves both). With
  # both from the data, split their covariance (Cholesky): "vo2" moves VO2 and
  # the correlated part of VCO2, "vco2" moves the rest of VCO2.
  rho <- 0
  if (identical(v$vo2, "data") && identical(v$vco2, "data") && "gas_cor" %in% names(calo_df)) {
    rho <- dplyr::coalesce(calo_df$gas_cor, 0)
  }
  zero <- rep(0, nrow(calo_df))
  if (!is.null(u_vo2) && any(u_vo2 > 0, na.rm = TRUE)) {
    add("vo2", u$kind$vo2, u_vo2,
        apply = move_gases(u_vo2, if (is.null(u_vco2)) zero else rho * u_vco2))
  }
  if (!is.null(u_vco2) && any(u_vco2 > 0, na.rm = TRUE)) {
    add("vco2", u$kind$vco2, u_vco2, apply = move_gases(zero, u_vco2 * sqrt(1 - rho^2)))
  }
  n_na <- sum(is.na(c(if (identical(v$vo2, "data")) u_vo2, if (identical(v$vco2, "data")) u_vco2)) &
                !is.na(c(if (identical(v$vo2, "data")) calo_df[[vo2_col]], if (identical(v$vco2, "data")) calo_df[[vco2_col]])))
  if (n_na > 0) {
    cli::cli_alert_warning("{n_na} gas value{?s} with one replicate: the uncertainty of {?its/their} row{?s} is NA.")
  }

  # Shared error of VO2 and VCO2 (flow or volume)
  if (v$volume > 0) {
    add("volume", u$kind$volume, v$volume, apply = function(st, z) {
      for (col in c(st@calorimetry@vo2_col, st@calorimetry@vco2_col)) {
        st@calorimetry@data[[col]] <- st@calorimetry@data[[col]] * (1 + z * v$volume)
      }
      st
    })
  }

  iso <- study@isotopes
  if (!is.null(iso) && !is.null(control_protocol)) {
    if (v$rexp > 0) {
      add("rexp", u$kind$rexp, v$rexp, apply = function(st, z) {
        d <- st@isotopes@rexp
        moved <- if (!is.null(st@isotopes@protocol_col)) d[[st@isotopes@protocol_col]] != control_protocol else TRUE
        d[[st@isotopes@rexp_col]] <- d[[st@isotopes@rexp_col]] + z * v$rexp * moved
        st@isotopes@rexp <- d
        st
      })
    }
    if (v$rexo > 0) {
      add("rexo", u$kind$rexo, v$rexo, apply = function(st, z) {
        st@isotopes@rexo[[st@isotopes@rexo_col]] <- st@isotopes@rexo[[st@isotopes@rexo_col]] + z * v$rexo
        st
      })
    }
    if (v$rpla > 0 && !is.null(iso@rpla) && nrow(iso@rpla) > 0) {
      add("rpla", u$kind$rpla, v$rpla, apply = function(st, z) {
        st@isotopes@rpla[[st@isotopes@rpla_col]] <- st@isotopes@rpla[[st@isotopes@rpla_col]] + z * v$rpla
        st
      })
    }
    if (identical(v$rref, "data")) {
      sd_ok <- !is.null(base$rref) && any(!is.na(base$rref$rref_sd))
      if (!rref_by_subject && sd_ok) {
        # Each row gets the control-group mean Rref. If its subject did the
        # control trial (crossover), the mean includes its own background:
        # error SD * sqrt(1 - 1/n), random, nothing shared. If not (parallel
        # groups): SD (random) plus SD / sqrt(n) shared by all (systematic).
        rows <- rref_rows(study, base$rref, control_protocol)
        u_subject <- ifelse(rows$in_control, rows$sd * sqrt(1 - 1 / rows$n), rows$sd)
        u_mean <- ifelse(rows$in_control, 0, rows$sd / sqrt(rows$n))
        add("rref_subject", "random", NA_real_, rref = rref_by_row(rows, u_subject, iso))
        if (any(u_mean > 0, na.rm = TRUE)) {
          add("rref_mean", "systematic", NA_real_, rref = rref_by_row(rows, u_mean, iso))
        }
      }
    } else if (v$rref > 0) {
      # With each subject's own control trial, the errors are independent
      # across subjects (random) unless the user says otherwise
      kind <- if ("rref" %in% u$kind_set) u$kind$rref else if (rref_by_subject) "random" else u$kind$rref
      add("rref", kind, v$rref, rref = function(r, z) {
        r$rref <- r$rref + z * v$rref
        r
      })
    }
  }

  # Protein: scale the urea concentrations
  if (v$protein > 0 && has_protein_data(study)) {
    add("protein", u$kind$protein, v$protein, apply = function(st, z) {
      for (col in c(st@urea@sweat_urea_col, st@urea@urine_urea_col)) {
        st@urea@data[[col]] <- st@urea@data[[col]] * (1 + z * v$protein)
      }
      st
    })
  }

  sources
}

# One row per subject and time of the calorimetry data, with the control-group
# SD and n of Rref at that time and whether the subject did the control trial
rref_rows <- function(study, rref, control_protocol) {
  calo <- study@calorimetry
  iso <- study@isotopes
  rows <- unique(data.frame(id = calo@data[[calo@id_col]], time = calo@data[[calo@time_col]]))
  ctrl <- iso@rexp
  if (!is.null(iso@protocol_col)) ctrl <- ctrl[ctrl[[iso@protocol_col]] == control_protocol, ]
  ctrl <- ctrl[!is.na(ctrl[[iso@rexp_col]]), ]
  rows$in_control <- paste(rows$id, rows$time) %in% paste(ctrl[[iso@id_col]], ctrl[[iso@time_col]])
  at <- if (iso@time_col %in% names(rref)) match(rows$time, rref[[iso@time_col]]) else 1
  rows$sd <- rref$rref_sd[at]
  rows$n <- rref$n[at]
  rows
}

# An Rref change by row: returns a per-subject Rref table (isotope ID and time
# columns) that moves each row's group Rref by z times its own uncertainty
rref_by_row <- function(rows, u_row, iso) {
  force(u_row)
  function(r, z) {
    at <- if (iso@time_col %in% names(r)) match(rows$time, r[[iso@time_col]]) else 1
    out <- stats::setNames(rows[c("id", "time")], c(iso@id_col, iso@time_col))
    out$rref <- r$rref[at] + z * u_row
    out
  }
}

# Variables whose uncertainty is reported, by table of the pipeline output
uncertainty_variables <- function(out) {
  ox <- intersect(c("cho_total", "fat_total", "protein_ox", "cho_exo", "cho_endo",
                    "cho_pla", "cho_mus", "cho_liv"), names(out$oxidation))
  en <- if (!is.null(out$energy)) grep("^(e_|pct_)", names(out$energy), value = TRUE) else character()
  list(oxidation = ox, energy = en)
}

# One matrix (rows x variables) of the reported variables of a pipeline output
uncertainty_values <- function(out, vars) {
  cbind(as.matrix(out$oxidation[vars$oxidation]),
        if (length(vars$energy)) as.matrix(out$energy[vars$energy]))
}

# Propagate the input uncertainties `u` through `run` (see analyze_oxidation()).
# Returns the contribution table of OxidationResults@uncertainty, or NULL.
propagate_uncertainty <- function(study, u, base, run, control_protocol, rref_by_subject) {
  sources <- uncertainty_sources(study, u, base, control_protocol, rref_by_subject)
  if (length(sources) == 0) {
    return(NULL)
  }
  cli::cli_alert_info("Propagating uncertainty ({u$method}): {names(sources)}")

  vars <- uncertainty_variables(base)
  base_values <- uncertainty_values(base, vars)
  run_moved <- function(srcs, z) {
    st <- study
    for (i in seq_along(srcs)) st <- srcs[[i]]$apply(st, z[i])
    rref_fn <- function(r) {
      for (i in seq_along(srcs)) r <- srcs[[i]]$rref(r, z[i])
      r
    }
    values <- uncertainty_values(suppressMessages(run(st, rref_fn)), vars)
    if (nrow(values) != nrow(base_values)) {
      cli::cli_abort("The uncertainty run changed the number of rows; cannot match the results.")
    }
    values
  }
  as_table <- function(m, source, kind, input_u) {
    tibble::tibble(
      row_id = rep(seq_len(nrow(m)), ncol(m)),
      variable = rep(colnames(m), each = nrow(m)),
      source = source,
      kind = kind,
      input_u = if (length(input_u) == nrow(m)) rep(input_u, ncol(m)) else input_u[1],
      contribution = as.vector(m)
    )
  }

  if (u$method == "linear") {
    parts <- lapply(sources, function(src) {
      as_table(run_moved(list(src), 1) - base_values, src$name, src$kind, src$input_u)
    })
  } else {
    parts <- lapply(c("random", "systematic"), function(k) {
      srcs <- Filter(function(src) src$kind == k, sources)
      if (length(srcs) == 0) return(NULL)
      draws <- vapply(seq_len(u$n_draws), function(i) run_moved(srcs, stats::rnorm(length(srcs))),
                      base_values)
      sd_draws <- apply(draws, c(1, 2), stats::sd, na.rm = TRUE)
      dimnames(sd_draws) <- dimnames(base_values)
      as_table(sd_draws, paste0("monte_carlo_", k), k, NA_real_)
    })
  }
  dplyr::bind_rows(parts)
}

# Add <var>_u (total) and <var>_u_sys (systematic part) to each output table
add_uncertainty_columns <- function(out, unc) {
  rss <- function(x) if (length(x) == 0) 0 else sqrt(sum(x^2))
  totals <- unc |>
    dplyr::summarise(
      u = rss(.data$contribution),
      u_sys = rss(.data$contribution[.data$kind == "systematic"]),
      .by = c("row_id", "variable")
    )
  totals$u_sys[is.na(totals$u)] <- NA_real_
  for (tbl in c("oxidation", "partition", "energy")) {
    df <- out[[tbl]]
    if (is.null(df)) next
    for (var in intersect(unique(totals$variable), names(df))) {
      t <- totals[totals$variable == var, ]
      df[[paste0(var, "_u")]] <- t$u[match(seq_len(nrow(df)), t$row_id)]
      df[[paste0(var, "_u_sys")]] <- t$u_sys[match(seq_len(nrow(df)), t$row_id)]
    }
    out[[tbl]] <- df
  }
  out
}

#' Rank the Sources of Uncertainty
#'
#' Show how much each source contributes to the uncertainty of one variable,
#' from the `uncertainty` slot of [analyze_oxidation()] results.
#'
#' @param results An OxidationResults object with a propagated uncertainty.
#' @param variable The variable, for example `"cho_total"`, `"cho_exo"` or
#'   `"pct_fat"`.
#' @param by Optional columns of `results@oxidation_rates` to group by (for
#'   example `"protocol"` or `c("protocol", "time")`).
#'
#' @return A tibble with the `by` columns, `source`, `kind`, `contribution`
#'   (root mean square over the rows, in the units of the variable) and
#'   `share` (part of the variance, 0 to 1), the largest first. With
#'   `method = "monte_carlo"` the sources are the random and systematic totals.
#' @export
#'
#' @seealso [oxidation_uncertainty()], `vignette("uncertainty")`
#'
#' @examples
#' \dontrun{
#' res <- analyze_oxidation(study, control_protocol = "Water",
#'                          uncertainty = oxidation_uncertainty(vco2 = 0.01, rexp = 0.1))
#' uncertainty_budget(res, "cho_exo", by = "protocol")
#' }
uncertainty_budget <- function(results, variable, by = NULL) {
  if (!S7_inherits(results, OxidationResults)) {
    cli::cli_abort("results must be an OxidationResults object")
  }
  unc <- results@uncertainty
  if (is.null(unc)) {
    cli::cli_abort(c(
      "These results hold no propagated uncertainty.",
      "i" = "Run {.fn analyze_oxidation} with an {.fn oxidation_uncertainty} that has a source."
    ))
  }
  if (!variable %in% unc$variable) {
    cli::cli_abort("No uncertainty for {.val {variable}}. Available: {.val {unique(unc$variable)}}")
  }
  meta <- results@oxidation_rates
  missing_by <- setdiff(by, names(meta))
  if (length(missing_by) > 0) {
    cli::cli_abort("Grouping columns not found: {.val {missing_by}}")
  }
  meta$row_id <- seq_len(nrow(meta))

  unc[unc$variable == variable, ] |>
    dplyr::left_join(meta[c("row_id", by)], by = "row_id") |>
    dplyr::summarise(
      contribution = sqrt(mean(.data$contribution^2, na.rm = TRUE)),
      .by = dplyr::all_of(c(by, "source", "kind"))
    ) |>
    dplyr::mutate(share = share_of_variance(.data$contribution), .by = dplyr::all_of(by)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(by)), dplyr::desc(.data$share))
}

#' Compare Results Across Constant Sets
#'
#' Run the analysis with several [oxidation_constants()] sets and summarise
#' the results of each, to see how much the choice of constants changes them.
#' This difference is a model choice, not a measurement error, so it is shown
#' apart and does not enter the propagated uncertainty.
#'
#' @param study An OxidationStudy object.
#' @param sets A named list of [oxidation_constants()] sets. Default (NULL):
#'   the oxidizr defaults, then each other option of each part, one at a time.
#' @param vars Variables to summarise (from `oxidation_rates` or, for `e_` and
#'   `pct_` columns, `energy_contributions`).
#' @param by Optional grouping columns (for example `"protocol"`).
#' @param ... Other arguments of [analyze_oxidation()], for example
#'   `control_protocol` or `time_range`.
#'
#' @return A tibble with one row per set (and `by` group): `set`, the `by`
#'   columns, `n` and, for each variable, the summary columns of
#'   [summarize_oxidation()] (mean, SD, SE, CI).
#' @export
#'
#' @seealso [oxidation_constants()], `vignette("uncertainty")`
#'
#' @examples
#' \dontrun{
#' compare_constants(study, vars = c("cho_total", "fat_total"))
#'
#' compare_constants(study, sets = list(
#'   oxidizr = oxidation_constants(),
#'   peronnet_massicotte = oxidation_constants(lipid = "fatty_acid", molar_volume = "real_gas")
#' ))
#' }
compare_constants <- function(study, sets = NULL, vars = c("cho_total", "fat_total"),
                              by = NULL, ...) {
  if (is.null(sets)) sets <- constant_variants()
  if (!is.list(sets) || is.null(names(sets)) || any(names(sets) == "")) {
    cli::cli_abort("{.arg sets} must be a named list of {.fn oxidation_constants} sets.")
  }
  rows <- lapply(names(sets), function(nm) {
    res <- suppressWarnings(suppressMessages(
      analyze_oxidation(study, constants = sets[[nm]], uncertainty = NULL, validate = FALSE, ...)
    ))
    df <- res@oxidation_rates
    if (!all(vars %in% names(df)) && !is.null(res@energy_contributions)) {
      df <- res@energy_contributions
    }
    summary <- suppressMessages(
      summarize_oxidation(df, vars = vars, by = by, unit = summary_unit(res, df))
    )
    dplyr::mutate(summary, set = nm, .before = 1)
  })
  dplyr::bind_rows(rows)
}

# Part of the total variance of each contribution (NA when the total is 0)
share_of_variance <- function(contribution) {
  total <- sum(contribution^2, na.rm = TRUE)
  if (total > 0) contribution^2 / total else rep(NA_real_, length(contribution))
}

# The default constant set, then each other option of each part, one at a time
constant_variants <- function() {
  sets <- list(oxidizr = oxidation_constants())
  for (part in setdiff(names(constant_options), "energy")) {
    for (opt in names(constant_options[[part]])[-1]) {
      sets[[paste0(part, " = ", opt)]] <- do.call(oxidation_constants, stats::setNames(list(opt), part))
    }
  }
  # The 37 degrees C energy values of Peronnet & Massicotte go with their lipid
  sets[["energy = peronnet_massicotte, lipid = fatty_acid"]] <-
    oxidation_constants(lipid = "fatty_acid", energy = "peronnet_massicotte")
  sets
}
