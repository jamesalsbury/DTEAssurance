# Column names of a single_paired_rep() row, in order.
paired_rep_columns <- function(mw_t_stars) {
  mw <- function(prefix) paste0(prefix, "_MW_t", mw_t_stars)
  c("rep_id", "seed_used", "state",
    "true_lambda_c", "true_gamma_c", "true_delay", "true_HR",
    "t_int", "n_int", "frac_recruited_int", "fu_gt3_int", "fu_gt6_int",
    "Z_int_LRT", mw("Z_int"),
    "PP_val", "converged", "rhat_max", "n_nonfinite_rhat",
    "P_Z1", "P_Z2", "P_Z3",
    "t_eff", "n_eff", "Z_eff_LRT", mw("Z_eff"),
    "t_fin", "n_fin", "Z_fin_LRT", mw("Z_fin"),
    "continuation_success", "continuation_stop_time", "continuation_sample_size",
    "t_interim", "sample_size_interim",
    "error")
}

# One-row data frame of NAs with the single_paired_rep() columns, used when a
# replicate fails.
paired_rep_na_row <- function(i, seed_used, mw_t_stars, error) {
  cols <- paired_rep_columns(mw_t_stars)
  row <- stats::setNames(as.list(rep(NA_real_, length(cols))), cols)
  row$rep_id <- as.integer(i)
  row$seed_used <- as.integer(seed_used)
  row$state <- NA_integer_
  row$converged <- NA
  row$error <- as.character(error)
  as.data.frame(row, stringsAsFactors = FALSE)
}


#' Simulate one paired replicate for post hoc design evaluation
#'
#' Simulates a single trial and records everything needed to evaluate a
#' family of designs afterwards, without baking any design into the run:
#' interim statistics and the predictive probability (PP) at the futility
#' look, and the test statistics at the efficacy look and the final analysis
#' of the \emph{real} trial (which is never stopped here).
#'
#' All designs are then summaries of the returned columns. With critical
#' values \eqn{c_{eff}}, \eqn{c_{fin}} from \code{design} and a final-only
#' critical value \eqn{c_1}:
#' \itemize{
#'   \item D1 (fixed design): success if \code{Z_fin_LRT > c_1}.
#'   \item D2 (efficacy look only): success if \code{Z_eff_LRT > c_eff},
#'     otherwise \code{Z_fin_LRT > c_fin}. This is
#'     \code{continuation_success}.
#'   \item D3 (D2 plus PP futility): stop for futility if
#'     \code{PP_val < kappa}; see \code{\link{summarize_grid_by_kappa}}.
#'   \item D4, D5 (D2 plus Z futility): stop for futility if
#'     \code{Z_int_LRT} (or \code{Z_int_MW_t*}) is below a cutoff.
#' }
#'
#' @param i Replicate index.
#' @param seed Base seed. Replicate \code{i} uses
#'   \code{set.seed(seed * 1e5 + i)}, which must be below
#'   \code{.Machine$integer.max}; the JAGS seed is drawn from that stream, so
#'   the replicate is fully reproducible.
#' @param n_c,n_t Planned number of patients in the control / treatment arm.
#' @param total_events Maximum planned number of events.
#' @param futility_IF Information fraction of the futility (PP) look.
#' @param efficacy_IF Information fraction of the interim efficacy look
#'   (default 0.75).
#' @param design An \code{rpact} group-sequential design containing the
#'   efficacy look at \code{efficacy_IF} and the final analysis (and
#'   optionally the futility look, with zero alpha spent).
#' @param control_model,effect_model The \emph{analysis} prior, used for the
#'   interim posterior update (see \code{\link{update_priors}}).
#'   \code{control_model$parameter_mode} must be \code{"Distribution"}.
#' @param recruitment_model Recruitment specification. Must be
#'   \code{method = "power"} with \code{power = 1} (see \code{\link{PP_func}}).
#' @param truth Either a list with the true \code{lambda_c},
#'   \code{delay_time}, \code{post_delay_HR} and optionally \code{gamma_c}
#'   (exponential control arm if \code{NULL}) and \code{state}, or
#'   \code{NULL}, in which case the truth is drawn from \code{control_model}
#'   and \code{effect_model} via \code{simulate_trial_with_recruitment()},
#'   optionally with the latent state fixed by \code{force_state}.
#' @param force_state \code{NULL}, or 1 (no separation), 2 (immediate
#'   separation) or 3 (delayed separation). Only used when
#'   \code{truth = NULL}.
#' @param analysis_model_LRT Analysis specification for the log-rank test
#'   (\code{method = "LRT"}, \code{alpha}, \code{alternative_hypothesis}),
#'   used for the PP calculation and the \code{*_LRT} statistics.
#' @param mw_t_stars Values of \eqn{t^*} for the modestly weighted
#'   log-rank statistics (default \code{c(2, 6)}).
#' @param update_priors_sims Number of posterior samples per chain
#'   (required).
#' @param PP_sims Number of posterior-predictive draws (required).
#'
#' @return A one-row data frame with columns: \code{rep_id},
#'   \code{seed_used}, \code{state}, \code{true_lambda_c}, \code{true_gamma_c}
#'   (1 for an exponential control arm), \code{true_delay}, \code{true_HR};
#'   interim \code{t_int}, \code{n_int}, \code{frac_recruited_int} (enrolled
#'   / planned), \code{fu_gt3_int}, \code{fu_gt6_int} (fraction of enrolled
#'   treated patients with follow-up \code{t_int - rec_time} above 3 and 6),
#'   \code{Z_int_LRT}, \code{Z_int_MW_t<t>}; \code{PP_val}, \code{converged},
#'   \code{rhat_max}, \code{n_nonfinite_rhat}, \code{P_Z1}, \code{P_Z2},
#'   \code{P_Z3}; \code{t_eff}, \code{n_eff}, \code{Z_eff_LRT},
#'   \code{Z_eff_MW_t<t>}, \code{t_fin}, \code{n_fin}, \code{Z_fin_LRT},
#'   \code{Z_fin_MW_t<t>}; the D2 outcome \code{continuation_success},
#'   \code{continuation_stop_time}, \code{continuation_sample_size}; aliases
#'   \code{t_interim} and \code{sample_size_interim} (for
#'   \code{\link{summarize_grid_by_kappa}}); and \code{error} (\code{NA}, or
#'   the error message if the replicate failed, in which case all other
#'   outcome columns are \code{NA}).
#'
#' @seealso \code{\link{run_paired_scenario}}
#' @export
single_paired_rep <- function(i, seed,
                              n_c, n_t,
                              total_events,
                              futility_IF,
                              efficacy_IF = 0.75,
                              design,
                              control_model,
                              effect_model,
                              recruitment_model,
                              truth = NULL,
                              force_state = NULL,
                              analysis_model_LRT,
                              mw_t_stars = c(2, 6),
                              update_priors_sims,
                              PP_sims) {

  if (missing(update_priors_sims) || missing(PP_sims)) {
    stop("single_paired_rep: 'update_priors_sims' and 'PP_sims' must be supplied.")
  }
  if (!identical(analysis_model_LRT$method, "LRT")) {
    stop("single_paired_rep: analysis_model_LRT$method must be \"LRT\".")
  }
  check_uniform_recruitment(recruitment_model, "single_paired_rep")

  rep_seed_num <- seed * 1e5 + i
  if (!(rep_seed_num < .Machine$integer.max)) {
    stop("single_paired_rep: seed * 1e5 + i must be below .Machine$integer.max.")
  }
  rep_seed <- as.integer(rep_seed_num)

  tryCatch({

    set.seed(rep_seed)
    jags_seed <- sample.int(.Machine$integer.max, 1)

    # --- 2. Simulate the trial ---
    if (!is.null(truth)) {
      trial_data <- simulate_trial_from_truth(truth, n_c, n_t, recruitment_model)
      state <- if (!is.null(truth$state)) {
        truth$state
      } else if (truth$post_delay_HR == 1) {
        1L
      } else if (truth$delay_time == 0) {
        2L
      } else {
        3L
      }
      tr <- list(lambda_c = truth$lambda_c,
                 gamma_c = if (is.null(truth$gamma_c)) 1 else truth$gamma_c,
                 delay_time = truth$delay_time,
                 post_delay_HR = truth$post_delay_HR,
                 state = state)
    } else {
      trial_data <- simulate_trial_with_recruitment(n_c, n_t, control_model,
                                                    effect_model, recruitment_model,
                                                    force_state = force_state)
      tr <- attr(trial_data, "truth")
    }

    stat_LRT <- function(d) run_test(d, analysis_model_LRT)$Z
    stat_MW <- function(d, prefix) {
      z <- vapply(mw_t_stars, function(t) {
        run_test(d, utils::modifyList(analysis_model_LRT,
                                      list(method = "MW", t_star = t)))$Z
      }, numeric(1))
      stats::setNames(as.list(z), paste0(prefix, "_MW_t", mw_t_stars))
    }
    cut_at <- function(IF) {
      cens_data(trial_data, cens_method = "Events",
                cens_events = n_events_at(total_events, IF))
    }

    # --- 3. Interim (futility) look ---
    cut_int <- cut_at(futility_IF)
    d_int <- cut_int$data
    t_int <- cut_int$cens_time
    n_int <- cut_int$sample_size
    fu_trt <- t_int - d_int$rec_time[d_int$group == "Treatment"]

    # --- 4. Predictive probability ---
    posterior <- update_priors(d_int,
                               control_model = control_model,
                               effect_model  = effect_model,
                               n_samples     = update_priors_sims,
                               jags_seed     = jags_seed)
    Zp <- attr(posterior, "Z_probs")

    PP_out <- PP_func(d_int, posterior,
                      control_distribution = control_model$dist,
                      n_c_planned       = n_c,
                      n_t_planned       = n_t,
                      rec_time_planned  = recruitment_model$period,
                      df_cens_time      = t_int,
                      analysis_model    = analysis_model_LRT,
                      future_boundaries = make_future_boundaries(design, total_events,
                                                                 futility_IF),
                      n_sims            = PP_sims)

    # --- 5. Continuation of the real trial (never stopped) ---
    cut_eff <- cut_at(efficacy_IF)
    cut_fin <- cut_at(1)
    Z_eff <- stat_LRT(cut_eff$data)
    Z_fin <- stat_LRT(cut_fin$data)

    # --- 6. D2 outcome ---
    crit_eff <- crit_at(design, efficacy_IF)
    crit_fin <- crit_at(design, 1)
    if (is.na(crit_eff) || is.na(crit_fin)) {
      stop("design has no critical value at efficacy_IF and/or IF = 1.")
    }
    eff_success <- !is.na(Z_eff) && Z_eff > crit_eff
    cont <- if (eff_success) {
      list(1, cut_eff$cens_time, cut_eff$sample_size)
    } else {
      list(as.numeric(!is.na(Z_fin) && Z_fin > crit_fin),
           cut_fin$cens_time, cut_fin$sample_size)
    }

    # --- 7. Assemble ---
    row <- c(
      list(rep_id = as.integer(i), seed_used = rep_seed,
           state = as.integer(tr$state),
           true_lambda_c = tr$lambda_c, true_gamma_c = tr$gamma_c,
           true_delay = tr$delay_time, true_HR = tr$post_delay_HR,
           t_int = t_int, n_int = n_int,
           frac_recruited_int = n_int / (n_c + n_t),
           fu_gt3_int = mean(fu_trt > 3), fu_gt6_int = mean(fu_trt > 6),
           Z_int_LRT = stat_LRT(d_int)),
      stat_MW(d_int, "Z_int"),
      list(PP_val = mean(PP_out$PP_df$success),
           converged = attr(posterior, "converged"),
           rhat_max = attr(posterior, "rhat_max"),
           n_nonfinite_rhat = attr(posterior, "n_nonfinite_rhat"),
           P_Z1 = unname(Zp["P_Z1"]), P_Z2 = unname(Zp["P_Z2"]),
           P_Z3 = unname(Zp["P_Z3"]),
           t_eff = cut_eff$cens_time, n_eff = cut_eff$sample_size,
           Z_eff_LRT = Z_eff),
      stat_MW(cut_eff$data, "Z_eff"),
      list(t_fin = cut_fin$cens_time, n_fin = cut_fin$sample_size,
           Z_fin_LRT = Z_fin),
      stat_MW(cut_fin$data, "Z_fin"),
      list(continuation_success = cont[[1]],
           continuation_stop_time = cont[[2]],
           continuation_sample_size = cont[[3]],
           t_interim = t_int, sample_size_interim = n_int,
           error = NA_character_)
    )
    out <- as.data.frame(row, stringsAsFactors = FALSE)
    out[paired_rep_columns(mw_t_stars)]

  }, error = function(e) {
    paired_rep_na_row(i, rep_seed, mw_t_stars, conditionMessage(e))
  })
}


#' Run paired replicates for one scenario, with checkpointing
#'
#' Runs \code{\link{single_paired_rep}} for \code{i = 1, ..., n_sims}, in
#' chunks of \code{chunk_size} replicates (in parallel via
#' \code{parallel::mclapply} when \code{n_cores > 1}). After each chunk, all
#' rows so far are written to \code{checkpoint_file}; if that file exists at
#' the start, the run resumes from the replicates already completed.
#'
#' @param n_sims Number of replicates.
#' @param seed Base seed (see \code{\link{single_paired_rep}}).
#' @param ... Further arguments passed to \code{\link{single_paired_rep}}.
#' @param n_cores Number of cores (default 1).
#' @param checkpoint_file Optional path of an RDS checkpoint file.
#' @param chunk_size Number of replicates per chunk (default 100).
#'
#' @return A list with \code{raw} (one row per replicate, ordered by
#'   \code{rep_id}) and \code{settings} (all arguments, the package, R,
#'   rjags and JAGS versions, the git commit of the working directory, the
#'   hostname, a timestamp and \code{n_failed}). The
#'   number of failed replicates is reported with a message, and a warning
#'   is given if more than 1\% failed.
#'
#' @seealso \code{\link{single_paired_rep}}, \code{\link{summarize_grid_by_kappa}}
#' @export
run_paired_scenario <- function(n_sims, seed, ..., n_cores = 1,
                                checkpoint_file = NULL, chunk_size = 100) {

  args <- list(...)
  mw_t_stars <- if (is.null(args$mw_t_stars)) c(2, 6) else args$mw_t_stars

  if (!(seed * 1e5 + n_sims < .Machine$integer.max)) {
    stop("run_paired_scenario: seed * 1e5 + n_sims must be below .Machine$integer.max.")
  }

  done <- NULL
  if (!is.null(checkpoint_file) && file.exists(checkpoint_file)) {
    ck <- readRDS(checkpoint_file)
    if (!identical(ck$seed, seed)) {
      stop("run_paired_scenario: checkpoint_file was written with seed = ",
           ck$seed, ", not ", seed, ". Use a different checkpoint_file.")
    }
    done <- ck$raw
    message("run_paired_scenario: resuming from ", nrow(done),
            " completed replicates in ", checkpoint_file)
  }

  remaining <- setdiff(seq_len(n_sims), done$rep_id)
  chunks <- split(remaining, ceiling(seq_along(remaining) / chunk_size))

  run_one <- function(i) {
    tryCatch(do.call(single_paired_rep, c(list(i = i, seed = seed), args)),
             error = function(e) paired_rep_na_row(i, seed * 1e5 + i, mw_t_stars,
                                                   conditionMessage(e)))
  }

  for (chunk in chunks) {
    res <- if (n_cores > 1) {
      parallel::mclapply(chunk, run_one, mc.cores = n_cores)
    } else {
      lapply(chunk, run_one)
    }
    # A worker that dies under mclapply returns a try-error (or NULL), not
    # a data frame.
    res <- Map(function(r, i) {
      if (is.data.frame(r)) r else
        paired_rep_na_row(i, seed * 1e5 + i, mw_t_stars,
                          paste("worker failed:", paste(as.character(r), collapse = " ")))
    }, res, chunk)
    done <- rbind(done, do.call(rbind, res))

    if (!is.null(checkpoint_file)) {
      tmp <- paste0(checkpoint_file, ".tmp")
      saveRDS(list(raw = done, seed = seed), tmp)
      file.rename(tmp, checkpoint_file)
    }
  }

  raw <- done[done$rep_id <= n_sims, , drop = FALSE]
  raw <- raw[order(raw$rep_id), , drop = FALSE]
  rownames(raw) <- NULL

  n_failed <- sum(!is.na(raw$error))
  message("run_paired_scenario: ", n_failed, " of ", nrow(raw), " replicates failed.")
  if (n_failed > 0.01 * nrow(raw)) {
    warning("run_paired_scenario: ", n_failed, " of ", nrow(raw),
            " replicates (more than 1%) failed; see the 'error' column.")
  }

  settings <- do.call(make_settings, c(
    list(n_sims = n_sims, seed = seed, n_cores = n_cores,
         checkpoint_file = checkpoint_file, chunk_size = chunk_size),
    args,
    list(n_failed = n_failed)
  ))

  list(raw = raw, settings = settings)
}


#' Apply a design rule to paired replicate output
#'
#' Turns the output of \code{\link{run_paired_scenario}} into the decisions
#' of one design, post hoc. The efficacy look is only checked if the trial
#' was not stopped at the futility look. A missing test statistic never
#' crosses a boundary (as in \code{\link{apply_GSD_to_trial}}).
#'
#' \describe{
#'   \item{D1}{Fixed design: success iff \code{Z_fin_LRT > crit_final}.}
#'   \item{D2}{Efficacy look then final: \code{"Stop for efficacy"} if
#'     \code{Z_eff_LRT > crit_eff}, otherwise the final analysis with
#'     \code{Z_fin_LRT > crit_fin}.}
#'   \item{D3}{D2 plus PP futility: \code{"Stop for futility"} if
#'     \code{PP_val < kappa}.}
#'   \item{D4}{D2 plus Z futility: \code{"Stop for futility"} if
#'     \code{Z_int_LRT < z_fut}.}
#'   \item{D5}{As D4 with the modestly weighted statistics
#'     \code{Z_int_MW_t<t>}, \code{Z_eff_MW_t<t>}, \code{Z_fin_MW_t<t>} for
#'     \code{t = mw_t_star}.}
#' }
#'
#' @param raw The \code{raw} element returned by \code{\link{run_paired_scenario}}.
#' @param rule A list with \code{type} (\code{"D1"} to \code{"D5"}) and the
#'   rule's parameters: \code{crit_final} (D1); \code{crit_eff},
#'   \code{crit_fin} (D2-D5); \code{kappa} (D3); \code{z_fut} (D4, D5);
#'   \code{mw_t_star} (D5).
#'
#' @return A data frame with one row per replicate and columns
#'   \code{decision} (\code{"Stop for efficacy"}, \code{"Stop for futility"},
#'   \code{"Successful at final"} or \code{"Unsuccessful at final"}),
#'   \code{success}, \code{early_fut}, \code{early_eff}, \code{sample_size}
#'   and \code{duration}. Rows for failed replicates (non-missing
#'   \code{error}) are \code{NA}.
#'
#' @seealso \code{\link{run_paired_scenario}}, \code{\link{summarize_grid_by_kappa}}
#' @export
apply_design_rule <- function(raw, rule) {

  type <- rule$type
  needed <- switch(type,
                   D1 = "crit_final",
                   D2 = c("crit_eff", "crit_fin"),
                   D3 = c("kappa", "crit_eff", "crit_fin"),
                   D4 = c("z_fut", "crit_eff", "crit_fin"),
                   D5 = c("z_fut", "mw_t_star", "crit_eff", "crit_fin"),
                   stop("apply_design_rule: rule$type must be one of D1-D5."))
  missing_par <- needed[!needed %in% names(rule)]
  if (length(missing_par) > 0) {
    stop("apply_design_rule: rule ", type, " needs ", paste(missing_par, collapse = ", "), ".")
  }

  stat <- if (type == "D5") paste0("MW_t", rule$mw_t_star) else "LRT"
  col <- function(prefix) {
    nm <- paste0(prefix, "_", stat)
    if (!nm %in% names(raw)) stop("apply_design_rule: column ", nm, " not found in raw.")
    raw[[nm]]
  }
  crosses_above <- function(z, crit) !is.na(z) & z > crit
  crosses_below <- function(z, crit) !is.na(z) & z < crit

  n <- nrow(raw)
  no_stop <- rep(FALSE, n)

  if (type == "D1") {
    fut <- no_stop
    eff <- no_stop
    fin_success <- crosses_above(raw$Z_fin_LRT, rule$crit_final)
  } else {
    fut <- switch(type,
                  D2 = no_stop,
                  D3 = raw$PP_val < rule$kappa,
                  D4 = ,
                  D5 = crosses_below(col("Z_int"), rule$z_fut))
    eff <- !fut & crosses_above(col("Z_eff"), rule$crit_eff)
    fin_success <- crosses_above(col("Z_fin"), rule$crit_fin)
  }

  decision <- ifelse(fut, "Stop for futility",
                     ifelse(eff, "Stop for efficacy",
                            ifelse(fin_success, "Successful at final",
                                   "Unsuccessful at final")))
  out <- data.frame(
    decision = decision,
    success = decision %in% c("Stop for efficacy", "Successful at final"),
    early_fut = fut,
    early_eff = eff,
    sample_size = ifelse(fut, raw$n_int, ifelse(eff, raw$n_eff, raw$n_fin)),
    duration = ifelse(fut, raw$t_int, ifelse(eff, raw$t_eff, raw$t_fin)),
    stringsAsFactors = FALSE
  )

  failed <- !is.na(raw$error) | is.na(fut)
  out[failed, ] <- NA
  out
}
