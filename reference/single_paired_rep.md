# Simulate one paired replicate for post hoc design evaluation

Simulates a single trial and records everything needed to evaluate a
family of designs afterwards, without baking any design into the run:
interim statistics and the predictive probability (PP) at the futility
look, and the test statistics at the efficacy look and the final
analysis of the *real* trial (which is never stopped here).

## Usage

``` r
single_paired_rep(
  i,
  seed,
  n_c,
  n_t,
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
  PP_sims
)
```

## Arguments

- i:

  Replicate index.

- seed:

  Base seed. Replicate `i` uses `set.seed(seed * 1e5 + i)`, which must
  be below `.Machine$integer.max`; the JAGS seed is drawn from that
  stream, so the replicate is fully reproducible.

- n_c, n_t:

  Planned number of patients in the control / treatment arm.

- total_events:

  Maximum planned number of events.

- futility_IF:

  Information fraction of the futility (PP) look.

- efficacy_IF:

  Information fraction of the interim efficacy look (default 0.75).

- design:

  An `rpact` group-sequential design containing the efficacy look at
  `efficacy_IF` and the final analysis (and optionally the futility
  look, with zero alpha spent).

- control_model, effect_model:

  The *analysis* prior, used for the interim posterior update (see
  [`update_priors`](https://jamesalsbury.github.io/DTEAssurance/reference/update_priors.md)).
  `control_model$parameter_mode` must be `"Distribution"`.

- recruitment_model:

  Recruitment specification. Must be `method = "power"` with `power = 1`
  (see
  [`PP_func`](https://jamesalsbury.github.io/DTEAssurance/reference/PP_func.md)).

- truth:

  Either a list with the true `lambda_c`, `delay_time`, `post_delay_HR`
  and optionally `gamma_c` (exponential control arm if `NULL`) and
  `state`, or `NULL`, in which case the truth is drawn from
  `control_model` and `effect_model` via
  `simulate_trial_with_recruitment()`, optionally with the latent state
  fixed by `force_state`.

- force_state:

  `NULL`, or 1 (no separation), 2 (immediate separation) or 3 (delayed
  separation). Only used when `truth = NULL`.

- analysis_model_LRT:

  Analysis specification for the log-rank test (`method = "LRT"`,
  `alpha`, `alternative_hypothesis`), used for the PP calculation and
  the `*_LRT` statistics.

- mw_t_stars:

  Values of \\t^\*\\ for the modestly weighted log-rank statistics
  (default `c(2, 6)`).

- update_priors_sims:

  Number of posterior samples per chain (required).

- PP_sims:

  Number of posterior-predictive draws (required).

## Value

A one-row data frame with columns: `rep_id`, `seed_used`, `state`,
`true_lambda_c`, `true_gamma_c` (1 for an exponential control arm),
`true_delay`, `true_HR`; interim `t_int`, `n_int`, `frac_recruited_int`
(enrolled / planned), `fu_gt3_int`, `fu_gt6_int` (fraction of enrolled
treated patients with follow-up `t_int - rec_time` above 3 and 6),
`Z_int_LRT`, `Z_int_MW_t<t>`; `PP_val`, `converged`, `rhat_max`,
`n_nonfinite_rhat`, `P_Z1`, `P_Z2`, `P_Z3`; `t_eff`, `n_eff`,
`Z_eff_LRT`, `Z_eff_MW_t<t>`, `t_fin`, `n_fin`, `Z_fin_LRT`,
`Z_fin_MW_t<t>`; the D2 outcome `continuation_success`,
`continuation_stop_time`, `continuation_sample_size`; aliases
`t_interim` and `sample_size_interim` (for
[`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md));
and `error` (`NA`, or the error message if the replicate failed, in
which case all other outcome columns are `NA`).

## Details

All designs are then summaries of the returned columns. With critical
values \\c\_{eff}\\, \\c\_{fin}\\ from `design` and a final-only
critical value \\c_1\\:

- D1 (fixed design): success if `Z_fin_LRT > c_1`.

- D2 (efficacy look only): success if `Z_eff_LRT > c_eff`, otherwise
  `Z_fin_LRT > c_fin`. This is `continuation_success`.

- D3 (D2 plus PP futility): stop for futility if `PP_val < kappa`; see
  [`summarize_grid_by_kappa`](https://jamesalsbury.github.io/DTEAssurance/reference/summarize_grid_by_kappa.md).

- D4, D5 (D2 plus Z futility): stop for futility if `Z_int_LRT` (or
  `Z_int_MW_t*`) is below a cutoff.

## See also

[`run_paired_scenario`](https://jamesalsbury.github.io/DTEAssurance/reference/run_paired_scenario.md)
