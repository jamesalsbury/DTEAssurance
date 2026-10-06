# Apply a group-sequential design to one simulated trial

Runs the looks of a group-sequential design on one simulated trial, in
order, stopping at the first look whose rule is met: efficacy if Z
exceeds the efficacy critical value, futility under the design's
futility rule (`"Beta"`: Z below the beta-spending bound; `"MatchedZ"`:
Z below `GSD_model$futility_boundary_Z`; `"PP"`: predictive probability
below `GSD_model$kappa`), and otherwise the final analysis. Each look
cuts the trial with
[`cens_data`](https://jamesalsbury.github.io/DTEAssurance/reference/cens_data.md)
at `floor(IF * total_events)` events. This is the engine of
[`calc_dte_assurance_adaptive`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md).
For simulation studies, use
[`run_paired_scenario`](https://jamesalsbury.github.io/DTEAssurance/reference/run_paired_scenario.md)
and
[`apply_design_rule`](https://jamesalsbury.github.io/DTEAssurance/reference/apply_design_rule.md),
which evaluate all designs on the same replicates.

## Usage

``` r
apply_GSD_to_trial(
  n_c,
  n_t,
  trial_data,
  design,
  total_events,
  GSD_model,
  control_model = NULL,
  effect_model = NULL,
  recruitment_model = NULL,
  analysis_model = NULL,
  update_priors_sims = NULL,
  PP_sims = NULL,
  .PP_val = NULL
)
```

## Arguments

- n_c, n_t:

  Planned number of patients in the control / treatment arm.

- trial_data:

  A simulated trial with columns `time`, `group`, `rec_time` and
  `pseudo_time`.

- design:

  The `rpact` design, from
  [`make_gsd_design`](https://jamesalsbury.github.io/DTEAssurance/reference/make_gsd_design.md).

- total_events:

  Maximum planned number of events.

- GSD_model:

  The design specification (see
  [`calc_dte_assurance_adaptive`](https://jamesalsbury.github.io/DTEAssurance/reference/calc_dte_assurance_adaptive.md)).

- control_model, effect_model, recruitment_model:

  Analysis prior and recruitment model, used only for `"PP"` futility
  (recruitment must be `method = "power"` with `power = 1`).

- analysis_model:

  Test specification (`method`, `alpha`, `alternative_hypothesis`, and
  `rho`, `gamma`, `t_star`, `s_star` as needed); defaults to a one-sided
  LRT at 0.025.

- update_priors_sims, PP_sims:

  MCMC samples per chain and predictive draws; required for `"PP"`
  futility.

- .PP_val:

  Internal, for testing: a precomputed predictive probability to use at
  the PP futility look instead of computing it.

## Value

A list with `decision` (`"Stop for efficacy"`, `"Stop for futility"`,
`"Successful at final"` or `"Unsuccessful at final"`), `stop_time`,
`sample_size`, `PP_val`, `converged` and `Z_probs`.
