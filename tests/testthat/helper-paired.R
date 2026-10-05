# Small, fast settings shared by the paired-replicate tests.
paired_test_setup <- function() {
  design <- make_rpact_design_from_GSD_model(
    list(alpha_IF = c(0.75, 1), alpha_spending = c(0.0125, 0.025),
         futility_type = "PP", futility_IF = 0.5)
  )$design
  list(
    n_c = 60, n_t = 60, total_events = 80, futility_IF = 0.5,
    efficacy_IF = 0.75, design = design,
    control_model = list(dist = "Exponential", parameter_mode = "Distribution",
                         t1 = 12, t1_Beta_a = 20, t1_Beta_b = 20),
    effect_model = list(
      P_S = 0.9, P_DTE = 0.5,
      HR_SHELF = SHELF::fitdist(c(0.6, 0.7, 0.8), probs = c(0.25, 0.5, 0.75),
                                lower = 0, upper = 2),
      HR_dist = "gamma",
      delay_SHELF = SHELF::fitdist(c(2, 3, 4), probs = c(0.25, 0.5, 0.75),
                                   lower = 0, upper = 10),
      delay_dist = "gamma"
    ),
    recruitment_model = list(method = "power", period = 12, power = 1),
    truth = list(lambda_c = log(2) / 12, delay_time = 3, post_delay_HR = 0.6),
    analysis_model_LRT = list(method = "LRT", alpha = 0.025,
                              alternative_hypothesis = "one.sided")
  )
}
