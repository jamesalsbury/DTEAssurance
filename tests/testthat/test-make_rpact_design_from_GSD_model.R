test_that("make_rpact_design_from_GSD_model works for futility = 'none'", {

  GSD_model <- list(
    alpha_IF       = c(0.3, 0.6, 1.0),
    alpha_spending = c(0.005, 0.015, 0.025),
    futility_type  = "none"
  )

  out <- make_rpact_design_from_GSD_model(GSD_model)

  expect_true(is.list(out))
  expect_s3_class(out$design, "TrialDesignGroupSequential")

  expect_equal(out$IF_all, c(0.3, 0.6, 1.0))
  expect_equal(out$alpha_spending_full, c(0.005, 0.015, 0.025))

  # No frequentist futility for "none"
  expect_equal(out$beta_spending_full, c(0, 0, 0))
})

test_that("make_rpact_design_from_GSD_model works for futility = 'Beta'", {

  GSD_model <- list(
    alpha_IF        = c(0.75, 1.0),
    alpha_spending  = c(0.0125, 0.025),
    futility_type   = "Beta",
    futility_IF     = c(0.5, 1),
    beta_spending   = c(0.05, 0.15)
  )

  out <- make_rpact_design_from_GSD_model(GSD_model)

  expect_true(is.list(out))
  expect_s3_class(out$design, "TrialDesignGroupSequential")

  # Futility IFs merged with alpha IFs
  expect_equal(out$IF_all, c(0.5, 0.75, 1.0))

  # alpha spending step-expanded
  expect_equal(out$alpha_spending_full, c(0, 0.0125, 0.025))

  # beta spending step-expanded
  expect_equal(out$beta_spending_full, c(0.05, 0.05, 0.15))
})

test_that("make_rpact_design_from_GSD_model works for futility = 'BPP'", {

  GSD_model <- list(
    alpha_IF        = c(0.4, 1.0),
    alpha_spending  = c(0.012, 0.025),
    futility_type   = "BPP",
    futility_IF     = 0.5,
    BPP_threshold = 0.2
  )

  out <- make_rpact_design_from_GSD_model(GSD_model)

  expect_true(is.list(out))
  expect_s3_class(out$design, "TrialDesignGroupSequential")

  # For BPP: fut_IF included, but beta spending all zero
  expect_equal(out$IF_all, c(0.4, 0.5, 1.0))

  expect_equal(out$alpha_spending_full, c(0.012, 0.012, 0.025))

  expect_equal(out$beta_spending_full, c(0, 0, 0))
})

test_that("make_rpact_design_from_GSD_model errors on unknown futility type", {

  GSD_model <- list(
    alpha_IF        = c(0.5, 1.0),
    alpha_spending  = c(0.01, 0.025),
    futility_type   = "invalid"
  )

  expect_error(
    make_rpact_design_from_GSD_model(GSD_model),
    "Unknown futility type"
  )
})



