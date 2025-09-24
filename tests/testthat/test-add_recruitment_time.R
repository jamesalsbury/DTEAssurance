test_that("add_recruitment_time works with power model", {
  df <- data.frame(time = rexp(20, rate = 0.1))
  result <- add_recruitment_time(df, rec_method = "power", rec_period = 12, rec_power = 1)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("rec_time", "pseudo_time") %in% names(result)))
  expect_equal(nrow(result), 20)
  expect_true(all(result$rec_time >= 0))
  expect_equal(attr(result, "recruitment_model"), "power")
})

test_that("add_recruitment_time works with PWC model", {
  df <- data.frame(time = rexp(30, rate = 0.1))
  result <- add_recruitment_time(df, rec_method = "PWC",
                                 rec_rate = "2,3", rec_duration = "6,6")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("rec_time", "pseudo_time") %in% names(result)))
  expect_equal(nrow(result), 30)
  expect_true(all(result$rec_time >= 0))
  expect_equal(attr(result, "recruitment_model"), "PWC")
})

test_that("add_recruitment_time throws error for missing power inputs", {
  df <- data.frame(time = rexp(10))
  expect_error(add_recruitment_time(df, rec_method = "power"), "rec_period and rec_power")
})

test_that("add_recruitment_time throws error for mismatched PWC inputs", {
  df <- data.frame(time = rexp(10))
  expect_error(add_recruitment_time(df, rec_method = "PWC",
                                    rec_rate = "2,3", rec_duration = "6"), "same length")
})

test_that("add_recruitment_time throws error for invalid method", {
  df <- data.frame(time = rexp(10))
  expect_error(add_recruitment_time(df, rec_method = "invalid"), "rec_method must be either")
})
