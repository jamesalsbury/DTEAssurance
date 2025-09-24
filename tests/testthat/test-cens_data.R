test_that("cens_data returns expected structure with Time method", {
  df <- data.frame(
    pseudo_time = rexp(20, rate = 0.1),
    rec_time = runif(20, 0, 12),
    time = rexp(20, rate = 0.1)
  )
  result <- cens_data(df, cens_method = "Time", cens_time = 10)
  expect_type(result, "list")
  expect_named(result, c("data", "cens_events", "cens_time", "sample_size"))
  expect_s3_class(result$data, "data.frame")
  expect_true(all(c("status", "time") %in% names(result$data)))
})

test_that("cens_data handles Events method", {
  df <- data.frame(
    pseudo_time = rexp(50, rate = 0.1),
    rec_time = runif(50, 0, 12),
    time = rexp(50, rate = 0.1)
  )
  result <- cens_data(df, cens_method = "Events", cens_events = 30)
  expect_equal(result$cens_events, 30)
  expect_true(result$sample_size <= 50)
})

test_that("cens_data handles IF method", {
  df <- data.frame(
    pseudo_time = rexp(100, rate = 0.1),
    rec_time = runif(100, 0, 12),
    time = rexp(100, rate = 0.1)
  )
  result <- cens_data(df, cens_method = "IF", cens_IF = 0.5)
  expect_true(result$cens_time > 0)
  expect_true(result$sample_size <= 100)
})


