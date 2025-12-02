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

test_that("cens_data works for Time censoring", {
  df <- data.frame(
    pseudo_time = c(1, 5, 10),
    rec_time = c(0, 0, 0),
    time = c(1, 5, 10)
  )

  out <- cens_data(df, cens_method = "Time", cens_time = 6)

  expect_equal(out$cens_time, 6)
  expect_equal(out$sample_size, 3)  # only pseudo_time <= 6 remain enrolled
  expect_true(all(c("data", "cens_events", "cens_time", "sample_size") %in% names(out)))
})

test_that("cens_data errors when missing cens_time for Time method", {
  df <- data.frame(pseudo_time = 1:3, rec_time = 0, time = 1:3)
  expect_error(cens_data(df, cens_method = "Time"), "Please specify 'cens_time'")
})

test_that("cens_data works for Events censoring", {
  df <- data.frame(
    pseudo_time = c(2, 4, 6, 8),
    rec_time = c(0, 0, 0, 0),
    time = c(2, 4, 6, 8)
  )

  out <- cens_data(df, cens_method = "Events", cens_events = 2)

  expect_equal(out$cens_time, 4)
  expect_equal(out$sample_size, 4)
})

test_that("cens_data errors when cens_events missing for Events method", {
  df <- data.frame(pseudo_time = 1:3, rec_time = 0, time = 1:3)
  expect_error(cens_data(df, cens_method = "Events"), "Please specify 'cens_events'")
})

test_that("cens_data errors when cens_events too large", {
  df <- data.frame(pseudo_time = 1:3, rec_time = 0, time = 1:3)
  expect_error(cens_data(df, cens_method = "Events", cens_events = 10),
               "exceeds number of observations")
})

test_that("cens_data works for IF censoring", {
  df <- data.frame(
    pseudo_time = c(1, 3, 6, 9),
    rec_time = c(0, 0, 0, 0),
    time = c(1, 3, 6, 9)
  )

  out <- cens_data(df, cens_method = "IF", cens_IF = 0.5)

  expect_equal(out$cens_time, 3)
  expect_equal(out$sample_size, 4)
})

test_that("cens_data errors when cens_IF missing for IF method", {
  df <- data.frame(pseudo_time = 1:3, rec_time = 0, time = 1:3)
  expect_error(cens_data(df, cens_method = "IF"), "Please specify 'cens_IF'")
})

test_that("cens_data errors for invalid IF index (too small)", {
  df <- data.frame(pseudo_time = 1:3, rec_time = 0, time = 1:3)
  expect_error(cens_data(df, cens_method = "IF", cens_IF = 0),
               "Invalid 'cens_IF'")
})

test_that("cens_data creates time column if missing", {
  df <- data.frame(
    pseudo_time = c(2, 4, 10),
    rec_time = c(0, 0, 0)
  )

  out <- cens_data(df, cens_method = "Time", cens_time = 5)

  expect_true("time" %in% names(out$data))
})

test_that("cens_data errors for invalid method", {
  df <- data.frame(pseudo_time = 1:3, rec_time = 0, time = 1:3)
  expect_error(cens_data(df, cens_method = "BAD"), "cens_method must be one of")
})









