test_that("assurance_GSD_shiny_app is a callable function", {
  expect_true(is.function(assurance_GSD_shiny_app))
})

test_that("assurance_GSD_shiny_app fails gracefully if app directory is missing", {
  fake_package <- "FakePackage123"
  app_dir <- system.file("shiny/AdaptiveApp/app.R", package = fake_package)
  expect_equal(app_dir, "")
  expect_error(
    shiny::runApp(app_dir, display.mode = "normal"),
    class = "invalidShinyAppDir"
  )
})

test_that("app directory exists in installed package", {
  skip_on_cran()
  app_dir <- system.file("shiny/AdaptiveApp/app.R", package = "DTEAssurance")
  expect_true(file.exists(app_dir))
})
