context("check")
test_that("it runs without errors", {
  path <- new_tempdir()
  setwd(path)
  expect_message(init_package("mypackage", path))
  expect_message(recontools::check_package(".", run_gp = FALSE))
})
