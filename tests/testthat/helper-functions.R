expect_exists <- function(path) {
  testthat::expect_true(dir.exists(path) | file.exists(path))
}
