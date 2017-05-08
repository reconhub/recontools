expect_exists <- function(path) {
  testthat::expect_true(dir.exists(path) | file.exists(path))
}

# returns an empty tempdir
new_tempdir <- function() {
  path <- tempdir()
  build_dir <- digest::sha1(paste0(Sys.time(), runif(1)))
  path <- file.path(path, build_dir)
  if (!dir.exists(path)) {
    dir.create(path)
  } else {
    unlink(file.path(path, "*"), recursive = TRUE)
  }
  path
}
