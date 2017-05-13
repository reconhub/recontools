#' Performes various checks
#'
#' @param path the path to the package
#' @param run_gp run goodpractice tests
#' @export
check_package <- function(path = ".", run_gp = FALSE) {
  stopifnot(length(path) == 1, is.character(path))
  package <- devtools::as.package(path)
  if (run_gp) {
    if ("goodpractice" %in% utils::installed.packages()) {
      message("Running goodpractice::gp")
      message("--------------------------")
      print(goodpractice::gp(path = path))
      message("--------------------------")
      if(!ask_to_continue()) {
        return()
      }
    } else {
      message("Please consider installing `goodpractice`")
      message("devtools::install_github('MangoTheCat/goodpractice')")
    }
    message("")
  }
  message("Running RECON specific tests:")
  ok <- check_at_least_one_markdown_vignette(package)

  ok <- ok & check_no_imports(package)

  ok <- ok & check_news_file(package)

  ok <- ok & check_tests(package)

  ok <- ok & check_roxygen2(package)

  message("")
  if (!ok) {
    message("Consider fixing the issues identified above.")
    tpl <- "However, your package is already ${adjective}!"
    message(praise::praise(template = tpl))
  } else {
    message("All good. ",
            praise::praise(template = "Your package is ${adjective}!"))
  }
}

ask_to_continue <- function() {
  if (!interactive()) {
    return(TRUE)
  }
  res <- readline("Press enter to continue or type :q to quit")
  res != ":q"
}

check_at_least_one_markdown_vignette <- function(package) {
  path <- package$path
  vignette_path <- file.path(path, "vignettes")
  vignettes <- list.files(vignette_path, pattern = "\\.Rmd$")
  ok <- length(vignettes) > 0
  message_test(ok, "Packages should have at least one rmarkdown vignette")
  ok
}

check_no_imports <- function(package) {
  path <- package$path
  if (file.exists(file.path(path, "NAMESPACE"))) {
    ns <- devtools::parse_ns_file(package)
    ok <- length(ns$imports) == 0
    message_test(ok, paste0("Packages should not import ",
                            "functions in NAMESPACE but use :: instead"))
    ok
  } else {
    TRUE
  }
}

check_news_file <- function(package) {
  path <- package$path
  ok <- file.exists(file.path(path, "NEWS.md"))
  message_test(ok, paste0("Packages should have a NEWS.md file"))
  ok
}

check_tests <- function(package) {
  path <- package$path
  ok <- dir.exists(file.path(path, "tests"))
  message_test(ok, paste0("Packages should have tests"))
  ok
}

check_conduct <- function(package) {
  path <- package$path
  ok <- file.exists(file.path(path, "CONDUCT.md"))
  message_test(ok, paste0("Packages should have a CONDUCT.md file"))
  ok
}

check_roxygen2 <- function(package) {
  ok <- !is.null(package$roxygennote)
  message_test(ok, paste0("Packages should use roxygen2"))
  ok
}

message_test <- function(result, text) {
  if (!result) {
    result <- crayon::red("x")
  } else {
    result <- crayon::green("\u2713")
  }
  message("   ", result, " ", text)
}

