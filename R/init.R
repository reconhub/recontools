#' Initializes a new package
#' @param pkg_name the package name
#' @param path the directory path. Default is the current directory
#' @param check_cran_name check if the name is already taken on CRAN
#' @export
init_package <- function(pkg_name, path = ".", check_cran_name = TRUE) {
  if (!valid_name(pkg_name)) {
    stop("Package name is not valid", call. = FALSE)
  }

  # check if the name is already taken
  if (check_cran_name) {
    repo_url <- getOption("repos")
    repo_url <- repo_url[repo_url != "@CRAN@"]
    if (length(repo_url) == 0) {
      repo_url <- "https://cloud.r-project.org/"
    }
    available_packages <- utils::available.packages(repos = repo_url)
    name_on_cran <- pkg_name %in% available_packages[, "Package"]
    if (name_on_cran) {
      stop("The package name", pkg_name, "is already taken on CRAN.
           Consider choosing a different name.", call. = FALSE)
    }
  }

  # check git
  if (!git2r::in_repository(path)) {
    message("Initializing git repository")
    git2r::init(path = path)

    # add a default gitignore
    write_template("gitignore", path,
                   list(pkg_name = pkg_name),
                   local_name = ".gitignore")
  }
  file_exists <- function(file) {
    file.exists(file.path(path, file))
  }
  r_source_path <- file.path(path, "R")
  create_dir(r_source_path)
  no_previous_r_files <- length(list.files(r_source_path)) == 0
  if (no_previous_r_files) {
    write_template("please-change.R", r_source_path, use_glue = FALSE)
  }

  # add a default Rbuildignore
  write_template("Rbuildignore", path,
                 list(pkg_name = pkg_name),
                 local_name = ".Rbuildignore")

  # create description
  if (!file_exists("DESCRIPTION")) {
    message("* Adding DESCRIPTION")
    current_r_version <- paste0(R.Version()$major, ".",
                                R.Version()$minor)
    write_template("DESCRIPTION", path,
                   list(r_version = current_r_version,
                        pkg_name = pkg_name,
                        license = "What LICENSE?"))
  }

  if (!file_exists("LICENSE") &&
      ask_yesno("Add MIT license?")) {
    devtools::use_mit_license(path)
  }

  # create tests
  if (!dir.exists(file.path(path, "tests"))) {
    message("* Adding testthat")
    suppressMessages(devtools::use_testthat(pkg = path))
  }

  # code of conduct
  if (!file_exists("CONDUCT.md")) {
    message("* Adding CONDUCT.md from devtools")
    suppressMessages(devtools::use_code_of_conduct(pkg = path))
  }

  # create readme
  if (!file_exists("README.Rmd")) {
    message("* Adding README.Rmd from template")
    write_template("README.Rmd", path, list(pkg_name = pkg_name))
  }

  if (!file_exists(".travis.yml") &&
      ask_yesno("Add travis CI?")) {
    message("* Adding .travis.yml from template")
    write_template("travis.yml", path, list(pkg_name = pkg_name),
                   local_name = ".travis.yml")
  }

  if (!file_exists("appveyor.yml") &&
      ask_yesno("Add appveyor CI?")) {
    message("* Adding appveyor.yml from template")
    write_template("appveyor.yml", path,
                   list(pkg_name = pkg_name))
  }

  if (!file_exists(".lintr")) {
    message("* Adding .lintr file")
    write_template("lintr", path, local_name = ".lintr")
  }

  if (!file_exists("NEWS.md")) {
    devtools::use_news_md(path)
  }

  devtools::document(pkg = path)

  message("All done! ", praise::praise())
}

write_template <- function(tpl_name, path,
                           parameters = list(),
                           local_name = tpl_name,
                           use_glue = TRUE) {
  if (file.exists(file.path(path, local_name))) {
    return()
  }
  tpl_path <- system.file("templates", tpl_name,
                          package = "recontools")
  stopifnot(file.exists(tpl_path))
  tpl_content <- readr::read_file(tpl_path)
  param_envir <- as.environment(parameters)
  parent.env(param_envir) <- environment()
  tpl_content <- if (use_glue) {
    glue::glue(tpl_content, .envir = param_envir)
  } else {
    tpl_content
  }
  invisible(writeLines(tpl_content, file.path(path, local_name)))
}

valid_name <- function(name) {
  all(grepl(pattern = "^[a-zA-Z0-9\\.]+$", x = name))
}

create_dir <- function(path) {
  if (!dir.exists(path) && !file.exists(path)) {
    dir.create(path)
  }
}

ask <- function(prompt, type_fun = as.character) {
  type_fun(readline(prompt = paste0(prompt, " ")))
}

ask_yesno <- function(prompt, default = "y") {
  response <- tolower(ask(paste0(prompt, " [Y/N]:")))
  (if (nchar(response) == 0) default else response) == "y"
}
