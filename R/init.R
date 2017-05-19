#' Initializes a new package
#' @param pkg_name the package name
#' @param path the directory path. Default is the current directory
#' @param check_cran_name check if the name is already taken on CRAN
#' @export
init_package <- function(pkg_name, path = ".", check_cran_name = TRUE) {
  if (length(pkg_name) != 1 || !is.character(pkg_name)) {
    stop("Package name needs to be a character vector of length 1",
         call. = FALSE)
  }
  if (!valid_name(pkg_name)) {
    stop("Package name is not valid", call. = FALSE)
  }
  stopifnot(dir.exists(path))
  stopifnot(is.logical(check_cran_name), length(check_cran_name) == 1)

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

  # at this point we create a new foder with the name
  new_path <- file.path(path, pkg_name)
  if (dir.exists(new_path) && length(list.files(new_path)) > 0) {
    stop("There already exists a non-empty directory with name ",
         pkg_name, ". Please choose another name or remove the directory",
         call. = FALSE)
  } else {
    if (!dir.exists(new_path)) {
      dir.create(new_path)
    }
  }
  path <- new_path

  # create a git repository if there none in the path (or above)
  if (!git2r::in_repository(path)) {
    message("* Initializing git repository")
    git2r::init(path = path)
  }

  # add a default gitignore
  message("* Add default .gitignore")
  write_template("gitignore", path,
                 list(pkg_name = pkg_name),
                 local_name = ".gitignore")

  file_exists <- function(file) {
    file.exists(file.path(path, file))
  }
  r_source_path <- file.path(path, "R")
  create_dir(r_source_path)
  no_previous_r_files <- length(list.files(r_source_path)) == 0
  if (no_previous_r_files) {
    message("* Add sample R file")
    write_template("please-change.R", r_source_path, use_glue = FALSE)
  }

  # add a default Rbuildignore
  message("* Add .Rbuildignore")
  write_template("Rbuildignore", path,
                 list(pkg_name = pkg_name),
                 local_name = ".Rbuildignore")

  # create description
  if (!file_exists("DESCRIPTION")) {
    message("* Add DESCRIPTION")
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

  if (ask_yesno("Add a vignette?")) {
    handle_answer <- function(x) {
      if (nchar(x) == 0) {
        message("* No answer given, calling vignette 'introduction'")
        "introduction"
      } else {
        x
      }
    }
    while(!valid_name(vignette_name <-
                      ask("Please enter a name for the vignette:", type_fun = handle_answer))) {

    }
    if (!file.exists(file.path(path, "vignettes", paste0(vignette_name, ".Rmd")))) {
      devtools::use_vignette(vignette_name, pkg = path)
    } else {
      message("* Vignette with name '", vignette_name,
              "' already present => No new vignette created")
    }
  }

  # create tests
  if (!dir.exists(file.path(path, "tests"))) {
    message("* Add testthat")
    suppressMessages(devtools::use_testthat(pkg = path))
    write_template("test-example.R",
                   path = file.path(path, "tests", "testthat"),
                   use_glue = FALSE)
  }

  # code of conduct
  if (!file_exists("CONDUCT.md")) {
    message("* Add CONDUCT.md from devtools")
    suppressMessages(devtools::use_code_of_conduct(pkg = path))
  }

  # create readme
  if (!file_exists("README.Rmd")) {
    message("* Add README.Rmd from template")
    write_template("README.Rmd", path, list(pkg_name = pkg_name))
  }

  if (!file_exists(".travis.yml") &&
      ask_yesno("Add travis CI?")) {
    message("* Add .travis.yml from template")
    write_template("travis.yml", path, list(pkg_name = pkg_name),
                   local_name = ".travis.yml")
  }

  if (!file_exists("appveyor.yml") &&
      ask_yesno("Add appveyor CI?")) {
    message("* Add appveyor.yml from template")
    write_template("appveyor.yml", path,
                   list(pkg_name = pkg_name))
  }

  if (!file_exists(".lintr")) {
    message("* Add .lintr file")
    write_template("lintr", path, local_name = ".lintr")
  }

  if (!file_exists("NEWS.md")) {
    devtools::use_news_md(path)
  }

  devtools::document(pkg = path)

  # compile readme if it exists
  readme_rmd_path <- file.path(path, "README.Rmd")
  if (file.exists(readme_rmd_path)) {
    message("* Compile Readme.Rmd")
    knitr::knit(readme_rmd_path, output = file.path(path, "README.md"))
  }

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
  length(name) == 1 && grepl(pattern = "^[a-zA-Z]+[a-zA-Z0-9\\.]*$", x = name)
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
  if (!interactive()) {
    return(TRUE)
  }
  response <- tolower(ask(paste0(prompt, " [Y/n]:")))
  (if (nchar(response) == 0) default else response) == "y"
}
