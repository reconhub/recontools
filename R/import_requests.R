#' Read RECON requests
#'
#'
#' @export
#' @author Thibaut Jombart

import_requests <- function(file = "RECON-requests", compile = TRUE,
			    erase = FALSE) {
  file_Rmd <- paste(file, "Rmd", sep = ".")
  file_html <- paste(file, "html", sep = ".")

  if (file.exists(file_Rmd) && !erase) {
    stop("file", file_Rmd, "exists; remove or rename it")
  }
  if (file.exists(file_html) && !erase) {
    stop("file", file_html, "exists; remove or rename it")
  }

  if (!require("googlesheets")) {
    devtools::install_github("jennybc/googlesheets")
    if (!require("googlesheets")) {
      stop("googlesheets is not present and cannot be installed")
    }
  }

  title <- "recon-requests"
  input <- gs_read(gs_title(title))
  output <- as.data.frame(input)

  format_output <- function(x) {
    txt <- c(" ",
	     paste("##", x$"Short title"),
	     "### Author",
	     paste(x$"First name", x$"Last name"),
	     "<br>",
	     x$"Institution", "<br>",
	     x$"Email address",
	     "",
	     "### Description",
	     x$"Short description",
	     "<br>",
	     "<br>",
	     "<br>",
	     "")
    out <- paste(txt, collapse = "\n")
    out
  }


  header <- c(
	      "---",
	      "title: RECON requests",
	      "author: R Epidemics Consortium",
	      paste("date:", format(Sys.time(), "%d %b %Y")),
	      "output:",
	      "  html_document:",
	      "    toc: true",
	      "    toc_depth: 1",
	      "---",
	      " "
	    )

  cat(header, sep = "\n", file = file_Rmd, append = FALSE)
  rows <- seq.int(nrow(output))
  for (i in rows) {
    tmp <- format_output(output[i,])
    ## cat(tmp, file = file_Rmd, append = (i > 1L))
    cat(tmp, file = file_Rmd, append = TRUE)
  }

  if (compile) {
    rmarkdown::render(file_Rmd)
  }

  browseURL(file_html)
}
