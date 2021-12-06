my_packages = c("shiny", "DBI", "RMySQL", "shinythemes", "DT", "EloRating", "ggplot2", "stringr", "shinyauthr")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
