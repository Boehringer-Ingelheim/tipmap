#' Load exemplary datasets
#'
#' @description
#' Loads one of three exemplary datasets in the package.
#'
#' @param file The dataset to be loaded.
#'
#' @return A pre-saved dataset.
#'
#' @examples
#'
#' load_tipmap_data(file = "tipdat.rds")
#' load_tipmap_data(file = "tipmapPrior.rds")
#' load_tipmap_data(file = "tipPost.rds")
#'
load_tipmap_data <- function(file) {
  path <- system.file("extdata", file, package = "tipmap")
  data <- readRDS(path)
  return(data)
}
