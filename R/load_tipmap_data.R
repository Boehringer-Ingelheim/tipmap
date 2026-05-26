#' Load exemplary datasets
#'
#' @description
#' Loads one of three exemplary datasets in the package.
#'
#' @param file The dataset to be loaded.
#'
#' @return A pre-saved dataset.
#' @export
#' @examples
#'
#' load_tipmap_data(file = "tipdat.rds")
#' load_tipmap_data(file = "tipmapPrior.rds")
#' load_tipmap_data(file = "tipPost.rds")
#'
load_tipmap_data <- function(file) {
  assert_that(is.character(file), msg = "`file` must be character")
  assert_that(length(file) == 1, msg = "`file` must be length 1")
  assert_that(!is.na(file) && nzchar(file), msg = "`file` must be a non-empty string")
  
  path <- system.file("extdata", file, package = "tipmap")
  assert_that(nzchar(path), msg = paste0("Could not find extdata file: ", file))
  
  readRDS(path)
}
