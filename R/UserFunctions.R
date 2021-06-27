#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param y Path to the input file
#' @return a Ssvs object
#' @export
SSVS <- function(y, x, nsave, nburn, tau0, tau1, S0) {
  object=svss_class$new(y, x, nsave, nburn, tau0, tau1, S0)

  return(object)
}
