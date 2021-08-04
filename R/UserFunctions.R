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
SSVS <- function(y, x, nsave=1000, nburn=1000, tau0=0.01, tau1=10, S0=0.01, PriorSemiScaling = F, scaling=T) {
  object=svss_class$new(y, x, nsave, nburn, tau0, tau1, S0, PriorSemiScaling, scaling)

  return(object)
}
