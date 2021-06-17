#' Create a set of normal matrices
#'
#' `make_norms` creates a list of normal matrices of mean 0 and
#' standard deviation 1 for each combination of feature length and
#' window size that can easily be applied to \code{\link{kitchen}}().
#'
#' @param featuresweep   A vector of feature sizes. Often varied in
#' exponents of 2, e.g. 2^c(4:10)
#' @param windowsweep  A vector of window sizes. Maximum should be no
#' larger than the length of the data to be analyzed.
#'
#' @return Returns a list of matrices where index [[f]][[w]] returns
#' the normal matrix for featuresweep[f] and windowsweep[w].
#'
#' @seealso \code{\link{sweep_kitchen}}()
#'
#' \code{\link{kitchen}}()
#'
#' @examples
#' x <- matrix(1:5000,100,50)
#'
#' myfeaturesweep <- 2^c(4:10)
#' mywindowsweep <- seq(10,ncol(x),10)
#' mynorms <- make_norms(myfeaturesweep, mywindowsweep)
#'
#' for(f in 1:length(myfeaturesweep)){
#'   for(w in 1:length(mywindowsweep)){
#'       nonlinearize(x,mynorms[[f]][[w]])
#'       }}
#'
#' @author Avery Kruger
#'

make_norms <- function(featuresweep, windowsweep){
  norm_list <- list()
  findex <- 1:length(featuresweep)
  windex <- 1:length(windowsweep)
  for(f in findex){
    norm_list[[f]] <- list()
    for(w in windex){
      nfeatures <- featuresweep[f]
      window <- windowsweep[w]
      norm_list[[f]][[w]] <- matrix(rnorm(nfeatures*window,mean=0,sd=1), window, nfeatures)
    }
  }
  norm_list
}
