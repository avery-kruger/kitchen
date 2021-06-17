#' Nonlinearize data with a convolutional kitchen sink
#'
#' This function nonlinearizes data with a (convolutional)
#'kitchen sink. It matrix multiplies data with a normal matrix
#'that must be provided, clamping the negative results to 0, and
#'then averaging across windows. Feature count and window size are
#'determined by the column and row numbers of the provided normal matrix
#'respectively.
#'
#' @param data   The data to be nonlinearized. Should not include the
#'    variable to be modeled by the kitchen sink.
#' @param norm   A normal matrix of dimensions c(window size, feature count).
#'    Can be generated using make_norms(). Window size should be no larger than
#'    the size (column length) of the data.

#'
#' @return Returns a matrix of nonlinearized counterparts to the original rows
#'   of the provided data. Column length is equal to the number of features,
#'   as determined by the provided normal matrix.
#'   #'
#' @seealso \code{\link{make_norms}}()
#' @author Avery Kruger
#'
#' @examples
#' x <- matrix(sample(1:10,500,T),100,5)
#' y <- x[,1]*x[,2]^2-0.5*x[,3]*x[,4]+x[,5]*x[,1]
#'
#' mynorm <- make_norms(64,5)[[1]][[1]]
#' nonlinx <- nonlinearize(x, mynorm)
#' summary(lm(y ~ nonlinx))
#'

kitchen <- function(data, norm){
  nfeatures <- ncol(norm)
  windowsize <- nrow(norm)
  if(nrow(norm) > ncol(data)) stop(
    'Normal matrix window size greater than ncol(data).')
  if(is.data.frame(data)){
    data <- as.matrix(data)
  }
  output <- matrix(nrow=nrow(data),ncol=nfeatures)
  for(i in 1:nrow(data)){
    windows <- t(sapply(1:(ncol(data)-windowsize+1),
                     function(w){data[i,w:(w+windowsize-1)]}))
    if(windowsize == 1){windows <- t(windows)}
    nonlinwindows <- as.matrix(windows) %*% norm
    nonlinwindows[nonlinwindows < 0] <- 0
    output[i,] <- colMeans(nonlinwindows)
  }
  output
}
