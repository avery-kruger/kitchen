#' Project data with Convolutional Kitchen Sinks
#'
#' This function projects data with Convolutional Kitchen Sinks.
#' It matrix multiplies data with a normal matrix that must be
#' provided, clamping the negative results to 0, and
#' then averaging across windows. Feature count and window size are
#' determined by the column and row numbers of the provided normal matrix
#' respectively.
#'
#' @param data   The data to be projected. Should not include the
#'    variable to be modeled by the kitchen sink.
#' @param norm   A normal matrix of dimensions c(window size, feature count).
#'    Can be generated using \code{\link{make_norms}}(). Window size should be
#'    no larger than the size (column length) of the data.
#' @param FUN A function to be used for nonlinearization of the normalized data.
#'    The default, \code{\link{clamp}}, sets negative values to 0.
#'    Other alternatives include cos() and sin().
#' @param ncores How many cores to use for the kitchen sink. Allows for
#'    parallelization in large datasets.
#' @param write_progress Write files for each row calculated. May be helpful
#'    for breaking up large calculations.
#' @param which_rows Specify rows to calculate for. Along with write_progress,
#'    may be helpful for breaking up large calculations.
#'
#' @return Returns a matrix of nonlinearized counterparts to the original rows
#'   of the provided data. Column length is equal to the number of features,
#'   as determined by the provided normal matrix.
#'   #'
#' @seealso \code{\link{make_norms}}()
#'
#'   \code{\link{max0}}
#'
#' @author Avery Kruger
#'
#' @examples
#' x <- matrix(sample(1:10,500,T),100,5)
#' y <- x[,1]*x[,2]^2-0.5*x[,3]*x[,4]+x[,5]*x[,1]
#'
#' mynorm <- make_norms(64,5)[[1]][[1]]
#' nonlinx <- kitchen_sink(x, mynorm, bias=0.1)
#' summary(lm(y ~ nonlinx))
#'
#' @export

kitchen_sink <- function(data,
                         norm,
                         FUN = clamp,
                         ncores = 1,
                         write_progress = FALSE,
                         which_rows = NULL,
                         ...){
  nfeatures <- ncol(norm)
  windowsize <- nrow(norm)
  if(nrow(norm) > ncol(data)) stop(
    'Normal matrix window size greater than ncol(data).')
  if(is.data.frame(data)){
    data <- as.matrix(data)
  }
  if(write_progress){myname <- paste0("sink_",
                                        paste(sample(c(letters, 0:9),10,T),
                                              collapse=""),
                                        "_")}
  if(is.null(which_rows)){which_rows <- seq(nrow(data))}
  todo <- seq(nrow(data))[which_rows]
  temp <- mclapply(todo,
                   function(i){
                     windows <- t(sapply(1:(ncol(data)-windowsize+1),
                                         function(w){data[i,w:(w+windowsize-1)]}))
                     if(windowsize == 1){windows <- t(windows)}
                     nonlinwindows <- as.matrix(windows) %*% norm
                     nonlinwindows <- FUN(nonlinwindows, ...)
                     windowmeans <- colMeans(nonlinwindows)
                     if(write_progress){saveRDS(windowmeans, paste0(myname,i,".rds"))}
                     windowmeans
                   },
                   mc.cores = ncores)
  output <- do.call(rbind, temp)
  output
}

