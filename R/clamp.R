#' Clamps values beyond a threshold to the threshold.
#'
#' This function returns its input, with all values beyond a
#' threshold set to a specified value (default 0).
#'
#' @param x  A numeric object.
#' @param bias   A hyperparameter that can be used to adjust x.
#'
#'
#' @return Returns an object formatted identical to x
#'
#' @author Avery Kruger
#'
#' @examples
#' x <- matrix(rnorm(16,500,T),4,4)
#' clamp(x)
#' clamp(x, logic = `>`)
#'
#' @export

clamp <- function(x,
                  bias=0,
                  value=0,
                  logic = `<`){
  x[logic(x-bias, value)] <- value
  x
}
