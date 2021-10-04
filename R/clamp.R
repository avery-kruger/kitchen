#' Clamps values beyond a threshold to the threshold.
#'
#' This function returns its input, with all values beyond a
#' threshold set to a specified value (default 0).
#'
#' @param x  A numeric object.
#' @param bias   A hyperparameter that can be used to adjust x.
#' @param value The value to set (clamp) other values to.
#' @param logic A logical function that returns which in x should be clamped.
#'
#'
#' @return Returns an object formatted identical to x
#'
#' @author Avery Kruger
#'
#' @examples
#' x <- matrix(rnorm(16,500,TRUE),4,4)
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
