#' Quickly check performance of different kitchen sink models
#'
#' Sweeps across a set of feature counts and window sizes to provide
#' an idea of how different convolutional kitchen sink models perform.
#' Trains on a set of training data, x and y, and validates on separate data.
#' Only uses a single normal matrix for each model, so expect some
#' variance.
#'
#'@param  trainx A matrix of independent data the kitchen sink models will
#'    be trained on.
#'@param  trainy  A vector of dependent data the kitchen sink models will
#'    be trained on. trainy[i] should correspond to trainx[i,].
#'@param valx  A matrix of independent data the kitchen sink models will
#'  use to  make predictions.  Columns should be organized identically to the
#'  trainx data.
#'@param valy A vector of dependent data the kitchen sink models will use
#'  to validate predictions. valy[i] should correspond to valx[i,]
#'@param featuresweep A vector of feature counts to sweep across.
#'@param windowsweep A vector of window sizes to sweep across.
#'@param verbose Show progress if it takes a while.
#'
#'@return Returns a matrix of the adjusted R^2 values from the linear models
#' lm(valy ~ predictions(valx)) for each model. Index [f,w] returns
#' R^2 of the model for featuresweep[f] and windowsweep[w].
#'
#'@seealso \code{\link{make_norms}}()
#'
#'   \code{\link{boot_kitchen}}
#'
#'   \code{\link{kitchen}}()
#'
#' @examples
#' x <- matrix(sample(1:10,500,T),100,5)
#' y <- x[,1]*x[,2]^2-0.5*x[,3]*x[,4]+x[,5]*x[,1]*x[,3]-x[,3]^2*x[,2]
#'
#' a <- matrix(sample(1:10,500,T),100,5)
#' b <- x[,1]*x[,2]^2-0.5*x[,3]*x[,4]+x[,5]*x[,1]*x[,3]-x[,3]^2*x[,2]
#'
#' quicksweep(x,y,a,b,2^(4:8),2:5)
#'
#' @author Avery Kruger
#'

sweep_kitchen <- function(
  trainx,
  trainy,
  valx,
  valy,
  featuresweep,
  windowsweep,
  verbose = FALSE
){
  if(is.data.frame(trainx)){
    trainx <- as.matrix(trainx)
  }
  if(is.data.frame(valx)){
    trainx <- as.matrix(valx)
  }
  if(is.list(trainy)){
    trainy <- unlist(y)
  }
  if(is.list(valy)){
    trainy <- unlist(y)
  }

  allr2 <- as.data.frame(
    matrix(
      data = NA,
      nrow=length(featuresweep),
      ncol=length(windowsweep)
      )
    )
  rownames(allr2) <- as.character(featuresweep)
  colnames(allr2) <- as.character(windowsweep)

  mynorms <- make_norms(featuresweep,windowsweep)

  for(f in 1:length(featuresweep)){
    for(w in 1:length(windowsweep)){
      nonlinx <- kitchen(trainx, mynorms[[f]][[w]])
      x <- as.data.frame(nonlinx)
      model <- lm(trainy ~ ., data = x)

      x <- as.data.frame(kitchen(valx, mynorms[[f]][[w]]))
      predictions <- suppressWarnings(predict(model,x))

      valmodel <- lm(valy ~ predictions)
      allr2[f,w] <- summary(valmodel)$adj.r.squared
      if(verbose){
        print(paste0(length(windowsweep)*(f-1)+w,"/",
                     length(featuresweep)*length(windowsweep)))
      }
    }
  }
  allr2
}

