#' Bootstrap kitchen sink model predictions
#'
#' `boot_kitchen` trains models on training data and then predicts values
#' for some other data using unique normal matrices. The output can be used
#' for convidence intervals or probability.
#'
#' This function bootstraps predictions of a kitchen sink model
#' by fitting models for a number of normal matrices and then predicting
#' values on the prediction data for each. It is simplest to choose a
#' single set of superparameters (feature count and window size) ahead of
#' time, but `boot_kitchen` can perform sweeps across superparameters as well.
#'
#' @param trainx A matrix of independent data to use for training models.
#'   trainx[i,] should correspond with trainy[i].
#' @param trainy A vector of dependent data to use for training models.
#'   trainy[i] should correspond with trainx[i,].
#' @param predictx A vector or matrix which `boot_kitchen` will predict values for.
#'   Values must correspond with the training data.
#' @param featuresweep A value or vector of feature counts to use in the
#'   kitchen sink models.
#' @param windowsweep A value or vector of window sizes to use in the
#'   kitchen sink models. Values should not exceed the column length
#'   of the data.
#' @param reps The number of repetitions to perform for each combination of
#'   feature count and window size.
#' @param verbose Print bootstrap progress.
#'
#' @return Returns a list where index [[f]][[w]] returns the predicted
#'   values from a model with feature count featuresweep[f] and window size
#'   windowsweep[w].
#'
#' @seealso \code{\link{sweep_kitchen}}()
#'
#' @examples
#' x <- matrix(sample(1:10,10000,T),2000,5)
#' y <- 5*x[,1] + 20*x[,1]*x[,2] + 3*x[,3]^2 - 10*x[,4] - 2*x[,5]
#' quicksweep(x[1:1000,],y[1:1000],
#'    x[1001:2000,],y[1001:2000],
#'    2^(4:8),2:5)
#'
#' a <- matrix(sample(1:10,5,T),1,5)
#' b <- 5*a[,1] + 20*a[,1]*a[,2] + 3*a[,3]^2 - 10*a[,4] - 2*a[,5]
#' mybootstrap <- boot_kitchen(x,y,a,64,5,100)
#' hist(unlist(mybootstrap))
#'
#' @author Avery Kruger
#'

boot_kitchen <- function(
  trainx,
  trainy,
  predictx,
  featuresweep,
  windowsweep,
  reps = 100,
  verbose = TRUE
){
  if(is.vector(predictx)){predictx <- matrix(predictx, nrow = 1)}
  if(ncol(trainx) != ncol(predictx)) stop(
    'Training data and prediction data columns do not match.'
  )
  if(max(windowsweep) > ncol(trainx)){
    if(!(min(windowsweep) > ncol(trainx))){
      windowsweep <- windowsweep[windowsweep <= ncol(trainx)]
      print('Window size cannot be greater than ncol(trainx)')
      Sys.sleep(0.5)
      print('Using remaining window sizes')
      Sys.sleep(0.5)
    } else{
      stop('Window size cannot be greater than ncol(trainx)')
    }
  }
  if(verbose){mytime <- Sys.time()}
  predictions <- list()

  if(is.data.frame(trainx)){
    trainx <- as.matrix(trainx)
  }
  if(is.data.frame(predictx)){
    if(nrow(predictx) > 1){
      predictx <- as.matrix(predictx)
      predictions <- matrix(NA, nrow=nrow(predictx), ncol=reps)
      } else{
      predictx <- matrix(predictx, nrow=1)
    }
  }
  if(is.list(trainy)){
    trainy <- unlist(y)
  }

for(f in 1:length(featuresweep)){
  predictions[[f]] <- list()
  attr(predictions[[f]],"feature count") <- featuresweep[f]
  for(w in 1:length(windowsweep)){
    if(any(is.vector(predictx), nrow(predictx) == 1)){
      predictions[[f]][[w]] <- matrix(NA, 1, reps)
    } else{
      predictions[[f]][[w]] <- matrix(NA, nrow=nrow(predictx), ncol=reps)
    }
    for(i in 1:reps){
      mynorm <- make_norms(featuresweep[f], windowsweep[w])[[1]][[1]]
      nonlinx <- kitchen(trainx, mynorm)
      x <- as.data.frame(nonlinx)
      model <- lm(trainy ~ ., data=x)

      x <- as.data.frame(kitchen(predictx, mynorm))
      predictions[[f]][[w]][,i] <- suppressWarnings(
        predict(model, x)
        )
      if(verbose){
        if(Sys.time() - mytime > 1){
          print(paste0('Feature ',f, '/', length(featuresweep),
                     ' Window ', w, '/', length(windowsweep),
                     ' Rep ', i, '/', reps))
          mytime <- Sys.time()
        }
      }
      }
    attr(predictions[[f]][[w]],"window size") <- windowsweep[w]
  }
}
  predictions
}
