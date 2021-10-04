#' Make predictions with Convolutional Kitchen Sinks
#'
#' `kitchen_prediction` trains models on CKS-projected training data and then
#'  predicts values for some other data using those models.
#'
#'  This function has a few uses. Most simply, it can generate predictions for
#'  data from a single kitchen sink model when provided with a single set of
#'  hyperparameters (feature count and window size). \code{\link{kitchen_sweep}}()
#'  can quickly assess what hyperparameters perform best. Models are trained by
#'  ridge regression using \code{\link[glmnet]{cv.glmnet}}, allowing for reduction
#'  of overfitting. When `reps` is greater than one, this function makes multiple
#'  predictions using unique normal matrices; the set of predictions can be used
#'  for confidence intervals of the true kernel function. If a set of
#'  hyperparameters are provided, the function will generate predictions
#'  for each unique combination, which could allow for model averaging.
#'

#'
#' @param trainx A matrix of independent data to use for training models.
#'   trainx[i,] should correspond with trainy[i].
#' @param trainy A vector of dependent data to use for training models.
#'   trainy[i] should correspond with trainx[i,].
#' @param predictx A vector or matrix to predict values for.
#'   Values must correspond with the training data.
#' @param features A value or vector of feature counts to use in the
#'   kitchen sink models.
#' @param windows A value or vector of window sizes to use in the
#'   kitchen sink models. Values should not exceed the column length
#'   of the data.
#' @param verbose Print progress.
#' @param simplify Return a matrix rather than a list when there is only
#' one combination of feature count and window size.
#' @param bootstrap A number of times to bootstrap predictions by predicting
#' from data sampled with replacement.
#' @param seed Set a seed for repeatable norms.
#' @param write_progress A file name to write predictions to.
#' @param ... Arguments to be passed to \code{\link{kitchen_sink}}().
#' Can include a bias term or alternate nonlinearization functions.
#'
#' @return Returns a list where index [[f]][[w]] returns the predicted
#'   values from a model with feature count features[f] and window size
#'   windows[w].
#'
#' @seealso \code{\link{kitchen_sweep}}()
#'
#' @examples
#' x <- matrix(sample(1:10,10000,T),2000,5)
#' y <- 5*x[,1] + 20*x[,1]*x[,2] + 3*x[,3]^2 - 10*x[,4] - 2*x[,5]
#' kitchen_sweep(x[1:1000,],y[1:1000],
#'    x[1001:2000,],y[1001:2000],
#'    2^(4:8),2:5)
#'
#' a <- matrix(sample(1:10,1000,T),1,5)
#' b <- 5*a[,1] + 20*a[,1]*a[,2] + 3*a[,3]^2 - 10*a[,4] - 2*a[,5]
#' mybootstrap <- kitchen_prediction(x,y,a,64,5,bootstrap=10)
#' hist(unlist(mybootstrap))
#'
#' @author Avery Kruger
#'
#' @export

kitchen_prediction <- function(
  trainx,
  trainy,
  predictx,
  features,
  windows,
  verbose = TRUE,
  simplify = FALSE,
  clampoutliers = TRUE,
  bootstrap = NULL,
  seed = NULL,
  write_progress = NULL,
  ...
){
  if(is.vector(predictx)){predictx <- matrix(predictx, nrow = 1)}
  if(ncol(trainx) != ncol(predictx)) stop(
    'Training data and prediction data columns do not match.'
  )
  if(min(c(features,windows)) < 1) stop(
    'All hyperparameters must be >= 1'
  )
  if(max(windows) > ncol(trainx)){
    if(!(min(windows) > ncol(trainx))){
      windows <- windows[windows <= ncol(trainx)]
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
      } else{
      predictx <- matrix(predictx, nrow=1)
    }
  }
  if(is.list(trainy)){
    trainy <- unlist(trainy)
  }

  if(is.null(bootstrap)){reps <- 1}else{reps <- bootstrap}
for(f in 1:length(features)){
  predictions[[f]] <- list()
  attr(predictions[[f]],"feature count") <- features[f]
  for(w in 1:length(windows)){
    if(any(is.vector(predictx), nrow(predictx) == 1)){
      predictions[[f]][[w]] <- matrix(NA, 1, reps)
    } else{
      predictions[[f]][[w]] <- matrix(NA, nrow=nrow(predictx), ncol=reps)
    }

    if(!is.null(seed)){
      global.seed <- .Random.seed
      set.seed(seed)
      on.exit(.Random.seed <<- global.seed)
    }

      mynorm <- make_norms(features[f], windows[w])[[1]][[1]]

      if(verbose){
        print(paste0(length(windows)*(f-1)+w,"/",
                     length(features)*length(windows),
                     " Creating Kitchen Sink for Prediction Data"))}
      nonlinpredict <- kitchen_sink(predictx, mynorm, ...)

      for(i in 1:reps){
        if(!is.null(bootstrap)){
          mysample <- sample(seq(nrow(trainx)),nrow(trainx), replace = T)
          myx <- trainx[mysample,]
          myy <- trainy[mysample]}
        else{myx <- trainx
        myy <- trainy}

      if(verbose){
        print(paste0(length(windows)*(f-1)+w,"/",
                     length(features)*length(windows),
                     " Creating Training Sink for Rep ", i))}
      nonlintrain <- kitchen_sink(myx, mynorm, ...)

      if(verbose){
        print(paste0(length(windows)*(f-1)+w,"/",
                     length(features)*length(windows),
                     " Running Ridge Regression"))}
      ridge_model <- glmnet::cv.glmnet(nonlintrain, myy, alpha = 0)


      if(verbose){
        print(paste0(length(windows)*(f-1)+w,"/",
                     length(features)*length(windows),
                     " Predicting"))}

      y_predicted <- predict(ridge_model$glmnet.fit,
                             s = ridge_model$lambda.min,
                             newx = nonlinpredict)
      if(clampoutliers){
        y_predicted <- clamp(clamp(y_predicted,
                                   value = min(trainy)),
                             value = max(trainy), logic = `>`)
      }

      predictions[[f]][[w]][,i] <- y_predicted
      if(!is.null(write_progress)){
        saveRDS(predictions, write_progress)
      }
      if(verbose){
        if(Sys.time() - mytime > 1){
          print(paste0('Feature ',f, '/', length(features),
                     ' Window ', w, '/', length(windows),
                     ' Rep ', i, '/', reps))
          mytime <- Sys.time()
        }
      }
      }
    attr(predictions[[f]][[w]],"window size") <- windows[w]
  }
}
  if(length(features)*length(windows) == 1 & simplify){
    return(unlist(predictions))
    } else{
    predictions
  }
}
