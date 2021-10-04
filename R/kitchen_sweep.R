#' Quickly check performance of different CKS models
#'
#' Sweeps across a set of feature counts and window sizes to provide
#' an idea of how different Convolutional Kitchen Sinks perform.
#' Trains on a set of training data, x and y, and validates on separate data.
#' Only uses a single normal matrix for each model, so expect some
#' variance.
#'
#' Models are trained by ridge regression using \code{\link[glmnet]{cv.glmnet}},
#' allowing for reduction of overfitting.
#'
#'@param  trainx A matrix of independent data the models will be trained on.
#'@param  trainy  A vector of dependent data the models will be trained on.
#' trainy[i] should correspond to trainx[i,].
#'@param valx  A matrix of independent data the kitchen sink models will
#'  use to  make predictions.  Columns should be organized identically to the
#'  trainx data.
#'@param valy A vector of dependent data the kitchen sink models will use
#'  to validate predictions. valy[i] should correspond to valx[i,]
#'@param featuresweep A vector of feature counts to sweep across.
#'@param windowsweep A vector of window sizes to sweep across.
#'@param verbose Print progress.
#'@param show.plot Draw plots of predicted values versus true for each model.
#'@param ... Arguments to be passed to \code{\link{kitchen_sink}}().
#'
#'@return Returns a matrix of the adjusted R^2 values from the linear models
#' lm(valy ~ predictions(valx)) for each model. Index [f,w] returns
#' R^2 of the model for featuresweep[f] and windowsweep[w].
#'
#' @importFrom graphics abline
#' @importFrom stats lm predict
#'
#'@seealso \code{\link{make_norms}}()
#'
#'   \code{\link{boot_kitchen}}
#'
#'   \code{\link{kitchen_sink}}()
#'
#' @examples
#' x <- matrix(sample(1:10,1000,T),200,5)
#' y <- x[,1]*x[,2]^2-0.5*x[,3]*x[,4]+x[,5]*x[,1]*x[,3]-x[,3]^2*x[,2]
#'
#' kitchen_sweep(trainx = x[1:100,],
#' trainy = y[1:100],
#' valx = x[101:200,],
#' valy = y[101:200],
#' featuresweep = c(2^(4:7),2^11,4000,6000,7000,2^13),
#' windowsweep = 2:5,
#' verbose = T,
#' ncores=2)
#'
#' @author Avery Kruger
#'
#' @export

kitchen_sweep <- function(
  trainx,
  trainy,
  valx,
  valy,
  featuresweep,
  windowsweep,
  clampoutliers = TRUE,
  verbose = FALSE,
  show.plot = FALSE,
  seed = NULL,
  partial_files = NULL,
  ...

){
  if(is.data.frame(trainx)){
    trainx <- as.matrix(trainx)
  }
  if(is.data.frame(valx)){
    valx <- as.matrix(valx)
  }
  if(is.list(trainy)){
    trainy <- unlist(trainy)
  }
  if(is.list(valy)){
    trainy <- unlist(valy)
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

  if(!is.null(seed)){
    global.seed <- .Random.seed
    set.seed(seed)
    on.exit(.Random.seed <<- global.seed)
    }
  mynorms <- make_norms(featuresweep, windowsweep)

  if(!is.null(partial_files)){
    list.files(pattern = partial_files)
  }

  for(f in 1:length(featuresweep)){
    for(w in 1:length(windowsweep)){
      if(verbose){
        print(paste0(length(windowsweep)*(f-1)+w,"/",
                     length(featuresweep)*length(windowsweep),
                     " Making Kitchen Sinks"))}
      nonlintrain <- kitchen_sink(trainx, mynorms[[f]][[w]], ...)
      nonlinpredict <- kitchen_sink(valx, mynorms[[f]][[w]], ...)
      #x <- nonlinx

      if(verbose){
        print(paste0(length(windowsweep)*(f-1)+w,"/",
                     length(featuresweep)*length(windowsweep),
                     " Running Ridge Regression"))}
      ridge_model <- glmnet::cv.glmnet(nonlintrain, trainy, alpha = 0)
      if(verbose){
        print(paste0(length(windowsweep)*(f-1)+w,"/",
                     length(featuresweep)*length(windowsweep),
                     " Predicting"))}
      y_predicted <- predict(ridge_model$glmnet.fit,
                             s = ridge_model$lambda.min,
                             newx = nonlinpredict)
      if(clampoutliers){
        y_predicted <- clamp(clamp(y_predicted,
                                   value = min(trainy)),
                             value = max(trainy), logic = `>`)
      }

      if(show.plot){plot(valy, y_predicted,
                    main=paste0(featuresweep[f]," Features, ",
                                windowsweep[w], " Window Size"))
        abline(0, 1)}

      valmodel <- lm(valy ~ y_predicted)
      allr2[f,w] <- summary(valmodel)$adj.r.squared
      if(verbose){
        print(paste0(length(windowsweep)*(f-1)+w,"/",
                     length(featuresweep)*length(windowsweep),
                     " Complete"))
      }
    }
  }
  allr2
}

