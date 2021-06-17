#convolutional sink function
#This function takes a set of data, a normal matrix.
#Number of features is determined by the number of columns in the normal matrix
#Window size is determined by the number of rows in the normal matrix.
convsink <- function(data, Norm){
  nfeatures <- ncol(Norm)
  window <- nrow(Norm)
  mymatrix <- matrix(nrow=nrow(data),ncol=nfeatures)
  for(i in 1:nrow(data)){
    conv <- t(sapply(1:(ncol(data)-window+1),
                     function(w){data[i,w:(w+window-1)]}))
    normconv <- as.matrix(conv) %*% Norm
    normconv[normconv < 0] <- 0 #clamp
    mymatrix[i,] <- colMeans(normconv)
  }
  mymatrix
}
#convsink(data=rbind(1:10,2:11),Norm=NULL,window=5, nfeatures = 16)

#This function generates a list of normal matrices of proper dimensions.
#The first index returns matrices of feature length i
#The second index returns matrices of window length j
  #E.g. myNorm[[2]][[4]] will return a normal matrix 
  #with featuresweep[2] features and windowsweep[4] window length
generateNorms <- function(featuresweep, windowsweep){
  normList <- list()
  findex <- 1:length(featuresweep)
  windex <- 1:length(windowsweep)
  for(f in findex){
    normList[[f]] <- list()
    for(w in windex){
      nfeatures <- featuresweep[f]
      window <- windowsweep[w]
      normList[[f]][[w]] <- matrix(rnorm(nfeatures*window,mean=0,sd=1), window, nfeatures)
    }
  }
  normList
}

#Generates a single normal matrix
normalMatrix <- function(data,nfeatures){
  if(!is.null(ncol(data))){x <- ncol(data)} else{x <- data}
  matrix(rnorm(nfeatures*x,mean=0,sd=1), x, nfeatures)
}

# testdata <- rbind(runif(10),runif(10),runif(10),runif(10))
# featuresweep <- 2^(3:5)
# windowsweep <- 3:10
# testNorms <- generateNorms(featuresweep,windowsweep)
# convsink(testdata, testNorms[[2]][[2]])
