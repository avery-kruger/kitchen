exponents <- data.frame(matrix(0, 10,10))
covariates <- round(runif(10, -10, 10),1)
for(i in 1:10){
  num <- sample(1:4,1)
  myvars <- sample(1:10, num)
  mypowers <- sample(c(0.5,1,2,3), num, replace = T)
  exponents[i,myvars] <- mypowers
}

ndata <- 1000
data <- round(matrix(runif(ndata*10*2, 0, 100), ncol=10),1)
y <- apply(data,1,function(dat){
  expdat <- dat^exponents
  sum(apply(expdat,1,prod)*covariates)
  })
for(i in 1:10){plot(y ~ data[,i], main = i)}

myeq <- gsub("\\^1","",sapply(seq(nrow(exponents)),function(i){
  expo <- exponents[i,]
  mycov <- covariates[i]
  ind <- which(expo > 0)
  paste(c(mycov,paste0("(",paste0(letters[ind],'^',expo[ind]),
                       ")")),collapse = "*")
}))
myeq[length(myeq)] <- gsub("\\+","",myeq[length(myeq)])
plot.new()
for(i in 1:length(myeq)){
  mywrite <- str2expression(myeq[i])
  text(0.3,1.1-(0.1*i),bquote(.(mywrite)),cex=0.6)
}

myequation <- paste(myeq,collapse=" + ")
myequation <- gsub("\\^1","",myequation)
myequation

featuresweep <- 2^(c(6:9))
windowsweep <- c(5,10)

mysweep <- kitchen_sweep(data[1:(ndata/2),],
                         y[1:(ndata/2)],
                         data[(ndata/2+1):ndata,],
                         y[(ndata/2+1):ndata],
                         featuresweep,
                         windowsweep, verbose = T,
                         show.plot=T,
                         FUN = clamp)
bestfeature <- featuresweep[which(mysweep == max(mysweep), arr.ind = T)[1]]
bestwindow <- windowsweep[which(mysweep == max(mysweep), arr.ind = T)[2]]

kitchenpred <- kitchen_prediction(data[1:(ndata/2),],
                                  y[1:(ndata/2)],
                                  data[(ndata/2+1):ndata,],
                                  bestfeature,
                                  bestwindow,
                                  1,
                                  FUN = clamp)

mymod <- lm(y[1:(ndata/2)] ~ ., as.data.frame(data[1:(ndata/2),]))
mypred <- predict(mymod, as.data.frame(data[(ndata/2+1):ndata,]))
summary(lm(y[(ndata/2+1):ndata] ~ mypred))
plot(y[(ndata/2+1):ndata], mypred); abline(0, 1)
plot(y[(ndata/2+1):ndata], unlist(kitchenpred)); abline(0, 1)


##complicated version
funs <- list(`^`,`log`,`cos`)
funs.names <- c("^","log","cos")

myequation.parts <- sapply(1:10, function(j){
  num <- sample(1:3,1)
  myvars <- sample(letters[1:10], num)
  myfuns <- sample(1:3,num,replace = T)
  mypowers <- sample(c(0.5,1,2), num, replace = T)
  paste0(c(covariates[j],
                      sapply(seq(myvars), function(i){
                        if(myfuns[i] == 1){paste0("(",myvars[i],"",
                                                  funs.names[myfuns[i]],
                                                  mypowers[i],")")
                        } else{paste0("(",funs.names[myfuns[i]],"(",myvars[i],"))")
                        }
                      })),collapse="*")
})
myequation <- paste0(myequation.parts[1], "+",
                     myequation.parts[2], "+",
                     myequation.parts[3], "+",
                     myequation.parts[4], "+",
                     myequation.parts[5], "+",
                     myequation.parts[6], "+",
                     myequation.parts[7], "+",
                     myequation.parts[8], "+",
                     myequation.parts[9], "+",
                     myequation.parts[10])

data
y <- sapply(seq(nrow(data)),function(i){
  eval(str2expression(myequation),
       list(a=data[i,1],
            b=data[i,2],
            c=data[i,3],
            d=data[i,4],
            e=data[i,5],
            f=data[i,6],
            g=data[i,7],
            h=data[i,8],
            i=data[i,9],
            j=data[i,10]))
})
which.inf <- which(is.infinite(y))
data <- data[-which.inf,]
y <- y[-which.inf]
ndata <- ceiling(nrow(data)/2)
mysweep <- kitchen_sweep(data[1:(ndata),],
                         y[1:(ndata)],
                         data[(ndata+1):nrow(data),],
                         y[(ndata+1):nrow(data)],
                         featuresweep,
                         windowsweep, verbose = T,
                         show.plot=T,
                         FUN = clamp)
mymod <- lm(y[1:(ndata)] ~ ., as.data.frame(data[1:(ndata),]))
mypred <- predict(mymod, as.data.frame(data[(ndata+1):nrow(data),]))
summary(lm(y[(ndata+1):nrow(data)] ~ mypred))
plot(y[(ndata+1):nrow(data)], mypred); abline(0, 1)
