regularizedPredictionsByKFoldCrossvalidation <- function(y, x, nGroups,
                                                               lambda,
                                                               verbose=FALSE) {
    if(is.na(nGroups) || nGroups>length(y)) {
        if(verbose) {
            show("Using leave-one-out cross validation")
        }
        nGroups <- length(y)
    }
    theta.fit <- function(x,y, lambda) {
        coefs <- coefsRidgeRegression(x=x, y=y, lambda=lambda)
        return(coefs)
    }
    theta.predict <- function(coefs,x) {
        predictions <- x%*%coefs
        return(predictions)
    }
    predictions <- crossval(x=x, y=y, 
                                 theta.fit=theta.fit, 
                                 theta.predict=theta.predict,
                                 lambda=lambda,
                                 ngroup=nGroups)$cv.fit
    return(predictions)
}
