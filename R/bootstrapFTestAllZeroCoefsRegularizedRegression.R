
# From Davidson and Hinkley 1997. Bootstrap methods and their applications.
# Section 6.3.2 Significance Tests

bootstrapFTestAllZeroCoefsRegularizedRegression <- function(y, x, lambda,
                                                                  nResamples) {
    bootFTest <- function(data, indices, allResiduals) {
        yAsterisk <- allResiduals[indices]
        x <- data[,-1]
        y <- data[,1]
        coefsRR <- coefsRidgeRegression(x=x, y=yAsterisk, lambda=lambda)
        predictions <- x%*%coefsRR
        residuals <- y-predictions
        resFTest <- computeFTestAllZeroCoefs(y=y,
                                              residuals=residuals,
                                              nRegressors=ncol(x))
        return(resFTest$fValue)
    }

    coefsRR <- coefsRidgeRegression(x=x, y=y, lambda=lambda)
    predictions <- x%*%coefsRR
    allResiduals <- y - predictions
    fObs <- computeFTestAllZeroCoefs(y=y,
                                      residuals=allResiduals,
                                      nRegressors=ncol(x))
    system.time(res <- boot(data=cbind(y, x), statistic=bootFTest, 
                                              R=nResamples, 
                                              allResiduals=allResiduals))
    ecdfRes <- stats:::ecdf(res$t[,1]) # Ralfuns overrides ecdf
    rankFObs <- nResamples*(1-ecdfRes(fObs))
    pValue <- rankFObs/nResamples
    return(list(pValue=pValue, fObs=fObs, bootRes=res))
}
