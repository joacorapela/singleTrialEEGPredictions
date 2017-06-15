# From Walker and Birch 1988
# Influence Measures in Ridge Regression
# Technometrics, 30(2): 221-227

cooksDistanceRidgeRegression <- function(y, x, lambda) {
    p <- ncol(x)
    n <- nrow(x)
    predictionsLM <- lm(y~x, data=data.frame(y=y, x=x))$fitted.values
    errorsLM <- y-predictionsLM
    s2 <- l2Norm2(errorsLM)/(n-p)
    coefsRR <- coefsRidgeRegression(x=x, y=y, lambda=lambda)
    predictionsRR <- x%*%coefsRR
    ps2 <- p*s2
    txx <- t(x)%*%x
    ds <- c()
    for(i in 1:length(y)) {
        caseRemovedCoefsRR <- coefsRidgeRegression(x=x[-i,], y=y[-i], 
                                                             lambda=lambda)
        casesRemovedPredictionsRR <- x%*%caseRemovedCoefsRR
        diffPredictions <- predictionsRR-casesRemovedPredictionsRR
        di <- 1/ps2*l2Norm2(diffPredictions)
        ds <- c(ds, di)
    }
# iMaxD <- which.max(ds)
# caseRemovedCoefsRR <- coefsRidgeRegression(x=x[-iMaxD,], y=y[-iMaxD], lambda=lambda)
# matplot(cbind(coefsRR, caseRemovedCoefsRR))
# browser()
    return(ds)
}
