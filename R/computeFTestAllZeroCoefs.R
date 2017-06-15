computeFTestAllZeroCoefs <- function(y, residuals, nRegressors) {
# computeFTestAllZeroCoefs <- function(lmRes) {
    # y <- lmRes$model$y
    n <- length(y)

    ssto <- sum(y^2)-n*mean(y)^2
    # sseF <- sum(lmRes$residuals^2)
    sseF <- sum(residuals^2)
    # nRegressors <- length(lmRes$coefficients)

    fValue <- ((ssto-sseF)/(nRegressors-1))/(sseF/(n-nRegressors))
    dfNum <- nRegressors-1
    dfDen <- n-nRegressors
    pValue <- pf(q=fValue, df1=dfNum, df2=dfDen, lower.tail=FALSE)
    return(list(fValue=fValue, dfNum=dfNum, dfDen=dfDen, pValue=pValue))
}
