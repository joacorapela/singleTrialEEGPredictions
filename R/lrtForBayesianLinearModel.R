
lrtForBayesianLinearModel <- function(model, x, y, a0, b0, c0, d0, maxIter,
                                             convergenceTol, nResamples) {
    xReducedModel <- matrix(rep(1, times=length(y)), ncol=1)
    reducedModel <- variationalLinearRegression(t=y, 
                                                 phi=xReducedModel, 
                                                 a0=a0, b0=b0, c0=c0, d0=d0,
                                                 maxIter=maxIter,
                                                 convergenceTol=convergenceTol)
    permRes <- permuteLogLRTBayesianLinearModels(fullModel=model, 
                                                  reducedModel=reducedModel,
                                                  y=y,
                                                  xFullModel=x,
                                                  xReducedModel=xReducedModel,
                                                  nResamples=nResamples)
    pValue <- mean(permRes$t0<permRes$t)
    return(list(logLRT=permRes$t0, pValue=pValue))
}
