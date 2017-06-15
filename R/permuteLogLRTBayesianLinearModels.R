permuteLogLRTBayesianLinearModels <- function(fullModel, reducedModel, 
                                                         y,
                                                         xFullModel, 
                                                         xReducedModel,
                                                         nResamples) {
    computeLogLRT <- function(y, i, xFullModel, xReducedModel) {
        lambdaFullModel <- fullModel$cN/fullModel$dN
        coefsFullModel <- coefsRidgeRegression(x=xFullModel, 
                                                y=y[i], 
                                                lambda=lambdaFullModel)
        lambdaReducedModel <- reducedModel$cN/reducedModel$dN
        coefsReducedModel <- coefsRidgeRegression(x=xReducedModel, 
                                                   y=y[i], 
                                                   lambda=lambdaReducedModel)
        logLRT <- 2*(log(fullModel$aN/fullModel$bN)-
                      fullModel$aN/(2*fullModel$bN)*
                       l2Norm2(y-xFullModel%*%coefsFullModel))-
                  2*(log(reducedModel$aN/reducedModel$bN)-
                      reducedModel$aN/(2*reducedModel$bN)*
                       l2Norm2(y-xReducedModel%*%coefsReducedModel))
        return(logLRT)
    }
    bootRes <- boot(data=y, statistic=computeLogLRT,
                            sim="permutation", R=nResamples,
                            xFullModel=xFullModel, 
                            xReducedModel=xReducedModel)
    return(bootRes)
}
