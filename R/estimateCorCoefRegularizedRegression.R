
estimateCorCoefRegularizedRegression <- function(erpImage, 
                                                  minSFPD,
                                                  maxSFPD,
                                                  lambda, 
                                                  order, 
                                                  scaleData, 
                                                  nGroups, 
                                                  a0, b0, c0, d0, 
                                                  maxIter,
                                                  convergenceTol,
                                                  interactions=FALSE, 
                                                  plotERPImages=FALSE, 
                                                  averageTrialsWinSize=NaN, 
                                                  zlim=NA,
                                                  verbose=TRUE) {

    prepareRes <- prepareDataRegularizedRegression(erpImage=erpImage, 
                                                    minSFPD=minSFPD,
                                                    maxSFPD=maxSFPD,
                                                    lambda=lambda,
                                                    order=order,
                                                    scaleData=scaleData, 
                                                    interaction=interactions,
                                                    plotERPImages=
                                                     plotERPImages, 
                                                    averageTrialsWinSize=
                                                     averageTrialsWinSize, 
                                                    zlim=zlim, 
                                                    verbose=verbose)
    if(!fieldExistsInList(fieldName="data", list=prepareRes)) {
        return(NaN)
    }
    data <- prepareRes$data
    x <- cbind(1, data[, -1])
    y <- data[, 1]
    variationalLM <- variationalLinearRegression(t=y,
                                                  phi=x,
                                                  a0=a0, b0=b0, 
                                                  c0=c0, d0=d0,
                                                  maxIter=maxIter,
                                                  convergenceTol=
                                                   convergenceTol)
    regularizationParam <- variationalLM$cN/variationalLM$dN

    predictions <- 
     regularizedPredictionsByKFoldCrossvalidation(y=y, x=x,
                                                   lambda=regularizationParam,
                                                   nGroups=nGroups)
    scorRes <- scor(x=y, y=predictions, plotit=FALSE)
    return(scorRes$cor.value)
}
