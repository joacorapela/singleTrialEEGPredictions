analyzeDataRegularizedRegression <- function(erpImage, 
                                              minSFPD,
                                              maxSFPD,
                                              lambda, 
                                              order, 
                                              scaleData, 
                                              nGroups, 
                                              a0, b0, c0, d0, 
                                              computeCoefsCIs,
                                              maxIter,
                                              convergenceTol,
                                              nResamplesLRT=2000,
                                              nResamplesCoefs=2000, 
                                              nResamplesPredictions=2000, 
                                              confCIs=.95,
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
    if(is.null(prepareRes$data)) {
        return(prepareRes)
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

    if(nrow(x)>ncol(x)) {
        if(verbose) {
            show("Removing influential cases ...")
        }
        ds <- cooksDistanceRidgeRegression(y=y, x=x, lambda=regularizationParam)
        influentialIndices <- adjboxout(ds)$out.id
        if(!is.null(influentialIndices[1])) {
            x <- x[-influentialIndices,]
            y <- y[-influentialIndices]
            prepareRes$removedCasesIndices <- 
             c(prepareRes$removedCasesIndices, 
                list(influentialCases=
                      prepareRes$epochEventIDs[influentialIndices]))
            scale <- attr(prepareRes$data, "scaled:scale")
            center <- attr(prepareRes$data, "scaled:center")
            prepareRes$data <- prepareRes$data[-influentialIndices,]
            attr(prepareRes$data, "scaled:scale") <- scale
            attr(prepareRes$data, "scaled:center") <- center
            prepareRes$epochEventIDs <- 
             prepareRes$epochEventIDs[-influentialIndices]
        }
    }

    if(verbose) {
        show("Computing bootstrap test of significance ...")
    }
    lrtRes <- 
     lrtForBayesianLinearModel(model=variationalLM, x=x, y=y, 
                                                    a0=a0, b0=b0, c0=c0, d0=d0,
                                                    maxIter=maxIter,
                                                    convergenceTol=
                                                     convergenceTol, 
                                                    nResamples=nResamplesLRT)
    if(verbose) {
        show("Computing cross-validated predictions ...")
    }
    predictions <- 
     regularizedPredictionsByKFoldCrossvalidation(y=y, x=x,
                                                   lambda=regularizationParam,
                                                   nGroups=nGroups)
    if(verbose) {
        show("Bootstrapping correlation between predictions and SFPD ...")
    }
    resBootPredSFPDurCor <- bootstrapSkippedPearsonCorCoef(
                             x=y, y=predictions,
                             nResamples=nResamplesPredictions)
    if(verbose) {
        show("Computing CIs for correlation between predictions and SFPD ...")
    }
    predSFPDurCorCI <- getBootstrapCIs(bootRes=resBootPredSFPDurCor,
                                        conf=confCIs)

    if(computeCoefsCIs) {
        if(verbose) {
            show("Bootstraping coefficients  ...")
        }
        resBootCoefs <- bootstrapRegularizedCoefs(y=y, x=x, 
                                                   nResamples=nResamplesCoefs,
                                                   lambda=regularizationParam)
        if(verbose) {
            show("Computing CIs for coefficients ...")
        }
        coefsCIs <- getBootstrapCIs(bootRes=resBootCoefs, conf=confCIs)
    } else {
        coefsCIs <- NA
    }

    return(c(list(variationalLM=variationalLM, 
                   lrtRes=lrtRes,
                   predictions=predictions, 
                   predSFPDurCorCI=predSFPDurCorCI,
                   coefsCIs=coefsCIs),
              prepareRes))
}
