getStatsPredictionsErrorsVsRTs3 <- function(predictionErrors,
                                             rts,
                                             dfpds,
                                             maxRT,
                                             maxDFPD,
                                             nResamples, 
                                             conf,
                                            ...) {
    validIndices <- which(!is.nan(rts) & 
                          !is.nan(dfpds) & dfpds<maxDFPD)
    rts <- rts[validIndices]
    dfpds <- dfpds[validIndices]
    predictionErrors <- predictionErrors[validIndices]
    rts[rts>maxRT] <- Inf
    indicesDetectedTargets <- which(is.finite(rts))

    detectedTargetsRTs <- rts[indicesDetectedTargets]
    detectedTargetsDFPDs <- dfpds[indicesDetectedTargets]
    detectedTargetsPredictionsErrors <- predictionErrors[indicesDetectedTargets]
    statsCorCoef <- getStatsCorCoefRTsVsValues(
                      rts=detectedTargetsRTs,
                      maxRT=maxRT,  
                      dfpds=detectedTargetsDFPDs,
                      maxDFPD=maxDFPD,  
                      values=detectedTargetsPredictionsErrors,
                      maxValue=Inf,  
                      nResamples=nResamples,
                      conf=conf,
                      ...)
    return(list(cor=statsCorCoef$scorRes$cor.value,
                 statistic=statsCorCoef$scorRes$test.stat,
                 criterion=statsCorCoef$scorRes$crit.05, 
                 bootCI=statsCorCoef$bootCI,
                 permCI=statsCorCoef$permCI,
                 pValue=statsCorCoef$pValue))
}
