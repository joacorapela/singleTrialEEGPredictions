getStatsCorCoefRTsVsValues <- function(rts, maxRT, 
#                                         dfpds, maxDFPD, 
                                        values, maxValue, 
                                        nResamples, conf, plotit=FALSE) {
    validIndices <- which(!is.nan(rts) & rts<maxRT & 
#                           !is.nan(dfpds) & dfpds<maxDFPD &
                          !is.nan(values) & values<maxValue) 
                                       
    rts <- rts[validIndices]
    values <- values[validIndices]

    scorRes <- tryCatch({
        sink("/dev/null")
        scor(cbind(rts, values), plotit=plotit)
    }, finally={
        sink()
    })
    bootRes <- bootstrapSkippedPearsonCorCoef(x=rts, y=values, 
                                                     nResamples=nResamples)
    bootCI <- getBootstrapCIs(bootRes=bootRes, conf=conf)
    permRes <- permuteSkippedPearsonCorCoef(x=rts, y=values, 
                                                   nResamples=nResamples)
    permCI <- getBootstrapCIs(bootRes=permRes, conf=conf)
    pValue <- sum(abs(permRes$t)>abs(permRes$t0))/length(permRes$t)
    return(list(scorRes=scorRes, bootCI=bootCI, permCI=permCI, pValue=pValue))
}
