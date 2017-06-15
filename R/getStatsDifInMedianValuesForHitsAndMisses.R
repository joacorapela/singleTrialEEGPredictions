getStatsDifInMedianValuesForHitsAndMisses <- function(rts, maxRT,
                                                       dfpds, maxDFPD,
                                                       values, maxValue,
                                                       conf, nResamples,
                                                       minN) {
    validIndices <- which(!is.nan(rts) & 
                          !is.nan(dfpds) & dfpds<maxDFPD &
                          !is.nan(values) & values<maxValue)
    rts <- rts[validIndices]
    dfpds <- dfpds[validIndices]
    values <- values[validIndices]
    rts[rts>maxRT] <- Inf

    hitsIndices <- which(is.finite(rts))
    missesIndices <- which(is.infinite(rts))
    if(length(missesIndices)<minN || length(hitsIndices)<minN) {
        return(list())
    }
    hitValues <- values[hitsIndices]
    missValues <- values[missesIndices]
    bootRes <- bootstrapDifferenceMedians(x=hitValues, y=missValues,
                                                       nResamples=nResamples)
    bootCI <- getBootstrapCIs(bootRes=bootRes, conf=conf)
    permRes <- permuteDifferenceMedians(x=hitValues, y=missValues, 
                                                     nResamples=nResamples)
    permCI <- getBootstrapCIs(bootRes=permRes, conf=conf)
    pValue <- sum(abs(permRes$t)>abs(permRes$t0))/length(permRes$t)
    return(list(bootCI=bootCI, permCI=permCI, pValue=pValue))
}
