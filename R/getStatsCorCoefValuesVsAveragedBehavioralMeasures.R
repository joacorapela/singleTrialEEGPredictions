

getStatsCorCoefValuesVsAveragedBehavioralMeasures <- 
 function(values, behavioralMeasures, nResamples, conf, STAND=TRUE, ...) {
    mscorRes <- tryCatch({
        sink("/dev/null")
        # Fixing a problem in outpro when median(values)==0
        if(median(values)==0 || median(behavioralMeasures)==0) {
            STAND=FALSE
        }
        #
        mscorRes <- mscor(m=cbind(behavioralMeasures, values), STAND=STAND, ...)
    }, finally={
        sink()
    })
    bootRes <- bootstrapSkippedSpearmanCorCoef(x=behavioralMeasures, 
                                                y=values, 
                                                nResamples=nResamples, ...)
    if(!is.nan(bootRes$t0)) {
        bootCI <- getBootstrapCIs(bootRes=bootRes, conf=conf)
    } else {
        bootCI <- c(NaN, NaN, NaN)
    }
    permRes <- permuteSkippedSpearmanCorCoef(x=behavioralMeasures, 
                                              y=values, 
                                              nResamples=nResamples, ...)
    if(!is.nan(permRes$t0)) {
        permCI <- getBootstrapCIs(bootRes=permRes, conf=conf)
        pValue <- sum(abs(permRes$t)>abs(permRes$t0))/length(permRes$t)
    } else {
        permCI <- c(NaN, NaN, NaN)
        pValue <- NaN
    }

    return(list(cor=mscorRes$cor[1, 2], statistic=mscorRes$test.stat[1, 2],
                                        criterion=mscorRes$crit.val,
                                        bootCI=bootCI, permCI=permCI, 
                                        pValue=pValue))
}
