prepareDataAndComputeAmplitudeAndPhaseERPImages <- function(freq,
                                                             nCycles,
                                                             trials, 
                                                             times, 
                                                             srate,
                                                             minTime=min(times),
                                                             maxTime=max(times)) {
    selectedTimesIndices <- which(minTime<=times & times<=maxTime)
    times <- times[selectedTimesIndices]
    trials <- trials[selectedTimesIndices,]

    show(sprintf("Performinig cgt between %.2f ms and %.2f ms for %d time points", min(times), max(times), length(times)))

    res <- computeAmplitudeAndPhaseERPImagesCGT(freq=freq, nCycles=nCycles, 
                                                           trials=trials, 
                                                           srate=srate)
    return(c(list(times=times), res))
}
