computeITCWithSignificanceMaskAndPeakCI <- function(times, trials, 
                                                     noctave, nvoice, w0, 
                                                     srate, 
                                                     peakFromTime, peakToTime,
                                                     peakFromFreq, peakToFreq,
                                                     nResamples,
                                                     significance=.01,
                                                     conf=.95) {
    timeFreqs <- performMorletWTSingleTrials(trials=trials, noctave=noctave, 
                                                            nvoice=nvoice, 
                                                            w0=w0)
    itc <- computeITCFromTimeFreqs(timeFreqs=timeFreqs)
    timeFreqRayleighPValues <- computeTimeFreqRayleighPValues(timeFreqs=
                                                               Arg(timeFreqs))
    significanceMask <- timeFreqRayleighPValues<significance

    freqs <- getFreqs(noctave=noctave, nvoice=nvoice, w0=w0, srate=srate)
    peakRes <- getPeakTimeFreq(timeFreq=itc, times=times, freqs=freqs, 
                                             fromTime=peakFromTime, 
                                             toTime=peakToTime, 
                                             fromFreq=peakFromFreq, 
                                             toFreq=peakToFreq)
    peakPhases <- Arg(timeFreqs[peakRes$timeIndex, peakRes$freqIndex, ])
    bootRes <- bootstrapITCFromPhases(phases=peakPhases, nResamples=nResamples)
    peakCI <- getBootstrapCIs(bootRes=bootRes, conf=conf)
    peakInfo <- c(peakRes, list(ci=peakCI))
    return(list(itc=itc, freqs=freqs, times=times,
                         significanceMask=significanceMask,
                         peakInfo=peakInfo))
}
