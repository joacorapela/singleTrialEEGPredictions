performMorletWTSingleTrials <- function(trials, noctave, nvoice, w0) {
    nTrials <- ncol(trials)
    nFrames <- nrow(trials)
    nFreqs <- noctave*nvoice

    timeFreqs <- array(data=NA, dim=c(nFrames, nFreqs, nTrials))
    for(i in 1:ncol(trials)) {
        if(i%%10==0) {
            show(sprintf("Processed %d trials (%d)", i, ncol(trials)))
        }
        timeFreq <- performMorletWTSingleTrial(trial=trials[,i], 
                                                                                                noctave=noctave, 
                                                                                                                                                nvoice=nvoice,
                                                                                                                                                w0=w0, 
                                                                                                                                                                                                plot=FALSE)
        timeFreqs[,,i] <- timeFreq
    }
    return(timeFreqs)
}

