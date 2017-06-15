computeITCFromTimeFreqs <- function(timeFreqs) {
    nFrames = dim(timeFreqs)[1]
    nFreqs = dim(timeFreqs)[2]
    nTrials = dim(timeFreqs)[3]
    itc <- matrix(0, nrow=nFrames, ncol=nFreqs)
    for(i in 1:nTrials) {
        if(i%%100==0) {
            show(sprintf("Processed %d trials (%d)", i, nTrials))
        }
        timeFreq <- timeFreqs[,,i]
        argTimeFreq <- Arg(timeFreq)
        itc <- itc + complex(modulus=1, argument=argTimeFreq)
    }
    itc <- Mod(itc)/nTrials
    return(itc)
}

