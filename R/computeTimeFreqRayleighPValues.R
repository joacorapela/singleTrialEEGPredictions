computeTimeFreqRayleighPValues <- function(timeFreqs) {
    nFrames = dim(timeFreqs)[1]
    nFreqs = dim(timeFreqs)[2]
    nTrials = dim(timeFreqs)[3]
    pValues <- matrix(NaN, nrow=nFrames, ncol=nFreqs)
    for(i in 1:nFrames) {
        if(i%%10==0) {
            show(sprintf("Processing frame %d (out of %d)", i, nFrames))
        }
        for(j in 1:nFreqs) {
            pValues[i, j] <- rayleighUniformityTest(timeFreqs[i, j, ])
        }
    }
    return(pValues)
}

