computeITCFromPhaseERPImage <- function(phaseERPImage) {
    nFrames = nrow(phaseERPImage)
    nTrials = ncol(phaseERPImage)
    itc <- rep(0, times=nFrames)
    for(i in 1:nTrials) {
        if(i%%100==0) {
            show(sprintf("Processed %d trials (%d)", i, nTrials))
        }
        itc <- itc + complex(modulus=1, argument=phaseERPImage[,i])
    }
    itc <- Mod(itc)/nTrials
    return(itc)
}

