computeAmplitudeAndPhaseERPImagesCGT <- function(freq, nCycles, trials, srate) {
    amplitudeERPImage <- c()
    phaseERPImage <- c()
    scale <- nCycles*srate/(2*pi*freq)
    show(sprintf("scale=%.2f", scale))
    for(i in 1:ncol(trials)) {
        if(i%%100==0) {
            show(sprintf("Processed %d trials", i))
        }
        df <- freq/srate
        cgtRes <- cgt(input=trials[,i], nvoice=1, freqstep=2*freq/srate, 
                                        scale=scale, plot=FALSE)
        amplitudeERPImage <- cbind(amplitudeERPImage, Mod(cgtRes))
        phaseERPImage <- cbind(phaseERPImage, Arg(cgtRes))
    }
    return(list(amplitudeERPImage=amplitudeERPImage,
                 phaseERPImage=phaseERPImage))
}
