
alignERPImageToRandomSFPDs <- function(erpImage, times, sfpds, shifts, srate) {
    shiftsInSamples <- shifts*srate/1000
    rERPImage <- array(NA, dim=c(nrow(erpImage), ncol(erpImage)))
    nrows <- nrow(erpImage)
    for(i in 1:length(shiftsInSamples)) {
        if(shiftsInSamples[i]>0) {
            rERPImage[1:(nrows-shiftsInSamples[i]), i] <- 
             erpImage[(shiftsInSamples[i]+1):nrows, i]
        } else {
            rERPImage[(-shiftsInSamples[i]+1):nrows, i] <- 
             erpImage[1:(nrows+shiftsInSamples[i]), i]
        }
    }
    return(rERPImage)
}

