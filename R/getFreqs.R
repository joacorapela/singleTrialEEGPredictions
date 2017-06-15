
getFreqs <- function(noctave, nvoice, w0, srate) {
    scales <- getScales(noctave=noctave, nvoice=nvoice)
    freqs <- w0*srate/(2*pi*scales)
    return(freqs)
}

