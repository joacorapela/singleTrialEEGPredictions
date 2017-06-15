performMorletWTSingleTrial <- function(trial, noctave=5, nvoice=12,
                                              w0=2*pi, plot=TRUE) {
    cwt <- cwt(trial, noctave=noctave, nvoice=nvoice, w0=w0, plot=plot)
    return(cwt)
}
