bootstrapITCFromPhases <- function(phases, nResamples) {
    bootStatistic <- function(data, indices, nFrames, nFreqs) {
        itc <- computeITCFromPhases(phases=data[indices])
        return(as.vector(itc))
    }

    system.time(resBoot <- boot(data=phases, statistic=bootStatistic, 
                                             R=nResamples))
    return(resBoot)
}

