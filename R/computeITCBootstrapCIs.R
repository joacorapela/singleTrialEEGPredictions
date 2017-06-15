computeITCBootstrapCIs <- function(timeFreqs, nResamples, conf) {
    bootStatistic <- function(data, indices, nFrames, nFreqs) {
        iteration <<- iteration + 1
        show(sprintf('Resample %d (out of %d)', iteration, nResamples))

        bootTimeFreqs <- array(data=t(data[indices,]), 
                                dim=c(nFrames, nFreqs, nrow(data)))
        itc <- computeITCFromTimeFreqs(timeFreqs=bootTimeFreqs)
        return(as.vector(itc))
    }

    nFrames = dim(timeFreqs)[1]
    nFreqs = dim(timeFreqs)[2]
    nTrials = dim(timeFreqs)[3]
    data <- t(matrix(data=timeFreqs, ncol=nTrials))

    iteration <- 0
    system.time(resBoot <- boot(data=data, statistic=bootStatistic, 
                                                R=nResamples, 
                                                nFrames=nFrames, 
                                                nFreqs=nFreqs))
    itcCIs <- getBootstrapCIs(bootRes=resBoot, conf=conf)
    return(itcCIs)
}

