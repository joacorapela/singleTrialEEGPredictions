clipERPI <- function(erpImage, times, clipFrom, clipTo, plotSteps, sfpds,
                               averageTrialsWinSize, plotsFilenamePattern, 
                               width, height, xlim, zlim, figDescriptor, 
                               plotCueNTsDelays=-1) {
    clippedTimeIndices <- which(clipFrom<=times & times<=clipTo)
    clippedERPI <- erpImage[clippedTimeIndices,]
    clippedTimes <- times[clippedTimeIndices]
    if(plotSteps) {
        if(!is.na(plotsFilenamePattern)) {
            imageFilename <- sprintf(plotsFilenamePattern, figDescriptor,
                                                            'eps')
            trellis.device("postscript", width=width, height=height, 
                           onefile=FALSE, horizontal=FALSE, 
                           file=imageFilename)
        } else {
            X11()
        }
        plotERPImage(times=clippedTimes, trials=clippedERPI, 
                                         sfpds=sfpds, 
                                         plotCueNTsDelays=plotCueNTsDelays, 
                                         main=figDescriptor,
                                         averageTrialsWinSize=
                                          averageTrialsWinSize, 
                                         xlim=c(max(xlim[1], clipFrom),
                                                 min(xlim[2], clipTo)),
                                         zlim=zlim)
        if(!is.na(plotsFilenamePattern)) {
            dev.off()
        }
    }
    return(list(erpImage=clippedERPI, times=clippedTimes))
}
