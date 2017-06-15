alignERPI <- function(erpImage, times, cueNTsDelays, srate, plotSteps, 
                                averageTrialsWinSize, plotsFilenamePattern, 
                                width, height, xlim, zlim, figDescriptor) {
    alignTime <- median(cueNTsDelays)  
    alignedTimes <- times-alignTime
    alignedERPI <- alignData(data=erpImage, alignTime=alignTime, 
                                            sortvar=cueNTsDelays, 
                                            srate=srate)$alignedData
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
        plotERPImage(times=alignedTimes, trials=alignedERPI, 
                                  cueNTsDelays=cueNTsDelays, 
                                  plotCueNTsDelays=-1, 
                                  main=figDescriptor,
                                  averageTrialsWinSize=averageTrialsWinSize,
                                  xlim=xlim,
                                  zlim=zlim)
        if(!is.na(plotsFilenamePattern)) {
            dev.off()
        }
    }
    return(list(alignedERPI=alignedERPI, alignedTimes=alignedTimes))
}
