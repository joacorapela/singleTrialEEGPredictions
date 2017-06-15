randomizeSTDsAndPreProcessERPI <- function(
 phaseERPImage,
 clipFrom,
 clipTo,
 srate,
 significantTimes,
 stdsNextDsLatenciesInfo,
 stdsNextCuesLatenciesInfo,
 stdsNextEOBsLatenciesInfo,
 plotSteps,
 averageTrialsWinSize,
 plotsFilenamePattern,
 width, height,
 xlim, zlim) {
    shifts <- getRandomShifts(
               epochEventIDs=phaseERPImage$epochEventIDs,
               sfpds=phaseERPImage$sfpds,
               epochStart=min(phaseERPImage$times),
               epochEnd=max(phaseERPImage$times),
               clipTo=clipTo,
               stdsNextDsLatenciesInfo=stdsNextDsLatenciesInfo,
               stdsNextCuesLatenciesInfo=stdsNextCuesLatenciesInfo,
               stdsNextEOBsLatenciesInfo=stdsNextEOBsLatenciesInfo)
    erpImageRandomizedNTs <-
     alignERPImageToRandomSFPDs(erpImage=phaseERPImage$phaseERPImage,
                                 times=phaseERPImage$times, 
                                 sfpds=phaseERPImage$sfpds,
                                 shifts=shifts,
                                 srate=srate)
    rSFPDs <- phaseERPImage$sfpds+shifts
    preProcessedERPI <- 
     preProcessPhaseERPI(phaseERPImage=erpImageRandomizedNTs,
                          times=phaseERPImage$times, 
                          sfpds=rSFPDs,
                          sfpdsOutliers=phaseERPImage$sfpdsOutliers,
                          srate=srate, 
                          clipFrom=clipFrom, clipTo=clipTo, 
                          alignmentSignificance=NaN,
                          significantTimes=significantTimes,
                          plotSteps=plotSteps, 
                          averageTrialsWinSize=averageTrialsWinSize, 
                          plotsFilenamePattern=plotsFilenamePattern, 
                          width=width, height=height, xlim=xlim, zlim=zlim)
    preProcessedERPI <- c(preProcessedERPI, 
                           list(sfpds=rSFPDs,
                                 epochEventIDs=
                                  phaseERPImage$epochEventIDs,
                                 peakITCFreq=
                                  phaseERPImage$peakITCFreq))
    return(preProcessedERPI)
}

