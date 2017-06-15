preProcessPhaseERPI <- function(phaseERPImage, times, sfpds, sfpdsOutliers,
                                               srate, clipFrom, clipTo,
                                               alignmentSignificance,
                                               significantTimes=NULL,
                                               tolSignificantTimes=1e-4,
                                               plotSteps=FALSE,
                                               averageTrialsWinSize=101,
                                               plotsFilenamePattern=NA,
                                               width=6, height=6, 
                                               xlim=NA,
                                               zlim=NA) {
    res <- clipERPI(erpImage=phaseERPImage, 
                     times=times,
                     clipFrom=clipFrom,
                     clipTo=clipTo,
                     plotSteps=plotSteps,
                     sfpds=sfpds,
                     averageTrialsWinSize=averageTrialsWinSize,
                     plotsFilenamePattern=plotsFilenamePattern,
                     width=width, 
                     height=height, 
                     xlim=xlim,
                     zlim=zlim, 
                     figDescriptor='phase-aligned-clipped',
                     plotCueNTsDelays=-1)
    clippedERPI <- res$erpImage
    clippedTimes <- res$times

    itc <- computeITCFromPhaseERPImage(phaseERPImage=clippedERPI)

    if(!is.nan(alignmentSignificance)) {
        alignmentPValues <- testERPImageForRandomITC(phaseERPImage=clippedERPI)
        signIndices <- which(alignmentPValues<alignmentSignificance)
    } else {
        if(length(significantTimes)==0) {
            warning("Specify alignmentSignificance>0 or provide significantTimes")
            return(list())
        }
        signIndices <- getIndicesOfValuesInVector(values=significantTimes,
                                                   vector=clippedTimes,
                                                   tol=tolSignificantTimes)
    }

    if(length(signIndices)<2) {
        return(list())
    }
    dPhaseRes <- computeDeviationFromMeanPhaseForERPI(phaseERPImage=clippedERPI)
    signClippedTimes <- clippedTimes[signIndices]
    signMeanDirection <- dPhaseRes$meanDirections[signIndices]
    signDevMeanPhaseERPI <- 
     dPhaseRes$deviationFromMeanPhaseERPImage[signIndices,]
    signITC <- itc[signIndices]

    return(list(erpImage=signDevMeanPhaseERPI, 
                 times=signClippedTimes, 
                 meanDirection=signMeanDirection,
                 itc=signITC,
                 removedCasesIndices=list(sfpdsOutliers=sfpdsOutliers)))
}
