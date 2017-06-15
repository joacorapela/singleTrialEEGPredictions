computeDeviationFromMeanPhaseForERPI<- function(phaseERPImage) {
    deviationFromMeanPhaseERPImage <- matrix(NaN, nrow=nrow(phaseERPImage),
                                                  ncol=ncol(phaseERPImage))
    meanDirections <- rep(NaN, nrow(phaseERPImage))
    for(frame in 1:nrow(phaseERPImage)) {
        res <- computeDeviationFromMeanPhaseForPhases(phases=
                                                       phaseERPImage[frame,])
        deviationFromMeanPhaseERPImage[frame,] <- res$deviations
        meanDirections[frame] <- res$meanDirection
    }
    return(list(deviationFromMeanPhaseERPImage=deviationFromMeanPhaseERPImage,
                 meanDirections=meanDirections))
}
