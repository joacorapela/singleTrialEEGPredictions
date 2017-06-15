

computeDeviationFromMeanPhaseForPhases <- function(phases) {
    deviations <- array(NaN, dim=length(phases))
    meanDirection <- computeMeanDirection(angles=phases)
    for(i in 1:length(phases)) {
        phase <- phases[i]
        deviation <- computeCircularVariance(angles=c(phase, meanDirection))
        deviations[i] <- deviation 
    }
    return(list(meanDirection=meanDirection, deviations=deviations))
}
