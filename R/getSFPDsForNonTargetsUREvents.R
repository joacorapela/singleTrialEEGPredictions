getSFPDsForNonTargetsUREvents <- function(sfpdsInfo, nonTargetsUREvents) {
    sfpds <- array(NaN, dim=length(nonTargetsUREvents))
    validIndices <- which(!is.nan(nonTargetsUREvents) & !is.na(nonTargetsUREvents))
    for(i in 1:length(validIndices)) {
        nonTargetUREvent <- nonTargetsUREvents[validIndices[i]]
        sfpdIndex <- which(sfpdsInfo[,1]==nonTargetUREvent)
        if(length(sfpdIndex)==0) {
            stop(sprintf("nonTargetUREvent=%d not found", nonTargetUREvent))
        }
        sfpds[validIndices[i]] = sfpdsInfo[sfpdIndex, 2]
    }
    return(sfpds)
}
