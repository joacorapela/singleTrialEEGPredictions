removeEarlyOrLateCasesWithMissingEEGFromERPI <- function(erpImage, 
                                                          cueNTsDelays, 
                                                          times, 
                                                          clipFrom, 
                                                          clipTo) {
    maxDelay <- max(times)-clipTo
    minDelay <- min(times)-clipFrom
    removedCasesIndices <- which(cueNTsDelays<minDelay | maxDelay<cueNTsDelays)
    if(length(removedCasesIndices)>0) {
        noExtremesERPI <- erpImage[, -removedCasesIndices]
    } else {
        noExtremesERPI <- erpImage
    }
    return(list(removedCasesIndices=removedCasesIndices,
                 noExtremesERPI=noExtremesERPI))
}

