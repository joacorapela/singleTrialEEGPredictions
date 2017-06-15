

computeAvgDMPDiffsLateMinusEarlyTrials <- function(dmps, 
                                                    sfpds, 
                                                    earlyFromPercentage,
                                                    earlyToPercentage,
                                                    lateFromPercentage,
                                                    lateToPercentage) {
    nTrials <- ncol(dmps)
    earlyFromTrial <- round(nTrials*earlyFromPercentage+1)
    earlyToTrial <- round(nTrials*earlyToPercentage)
    lateFromTrial <- round(nTrials*lateFromPercentage+1)
    lateToTrial <- round(nTrials*lateToPercentage)
    sortRes <- sort(sfpds, index.return=TRUE)
    sortedDMPs <- dmps[,sortRes$ix]
    earlyMeans <- rowMeans(sortedDMPs[,earlyFromTrial:earlyToTrial])
    lateMeans <- rowMeans(sortedDMPs[,lateFromTrial:lateToTrial])
    return(lateMeans-earlyMeans)
}
