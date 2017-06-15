
getLowCoherenceTimesBlocks <- function(times, epochStartTime, epochEndTime,
                                              srate) {
    timeStep <- 1000/srate
    allTimes <- seq(from=epochStartTime, to=epochEndTime, by=1000/srate)
    lowCoherenceTimes <- allTimes[is.na(match(allTimes, times))]
    if(length(lowCoherenceTimes)==0) {
        return(NA)
    }
    blockStartTime <- lowCoherenceTimes[1]
    lowCoherenceTimeBlocks <- c()
    if(length(lowCoherenceTimes)>1) {
        for(i in 2:length(lowCoherenceTimes)) {
            if(lowCoherenceTimes[i]!=lowCoherenceTimes[i-1]+timeStep) {
                lowCoherenceTimeBlocks <- rbind(lowCoherenceTimeBlocks,
                                                 c(blockStartTime,
                                                    lowCoherenceTimes[i-1]))
                blockStartTime <- lowCoherenceTimes[i]
            }
        }
    }
    lowCoherenceTimeBlocks <- rbind(lowCoherenceTimeBlocks, 
                                     c(blockStartTime, 
                                        lowCoherenceTimes[length(lowCoherenceTimes)]))
    return(lowCoherenceTimeBlocks)
}
