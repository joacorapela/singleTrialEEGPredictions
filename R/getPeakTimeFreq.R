getPeakTimeFreq <- function(timeFreq, times, freqs, fromTime, toTime, 
                                      fromFreq, toFreq) {
    timeIndices <- which(fromTime<=times & times<=toTime)
    freqIndices <- which(fromFreq<=freqs & freqs<=toFreq)
    timeFreqToSearch <- timeFreq[timeIndices, freqIndices]

    peakIndex <- which.max(timeFreqToSearch)
    peakValue <- timeFreqToSearch[peakIndex]

    peakTimeIndex <- ((peakIndex-1)%%length(timeIndices))+1
    peakTimeIndex <- timeIndices[peakTimeIndex]

    peakFreqIndex <- ((peakIndex-1)%/%length(timeIndices))+1
    peakFreqIndex <- freqIndices[peakFreqIndex]

    return(list(value=peakValue, 
                 time=times[peakTimeIndex], 
                 freq=freqs[peakFreqIndex],
                 timeIndex=peakTimeIndex,
                 freqIndex=peakFreqIndex))
}
