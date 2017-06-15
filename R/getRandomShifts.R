
getRandomShifts <- 
 function(epochEventIDs, sfpds, epochStart, epochEnd, clipTo,
           stdsNextDsLatenciesInfo, stdsNextCuesLatenciesInfo, 
           stdsNextEOBsLatenciesInfo) {
    sfpdsDF <- data.frame(epochEventID=epochEventIDs, sfpd=sfpds)
    stdsNextDsLatenciesInfoDF <- data.frame(epochEventID=
                                              stdsNextDsLatenciesInfo[,1],
                                             stdNextDLatency=
                                              stdsNextDsLatenciesInfo[,2])
    if(!is.na(stdsNextCuesLatenciesInfo[1])) {
        stdsNextCuesLatenciesInfoDF <- 
         data.frame(epochEventID=stdsNextCuesLatenciesInfo[,1],
                     stdNextCueLatency=stdsNextCuesLatenciesInfo[,2])
    } else {
        stdsNextCuesLatenciesInfoDF <- NA
    }
    stdsNextEOBsLatenciesInfoDF <- 
     data.frame(epochEventID=stdsNextEOBsLatenciesInfo[,1],
                 stdNextEOBLatency=stdsNextEOBsLatenciesInfo[,2])
    df <- merge(x=sfpdsDF, y=stdsNextDsLatenciesInfoDF, by="epochEventID")
    if(class(stdsNextCuesLatenciesInfoDF)=="data.frame") {
        df <- merge(x=df, y=stdsNextCuesLatenciesInfoDF, by="epochEventID")
    }
    df <- merge(x=df, y=stdsNextEOBsLatenciesInfoDF, by="epochEventID")
    # I should have used:
    # minValues <- apply(X=cbind(epochStart, -df$sfpd+200ms), MARGIN=1, FUN=max)
    minValues <- apply(X=cbind(epochStart, -df$sfpd), MARGIN=1, FUN=max)
    if(class(stdsNextCuesLatenciesInfoDF)=="data.frame") {
        maxValues <- apply(X=cbind(epochEnd, df$stdNextDLatency, 
                                             df$stdNextCueLatency,
                                             df$stdNextEOBLatency), 
                        MARGIN=1, FUN=min)-clipTo
    } else {
        maxValues <- apply(X=cbind(epochEnd, df$stdNextDLatency, 
                                             df$stdNextEOBLatency), 
                        MARGIN=1, FUN=min)-clipTo
    }
    shifts <- runif(nrow(df), min=minValues, max=maxValues)
    return(shifts)
}

