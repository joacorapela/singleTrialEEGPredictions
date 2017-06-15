getRTsForSTDsUREvents <- function(stdsUREvents, rtsForSTDsInfo) {
    rts <- array(NA, dim=length(stdsUREvents))
    for(i in 1:length(rts)) {
        stdUREvent <- stdsUREvents[i]
        ntIndexInRTsForSTDsInfo <- which(rtsForSTDsInfo[,1]==stdUREvent)
        if(length(ntIndexInRTsForSTDsInfo)!=1) {
#             warning(sprintf("stdUREvent %d not found", stdUREvent))
#             show(sprintf("stdUREvent %d not found", stdUREvent))
#             browser()
        } else {
            rts[i] <- rtsForSTDsInfo[ntIndexInRTsForSTDsInfo, 2]
        }
    }
    return(as.numeric(rts))
}
