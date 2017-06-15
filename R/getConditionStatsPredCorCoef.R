
getConditionStatsPredCorCoef <- function(condition, stats) {
    for(i in 1:length(stats)) {
        if(stats[[i]]$condition==condition) {
            return(stats[[i]]$stats)
        }
    }
    stop(sprintf("Could not find condition=%s", condition))
}
