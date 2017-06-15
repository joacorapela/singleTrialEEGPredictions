
getModalityStatsPredCorCoef <- function(modality, stats) {
    for(i in 1:length(stats)) {
        if(stats[[i]]$modality==modality) {
            return(stats[[i]]$stats)
        }
    }
    stop(sprintf("Could not find modality=%s", modality))
}
