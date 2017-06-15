calculateBonferroniAdjustedPValues <- function(datasets, calculatePValueFunc) {
    ps <- c()
    adjPs <- c()
    nDatasets <- length(datasets)
    if(nDatasets>0) {
        for(i in 1:nDatasets) {
            show(sprintf("Processing dataset %02d (%02d)", i, nDatasets))
            p <- calculatePValueFunc(x=datasets[[i]]$x, y=datasets[[i]]$y)
            ps <- c(ps, list(list(metaData=datasets[[i]]$metaData, p=p)))
        }
        for(i in 1:length(ps)) {
            adjP <- min(ps[[i]]$p*length(ps), 1)
            adjPs <- c(adjPs, list(c(ps[[i]], adjP=adjP)))
        }
    }
    return(adjPs)
}
