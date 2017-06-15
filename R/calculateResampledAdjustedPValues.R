calculateResampledAdjustedPValues <- function(datasets, minPStars, 
                                                        calculatePValueFunc) {
    ps <- c()
    nDatasets <- length(datasets)
    for(i in 1:nDatasets) {
        show(sprintf("Processing dataset %02d (%02d)", i, nDatasets))
        p <- calculatePValueFunc(x=datasets[[i]]$x, y=datasets[[i]]$y)
        ps <- c(ps, list(list(metaData=datasets[[i]]$metaData, p=p)))
    }
    adjPs <- c()
    for(i in 1:length(ps)) {
        adjP <- sum(ps[[i]]$p>minPStars)/length(minPStars)
        adjPs <- c(adjPs, list(c(ps[[i]], adjP=adjP)))
    }
    return(list(adjPs=adjPs, minPStars=minPStars))
}
