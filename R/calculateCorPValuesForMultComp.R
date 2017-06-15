calculateCorPValuesForMultComp <- function(datasets, 
                                            calculateCorPValueFunc,
                                            nResamples,
                                            printProgressEachNResamples=1) {
    # The following line is to force the random number generator to
    # reset the random key. If I don't reset it and then sample(1:10) from
    # two R instances using the sampe .RData (for example from two R instances
    # in two nodes of the cluster) will produce identical results
    rm(.Random.seed, envir=globalenv())
    #
    minPStars <- c()
    for(g in 1:nResamples) {
        if(g%%printProgressEachNResamples==0) {
            show(sprintf("Processing resample %d", g))
        }
        minPStar <- Inf
        pStars <- c()
        for(i in 1:length(datasets)) {
            pStar <- calculateCorPValueFunc(x=sample(datasets[[i]]$x),
                                             y=datasets[[i]]$y)
            pStars <- c(pStars, pStar)
        }
        minPStars <- c(minPStars, min(pStars))
    }
    return(minPStars)
}
