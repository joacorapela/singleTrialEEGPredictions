bootstrapVariationalCoefs <- function(data, nResamples, a0, b0, c0, d0, 
                                            maxIter, convergenceTol) {
    bootStatistic <- function(data, indices, a0, b0, c0, d0, 
                                    maxIter, convergenceTol) {
        res <- variationalLinearRegression(t=data[indices,1], 
                                            phi=data[indices,-1], 
                                            a0=a0, b0=b0, c0=c0, d0=d0, 
                                            maxIter=maxIter,
                                            convergenceTol=convergenceTol)
        return(res$mN)
    }

    system.time(res <- boot(data=data, statistic=bootStatistic, R=nResamples,
                                       a0=a0, b0=b0, c0=c0, d0=d0,
                                       maxIter=maxIter, 
                                       convergenceTol=convergenceTol))
    return(res)
}
