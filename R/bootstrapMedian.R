bootstrapMedian <- function(x, nResamples, ...) {
    bootStatistic <- function(data, indices) {
        median <- median(x=data[indices])
        return(median)
    }

    bootRes <- boot(data=x, statistic=bootStatistic, R=nResamples)
    return(bootRes)
}
