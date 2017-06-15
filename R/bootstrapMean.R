bootstrapMean <- function(x, nResamples, ...) {
    bootStatistic <- function(data, indices) {
        mean <- mean(x=data[indices])
        return(mean)
    }

    bootRes <- boot(data=x, statistic=bootStatistic, R=nResamples)
    return(bootRes)
}
