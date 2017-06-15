# bootstraps mean(x-y)

bootstrapMeanPairedDifference <- function(x, y, nResamples) {
    bootStatistic <- function(data, i) {
        return(mean(data[i, 1] - data[i, 2]))
    }
    data <- cbind(x, y)
    bootRes <- boot(data, bootStatistic, R=nResamples)
    return(bootRes)
}
