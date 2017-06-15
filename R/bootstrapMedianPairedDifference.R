# bootstraps median(x-y)

bootstrapMedianPairedDifference <- function(x, y, nResamples=2000) {
    bootStatistic <- function(data, i) {
        return(median(data[i, 1] - data[i, 2]))
    }
    data <- cbind(x, y)
    bootRes <- boot(data, bootStatistic, R=nResamples)
    return(bootRes)
}
