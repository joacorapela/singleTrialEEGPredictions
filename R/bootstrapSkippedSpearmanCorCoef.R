bootstrapSkippedSpearmanCorCoef <- function(x, y, nResamples, ...) {
    bootStatistic <- function(data, indices) {
        if(sd(data[indices, 1])==0 || sd(data[indices, 2])==0) {
            return(NaN)
        }
        cor <- spear(x=data[indices,1], y=data[indices,2])$cor
        return(cor)
    }

    outliersIndices <- getOutliersIndices(x=x, y=y)$bivariateOutliersIndices
    if(length(outliersIndices)>0) {
        x <- x[-outliersIndices]
        y <- y[-outliersIndices]
    }
    bootRes <- boot(data=cbind(x, y), statistic=bootStatistic, R=nResamples)
    return(bootRes)
}
