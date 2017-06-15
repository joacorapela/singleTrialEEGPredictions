bootstrapSkippedPearsonCorCoef <- function(x, y, nResamples) {
    bootStatistic <- function(data, indices) {
        if(sd(data[indices, 1])==0 || sd(data[indices, 2])==0) {
            return(NaN)
        }
        cor <- pcor(x=data[indices,])$cor[1, 2]
        return(cor)
    }

    outliersIndices <- getOutliersIndices(x=x, y=y)$bivariateOutliersIndices
    m <- cbind(x, y)
    if(length(outliersIndices)>0) {
        m <- m[-outliersIndices,]
    }
    bootRes <- boot(data=m, statistic=bootStatistic, R=nResamples)
    return(bootRes)
}
