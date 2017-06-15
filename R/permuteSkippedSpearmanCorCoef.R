permuteSkippedSpearmanCorCoef <- function(x, y, nResamples, ...) {
    permStatistic <- function(data, indices) {
        if(sd(data[,1])==0 || sd(data[indices, 2])==0) {
            return(NaN)
        }
        cor <- spear(x=data[,1], y=data[indices, 2])$cor
        return(cor)
    }

    outliersIndices <- getOutliersIndices(x=x, y=y)$bivariateOutliersIndices
    if(length(outliersIndices)>0) {
        x <- x[-outliersIndices]
        y <- y[-outliersIndices]
    }
    permRes <- boot(data=cbind(x, y), statistic=permStatistic, 
                                      sim="permutation", R=nResamples)
    return(permRes)
}
