permuteSkippedPearsonCorCoef <- function(x, y, nResamples) {
    permStatistic <- function(data, indices) {
        if(sd(data[,1])==0 || sd(data[indices, 2])==0) {
            return(NaN)
        }
        cor <- pcor(x=data[,1], y=data[indices, 2])$cor
        return(cor)
    }

    outliersIndices <- getOutliersIndices(x=x, y=y)$bivariateOutliersIndices
    m <- cbind(x, y)
    if(length(outliersIndices)>0) {
        m <- m[-outliersIndices,]
    }
    permRes <- boot(data=m, statistic=permStatistic, sim="permutation", 
                            R=nResamples)
    return(permRes)
}
