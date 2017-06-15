
permuteNCorrelation <- function(x, y, nResamples=2000) {
    computeNCorrelation <- function(data, i) {
        nCorrelation <- normalizedCorrelation(data[,1], data[i,2])
        return(nCorrelation)
    }
    data <- cbind(x, y)
    permRes <- boot(data=data, statistic=computeNCorrelation, 
                               sim="permutation", R=nResamples)
    return(permRes)
}
