
permuteCCF0 <- function(x, y, nResamples=2000) {
    computeCCF0 <- function(data, i) {
        ccf0 <- cor(data[,1], data[i,2])
        return(ccf0)
    }
    data <- cbind(x, y)
    permRes <- boot(data=data, statistic=computeCCF0, 
                               sim="permutation", R=nResamples)
    return(permRes)
}
