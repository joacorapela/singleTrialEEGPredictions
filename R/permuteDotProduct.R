
permuteDotProduct <- function(x, y, nResamples=2000) {
    computeDotProduct <- function(data, i) {
        indices <- i[1:(length(i)/2)]
        dotProduct <- sum(data[indices]*data[-indices])
        return(dotProduct)
    }
    data <- c(x, y)
    permRes <- boot(data=data, statistic=computeDotProduct, 
                               sim="permutation", R=nResamples)
    return(permRes)
}
