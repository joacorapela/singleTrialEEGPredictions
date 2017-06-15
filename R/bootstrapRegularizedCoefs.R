bootstrapRegularizedCoefs <- function(y, x, nResamples, lambda) {
    bootStatistic <- function(data, indices, lambda) {
        x <- data[indices,-1]
        y <- data[indices,1]
        coefs <- coefsRidgeRegression(x=x, y=y, lambda=lambda)
        return(coefs)
    }

    data <- cbind(y, x)
    res <- boot(data=data, statistic=bootStatistic, R=nResamples, lambda=lambda)
    return(res)
}
