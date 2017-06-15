# x should contain a first column of 1s

coefsRidgeRegression <- function(x, y, lambda) {
    # The constant term should not be penalized
    iTilde <- diag(ncol(x))
    # But I am not penalizing it to be coherent with the variational bayes 
    # approach to select lambda
    # iTilde[1, 1] <- 0
    #
    coefs <- solve(a=t(x)%*%x+lambda*iTilde, b=t(x)%*%y)
    return(coefs)
}
