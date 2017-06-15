# tests if x>y

bootstrapDifferenceMedians <- function(x, y, nResamples=2000) {
    computeDiffOfMedians <- function(data, i) {
        tApplyRes <- tapply(X=data[i,1], INDEX=data[i,2], FUN=median)
        return(tApplyRes[1]-tApplyRes[2])
    }
    data <- data.frame(values=c(x, y), labels=c(rep("x", times=length(x)), 
                                                 rep("y", times=length(y))))
    bootRes <- boot(data, computeDiffOfMedians, R=nResamples,
                          strata=data[,2])
    return(bootRes)
}
