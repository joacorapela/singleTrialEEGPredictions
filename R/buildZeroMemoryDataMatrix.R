buildZeroMemoryDataMatrix <- function(pxs, order, interactions=TRUE) {
    noRDs <- ncol(pxs)
    dataMatrix <- c()
    index <- 1

    # first order terms
    for(j in 1:noRDs) {
        dataMatrix <- cbind(dataMatrix, pxs[, j])
        index <- index+1
    }
    if(order<2) {
        return(dataMatrix)
    }

    # second order terms
    items <- array(dim=2)
    for(j1 in 1:noRDs) {
        if(interactions) {
            items[1] <- j1
            for(j2 in j1:noRDs) {
                items[2] <- j2
                dataMatrix <- cbind(dataMatrix, 
                                     computeSymmetriesCoef(items)*
                                     computeProductCols(pxs, items))
                index <- index+1
            } 
        } else {
            items <- c(j1, j1)
            dataMatrix <- cbind(dataMatrix, computeProductCols(pxs, items))
            index <- index+1
        }
    }
    if(order>2) {
        stop('order <= 2')
    }
    return(dataMatrix)
}
