
getScales <- function(noctave, nvoice) {
    scales <- c()
    for(j in 1:noctave) {
        twoToJ <- 2^j
        # bs <- 2^j
        # ts <- 2^(j+1)
        for(i in 1:nvoice) {
            # scales <- c(scales, bs + (i-1)/nvoice * (ts-bs))
            scales <- c(scales, twoToJ*(1+(i-1)/nvoice))
        }
    }
    return(scales)
}

