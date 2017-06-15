# tests if median(x)>median(y)

permuteDifferenceMedians <- function(x, y, nResamples=2000) {
    computeDiffOfMedians <- function(total, i) {
        diff <- median(total[i[1:length(x)]])-
                median(total[i[(length(x)+1):length(total)]])
        return(diff)
    }
    total <- c(x, y)
    bootRes <- boot(data=total, statistic=computeDiffOfMedians,
                                sim="permutation", R=nResamples)
    return(bootRes)
}
