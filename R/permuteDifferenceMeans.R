# tests if mean(x)>mean(y)

permuteDifferenceMeans <- function(x, y, nResamples=2000) {
    computeDiffOfMeans <- function(total, i) {
        diff <- mean(total[i[1:length(x)]])-
                mean(total[i[(length(x)+1):length(total)]])
        return(diff)
    }
    total <- c(x, y)
    bootRes <- boot(data=total, statistic=computeDiffOfMeans,
                                sim="permutation", R=nResamples)
    return(bootRes)
}
