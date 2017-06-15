# tests if sum(x^2)>sum(y^2)

permuteDifferenceEnergies <- function(x, y, nResamples=2000) {
    computeDiffOfEnergies <- function(total, i) {
        diff <- sum(total[i[1:length(x)]]^2)-
                sum(total[i[(length(x)+1):length(total)]]^2)
        return(diff)
    }
    total <- c(x, y)
    bootRes <- boot(data=total, statistic=computeDiffOfEnergies,
                                sim="permutation", R=nResamples)
    return(bootRes)
}
