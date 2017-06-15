
calculateSkippedPearsonCorPValue <- function(x, y, nResamples) {
    permRes <- permuteSkippedPearsonCorCoef(x=x, y=y, nResamples=nResamples)
    pValue <- sum(abs(permRes$t)>abs(permRes$t0))/length(permRes$t)
    return(pValue)
}

