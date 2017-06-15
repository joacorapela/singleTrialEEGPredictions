rayleighUniformityTest <- function(phases) {
    rBar <- computeMeanResultantLength(angles=phases)
    z <- length(phases)*rBar^2
    p <- exp(-z)
    return(p)
}
