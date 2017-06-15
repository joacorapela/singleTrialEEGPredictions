
calculatePearsonCorPValue <- function(x, y) {
    return(cor.test(x, y)$p.value)
}

