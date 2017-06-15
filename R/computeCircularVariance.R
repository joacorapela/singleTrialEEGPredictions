computeCircularVariance <- function(angles) {
    c <- 0
    s <- 0
    for(angle in angles) {
        c <- c + cos(angle)
        s <- s + sin(angle)
    }
    r <- sqrt(c^2+s^2)
    v <- 1-r/length(angles)
    return(v)
}
