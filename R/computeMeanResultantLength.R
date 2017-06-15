computeMeanResultantLength <- function(angles) {
    c <- 0
    s <- 0
    for(angle in angles) {
        c <- c + cos(angle)
        s <- s + sin(angle)
    }
    r <- sqrt(c^2+s^2)
    rBar <- r/length(angles)
    return(rBar)
}
