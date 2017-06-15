computeMeanDirection <- function(angles) {
    c <- 0
    s <- 0
    for(angle in angles) {
        c <- c + cos(angle)
        s <- s + sin(angle)
    }
    if(s>0 & c<0) {
        meanDirection <- atan(s/c) + pi
    } else {
        if(s<0 & c<0) {
            meanDirection <- atan(s/c)-pi
        } else {
            meanDirection <- atan(s/c)
        }
    }
    return(meanDirection)
}
