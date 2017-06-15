normalizedCorrelation <- function(x, y) {
    xN <- x/sqrt(cor(x,x))
    yN <- y/sqrt(cor(y,y))
    nCor <- cor(xN, yN)
    return(nCor)
}
