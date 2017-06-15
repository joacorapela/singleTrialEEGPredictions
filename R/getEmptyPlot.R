getEmptyPlot <- function() {
    df <- data.frame()
    p <- ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
    return(p)
}
