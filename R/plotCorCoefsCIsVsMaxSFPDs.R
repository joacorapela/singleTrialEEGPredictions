plotCorCoefsCIsVsMaxSFPDs <- function(maxSFPDs, cis, xlab, ylab, main,
                                       ylim=NA, errorBarWidth=.25, 
                                       errorBarLineType="solid",
                                       hlinetype="solid",
                                       hlineCol="black") {
    d <- data.frame(x=maxSFPDs, 
                     y=cis[, 1],
                     ymin=cis[, 2],
                     ymax=cis[, 3])
    p <- ggplot(d, aes(x, y)) + geom_point() +
                                geom_errorbar(aes(ymin=ymin, ymax=ymax),
                                               linetype=errorBarLineType,
                                               width=errorBarWidth) +
                                geom_hline(yintercept=0, linetype=hlinetype, 
                                                         colour=hlineCol) +
                                xlab(xlab) +
                                ylab(ylab) +
                                ggtitle(main)
    if(length(ylim)>1 && !is.na(ylim[1])) {
        p <- p + ylim(ylim)
    }
    print(p)
}
