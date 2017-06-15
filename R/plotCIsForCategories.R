plotCIsForCategories <- function(categoriesNames, cis, xlab, ylab, main,
                                      ylim=NA, errorBarWidth=.25, 
                                      errorBarLineType="solid",
                                      hlinetype="solid",
                                      hlineCol="black") {
    d <- data.frame(x=categoriesNames, 
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
                                ggtitle(main) +
                                theme(axis.text.x=element_text(angle=90)) +
                                scale_x_discrete(limits=categoriesNames)
    if(length(ylim)>1 && !is.na(ylim[1])) {
        p <- p + ylim(ylim)
    }
    print(p)
}
