

plotPairsValues <- function(labels, values1, values2, nResamples, ciConf, 
                                    annotationPattern,
                                    xAnnotation=0.17,
                                    yAnnotation=0.025,
                                    sizeLabels=3.5, 
                                    sizeAnnotations=4,
                                    errorBarWidth=0.01,
                                    errorBarHeight=0.01,
                                    xlab, ylab,
                                    xlim=NA, ylim=NA) {
    df <- data.frame(x=values1, y=values2, names=labels)
    bootRes <- bootstrapMeanPairedDifference(x=values1, y=values2, nResamples=nResamples)
    bootCIs <- getBootstrapCIs(bootRes=bootRes, conf=ciConf)
    annotation <- sprintf(annotationPattern, bootCIs[1], bootCIs[2], bootCIs[3])
    p <- ggplot(df, aes(x=x, y=y)) + geom_point() + 
         geom_abline(color="red") +
         geom_text(aes(label=names, vjust=1.5), size=sizeLabels) + 
         annotate("text", x=xAnnotation, y=yAnnotation, label=annotation, 
                  hjust=0, size=sizeAnnotations, colour="red") +
         xlab(xlab) +
         ylab(ylab)
    if(!is.na(xlim[1])) {
        p <- p + xlim(xlim)
    }
    if(!is.na(ylim[1])) {
        p <- p + ylim(ylim)
    }
    print(p)
}
