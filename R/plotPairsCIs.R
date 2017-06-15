

plotPairsCIs <- function(labels, cis1, cis2, nResamples, ciConf, 
                                 annotationPattern,
                                 bootPairedDifferencesFunction,
                                 xAnnotation=0.17,
                                 yAnnotation=0.025,
                                 sizeLabels=3.5, 
                                 sizeAnnotations=4,
                                 errorBarWidth=0.01,
                                 errorBarHeight=0.01,
                                 xlab="Correlation Coefficient in Surrogate Dataset",
                                 ylab="Correlation Coefficient in Original Dataset",
                                 xlim=NA, ylim=NA) {
    df <- data.frame(x=cis1[,1], y=cis2[,1], names=labels, 
                                 xmin=cis1[,2], xmax=cis1[,3],
                                 ymin=cis2[,2], ymax=cis2[,3])
    bootRes <- bootPairedDifferencesFunction(x=cis2[,1], y=cis1[,1], nResamples=nResamples)
#     bootRes <- bootstrapMeanPairedDifference(x=cis2[,1], y=cis1[,1], nResamples=nResamples)
#     bootRes <- bootstrapMedianPairedDifference(x=cis2[,1], y=cis1[,1], nResamples=nResamples)
    bootCIs <- getBootstrapCIs(bootRes=bootRes, conf=ciConf)
    annotation <- sprintf(annotationPattern, bootCIs[1], bootCIs[2], bootCIs[3])
    p <- ggplot(df, aes(x=x, y=y)) + geom_point() + 
         geom_abline(color="red") +
         geom_errorbar(aes(ymin=ymin, ymax=ymax), width=errorBarWidth) +
         geom_errorbarh(aes(xmin=xmin, xmax=xmax), height=errorBarHeight) +
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
