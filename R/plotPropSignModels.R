

plotPropSignModels <- function(ysProps, xsProps, annotation, clustersLabels,
                                        ylab, xlab, main,
                                        sizeLabels=2.5, 
                                        sizeAnnotations=4,
                                        ablineColour="grey", 
                                        xlim=c(0, 1),
                                        ylim=c(0, 1),
                                        xAnnotation=0.25,
                                        yAnnotation=0.25,
                                        ...) {
    d <- data.frame(x=xsProps, y=ysProps, names=clustersLabels)
    p <- ggplot(d, aes(x, y)) + 
          geom_point() + 
          geom_text(aes(label=names, vjust=1.5), size=sizeLabels) +
          geom_abline(intercept=0, slope=1, colour=ablineColour) + 
          annotate("text", x=xAnnotation, y=yAnnotation, 
                   label=annotation, hjust=0, size=sizeAnnotations, 
                   colour="red") +
          xlab(xlab) + 
          ylab(ylab) + 
          ggtitle(main) +
          xlim(xlim) +
          ylim(ylim)
    print(p)
}
