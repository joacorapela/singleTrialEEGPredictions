plotValuesCIsVsAveragedBehavioralMeasures <- function(
 valuesCIs, 
 behavioralMeasuresCIs, 
 scNames,
 statsCorCoefAnnotation,
 xlab,
 ylab="Correlation Coefficient", 
 main="",
 xCorCoefAnnotation=0.05,
 yCorCoefAnnotation=0.3, 
 vjust=1,
 hjust=1,
 sizePoints=3,
 sizeLabels=3.5, 
 sizeAnnotations=4,
 axisFontSize=14,
 errorBarWidth=.02,
 errorBarHeight=.02,
 errorBarLineType="solid",
 errorBarColour="grey",
 ylim=NA,
 ...) {
    if(nrow(behavioralMeasuresCIs)>2) {
        outRes <- getOutliersIndices(x=behavioralMeasuresCIs[, 1],
                                      y=valuesCIs[, 1], ...)
        bOutIndices <- outRes$bivariateOutliersIndices
#         xOutIndices <- outRes$xOutliersIndices
#         yOutIndices <- outRes$yOutliersIndices
#         allOutIndices <- outRes$allOutliersIndices
        xOutIndices <- c()
        yOutIndices <- c()
        allOutIndices <- bOutIndices

        if(length(allOutIndices)>0) {
            behavioralMeasuresWoOutliers <- 
             behavioralMeasuresCIs[-allOutIndices, 1]
            predSFPDurCorsWoOutliers <- valuesCIs[-allOutIndices, 1]
        } else {
            behavioralMeasuresWoOutliers <- behavioralMeasuresCIs[, 1]
            predSFPDurCorsWoOutliers <- valuesCIs[, 1]
        }

        lmRes <- lm(y~x, data=data.frame(x=behavioralMeasuresWoOutliers, 
                                          y=predSFPDurCorsWoOutliers))
        outFactor <- getOutliersFactor(allIndices=1:nrow(valuesCIs),
                                        xOutIndices=xOutIndices,
                                        yOutIndices=yOutIndices,
                                        bOutIndices=bOutIndices)
        d <- data.frame(x=behavioralMeasuresCIs[, 1], 
                         y=valuesCIs[, 1], 
                         names=scNames,
                         ymin=valuesCIs[,2], 
                         ymax=valuesCIs[,3],
                         xmin=behavioralMeasuresCIs[,2], 
                         xmax=behavioralMeasuresCIs[,3],
                         outFactor=outFactor)
        p <- ggplot(d, aes(x, y)) + 
             geom_point(aes(colour=outFactor, shape=outFactor), 
                         size=sizePoints) +
             scale_colour_manual(values=c(noOut="black", xOut="red", 
                                                         yOut="blue", 
                                                         bOut="green", 
                                                         xyOut="magenta", 
                                                         xbOut="yellow", 
                                                         ybOut="cyan",
                                                         xybOut="white")) +
              scale_shape_manual(values=c(noOut=1, xOut=16, yOut=16, bOut=16, 
                                                   xyOut=16, xbOut=16, ybOut=16,
                                                   xybOut=16)) +
              geom_errorbar(aes(ymin=ymin, ymax=ymax), 
                             linetype=errorBarLineType, 
                             width=errorBarWidth,
                             colour=errorBarColour) +
              geom_errorbarh(aes(xmin=xmin, xmax=xmax), 
                              linetype=errorBarLineType, 
                              height=errorBarHeight,
                              colour=errorBarColour) +
              geom_text(aes(label=names, vjust=1.5), size=sizeLabels) + 
              geom_abline(intercept=lmRes$coef[1], slope=lmRes$coef[2], 
                                                   colour="red") + 
              annotate("text", x=xCorCoefAnnotation, y=yCorCoefAnnotation, 
                       label=statsCorCoefAnnotation, hjust=hjust, vjust=vjust,
                       size=sizeAnnotations, colour="red") + 
              theme(text = element_text(size=axisFontSize)) +
              xlab(xlab) + ylab(ylab) + ggtitle(main) + 
              theme(legend.position="none")
        if(!is.na(ylim[1])) {
            p <- p + ylim(ylim)
        }
        print(p)
    }
}
