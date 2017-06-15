plotValuesVsAveragedBehavioralMeasures <- function(
 values, 
 behavioralMeasures, 
 scNames=NA,
 nResamples,
 conf,
 getStatsCorCoefAnnotationFunc,
 xlab,
 ylab="Correlation Coefficient", 
 main="",
 xCorCoefAnnotation=0.05,
 yCorCoefAnnotation=0.3, 
 sizePoints=3,
 sizeLabels=3.5, 
 sizeAnnotations=4,
 errorBarWidth=.02,
 errorBarHeight=.02,
 errorBarLineType="solid",
 errorBarColour="grey",
 ylim=NA,
 xlim=NA,
 ...) {

    if(length(behavioralMeasures)>2) {
        outRes <- getOutliersIndices(x=behavioralMeasures, y=values, ...)
        bOutIndices <- outRes$bivariateOutliersIndices
        xOutIndices <- outRes$xOutliersIndices
        yOutIndices <- outRes$yOutliersIndices
        allOutIndices <- outRes$allOutliersIndices

        if(length(allOutIndices)>0) {
            behavioralMeasuresWoOutliers <- 
             behavioralMeasures[-allOutIndices]
            predSFPDurCorsWoOutliers <- values[-allOutIndices]
        } else {
            behavioralMeasuresWoOutliers <- behavioralMeasures
            predSFPDurCorsWoOutliers <- values
        }

        lmRes <- lm(y~x, data=data.frame(x=behavioralMeasuresWoOutliers, 
                                          y=predSFPDurCorsWoOutliers))
        statsCorCoef <-
         getStatsCorCoefValuesVsAveragedBehavioralMeasures(
          values=values,
          behavioralMeasures=behavioralMeasures,
          nResamples=nResamples, 
          conf=conf, 
          ...)
        statsCorCoefAnnotation <- getStatsCorCoefAnnotationFunc(statsCorCoef=
                                                                  statsCorCoef,
                                                                 rConf=conf)
        outFactor <- getOutliersFactor(allIndices=
                                               1:length(values),
                                             xOutIndices=xOutIndices,
                                             yOutIndices=yOutIndices,
                                             bOutIndices=bOutIndices)
        d <- data.frame(x=behavioralMeasures, 
                         y=values, 
                         outFactor=outFactor)
        p <- ggplot(d, aes(x, y)) + 
             geom_point(aes(colour=outFactor, shape=outFactor), 
                         size=sizePoints)
        if(!is.na(ylim[1])) {
            p <- p + ylim(ylim)
        }
        if(!is.na(xlim[1])) {
            p <- p + xlim(xlim)
        }
        p <- p + scale_colour_manual(values=c(noOut="black", xOut="red", 
                                                         yOut="blue", 
                                                         bOut="green", 
                                                         xyOut="magenta", 
                                                         xbOut="yellow", 
                                                         ybOut="cyan",
                                                         xybOut="white")) +
              scale_shape_manual(values=c(noOut=1, xOut=16, yOut=16, bOut=16, 
                                                   xyOut=16, xbOut=16, ybOut=16,
                                                   xybOut=16)) +
#               geom_errorbarh(aes(xmin=xmin, xmax=xmax), 
#                               linetype=errorBarLineType, 
#                               height=errorBarHeight,
#                               colour=errorBarColour) +
#               geom_errorbarh(aes(xmin=xmin, xmax=xmax),
#                               colour=errorBarColour) +
              geom_abline(intercept=lmRes$coef[1], slope=lmRes$coef[2], 
                                                   colour="red") + 
              annotate("text", x=xCorCoefAnnotation, y=yCorCoefAnnotation, 
                       label=statsCorCoefAnnotation, hjust=0, 
                       size=sizeAnnotations, colour="red") + 
              xlab(xlab) + ylab(ylab) + ggtitle(main) + 
              theme(legend.position="none")
        if(!is.na(scNames)) {
            p <- p + geom_text(aes(label=scNames, vjust=1.5), size=sizeLabels)
        }
        print(p)
    }
}
