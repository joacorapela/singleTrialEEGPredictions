plotPredictionsVsSFPDs <- function(scaledTransformedPredictions, 
                                     scaledTransformedSFPDs, 
                                     statsCorCoef,
                                     corCoefAnnotationFunction,
                                     xCorCoefAnnotation=Inf,
                                     yCorCoefAnnotation=Inf, 
                                     hjust=0,
                                     vjust=1.0,
                                     sizePoints=3,
                                     sizeAnnotations=6,
                                     main="", 
                                     xlab="SFP durations (ms)",
                                     ylab="Predictions (ms)",
                                     ylim=NA,
                                     xlim=NA) {
    sfpds <- scaledTransformedSFPDs
    predictions <- scaledTransformedPredictions

    corCoefAnnotation <- corCoefAnnotationFunction(statsCorCoef=statsCorCoef)
    outRes <- getOutliersIndices(x=scaledTransformedSFPDs, 
                                  y=scaledTransformedPredictions)
    bOutIndices <- outRes$bivariateOutliersIndices
    if(length(bOutIndices)>0) {
        sfpdsWOOutliers <- sfpds[-bOutIndices]
        predictionsWOOutliers <- predictions[-bOutIndices]
    } else {
        sfpdsWOOutliers <- sfpds
        predictionsWOOutliers <- predictions
    }

    lmRes <- lm(y~x, data=data.frame(x=sfpdsWOOutliers,
                                      y=predictionsWOOutliers))
    outFactor <- getOutliersFactor(allIndices=1:length(scaledTransformedSFPDs),
                                    xOutIndices=c(),
                                    yOutIndices=c(),
                                    bOutIndices=bOutIndices)
    d <- data.frame(x=sfpds, y=predictions, outFactor=outFactor)
    p <- ggplot(d, aes(x, y)) + 
          geom_point(aes(colour=outFactor, shape=outFactor), size=sizePoints) + 
          scale_colour_manual(values=c(noOut="black", xOut="red", yOut="blue", 
                                                      bOut="green", 
                                                      xyOut="magenta", 
                                                      xbOut="yellow", 
                                                      ybOut="cyan",
                                                      xybOut="orange")) +
          scale_shape_manual(values=c(noOut=1, xOut=16, yOut=16, bOut=16, 
                                               xyOut=16, xbOut=16, ybOut=16,
                                               xybOut=16)) +
          geom_abline(intercept=lmRes$coef[1], slope=lmRes$coef[2], 
                                               colour="red") + 
          annotate("text", x=xCorCoefAnnotation, y=yCorCoefAnnotation,
                   label=corCoefAnnotation, hjust=hjust, vjust=vjust,
                   size=sizeAnnotations, colour="red") +
          xlab(xlab) + 
          ylab(ylab) + 
          ggtitle(main) +
          theme(legend.position="none")
    if(!is.na(xlim[1])) {
        p <- p + xlim(xlim)
    }
    if(!is.na(ylim[1])) {
        p <- p + ylim(ylim)
    }
    print(p)
}


