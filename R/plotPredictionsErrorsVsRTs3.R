plotPredictionsErrorsVsRTs3 <- function(predictionErrors,
                                         rts, 
                                         dfpds,
                                         maxRT=600,
                                         maxDFPD,
                                         nResamples=2000,
                                         conf,
                                         getStatsCorCoefAnnotationFunction,
                                         xCorCoefAnnotation=0.05,
                                         yCorCoefAnnotation=0.3, 
                                         sizePoints=3,
                                         sizeAnnotations=4,
                                         colourAnnotations="red",
                                         hjust=1, vjust=1,
                                         xlab="Reaction Time (ms)",
                                         ylab="Prediction Error",
                                         main="", 
                                         ...) {
#     transformedDelays <- scaleDelays*scaledTransformedDelays+centerDelays
#     transformedPredictions <- scaleDelays*scaledTransformedPredictions+centerDelays
#     if(!is.nan(lambda)) {
#         if(lambda==0) {
#             delays <- exp(transformedDelays)
#             predictions <- exp(transformedPredictions)
#         } else {
#             delays <- (transformedDelays*lambda+1)^(1/lambda)
#             predictions <- (transformedPredictions*lambda+1)^(1/lambda)
#         }
#     } else {
#         delays <- transformedDelays
#         predictions <- transformedPrediction
#     }
    validIndices <- which(!is.nan(rts) & 
                          !is.nan(dfpds) & dfpds<maxDFPD)
    rts <- rts[validIndices]
    dfpds <- dfpds[validIndices]
    predictionErrors <- predictionErrors[validIndices]
    rts[rts>maxRT] <- Inf
    indicesDetectedTargets <- which(is.finite(rts))

    detectedTargetsRTs <- rts[indicesDetectedTargets]
    detectedTargetsDFPDs <- dfpds[indicesDetectedTargets]
    detectedTargetsPredictionErrors <- predictionErrors[indicesDetectedTargets]
    statsCorCoef <- getStatsCorCoefRTsVsValues(
                      rts=detectedTargetsRTs,
                      maxRT=maxRT,  
                      dfpds=detectedTargetsDFPDs,
                      maxDFPD=maxDFPD,  
                      values=detectedTargetsPredictionErrors,
                      maxValue=Inf,  
                      nResamples=nResamples,
                      conf=conf,
                      ...)
    corCoefAnnotation <- getStatsCorCoefAnnotationFunction(statsCorCoef=
                                                            statsCorCoef)
    outRes <- getOutliersIndices(x=detectedTargetsRTs,
                                  y=detectedTargetsPredictionErrors, ...)
    bOutIndices <- outRes$bivariateOutliersIndices
#     xOutIndices <- outRes$xOutliersIndices
#     yOutIndices <- outRes$yOutliersIndices
    xOutIndices <- c()
    yOutIndices <- c()
    allOutIndices <- outRes$allOutliersIndices
    outFactor <- getOutliersFactor(allIndices=1:length(detectedTargetsRTs),
                                    xOutIndices=xOutIndices,
                                    yOutIndices=yOutIndices,
                                    bOutIndices=bOutIndices)
    if(length(allOutIndices)>0) {
        detectedTargetsRTsWoOutliers <- 
         detectedTargetsRTs[-allOutIndices]
        detectedTargetsPredictionErrorsWoOutliers <- 
         detectedTargetsPredictionErrors[-allOutIndices]
    } else {
        detectedTargetsRTsWoOutliers <- detectedTargetsRTs
        detectedTargetsPredictionErrorsWoOutliers <- 
         detectedTargetsPredictionErrors
    }
    lmRes <- lm(y~x, 
                 data=data.frame(x=detectedTargetsRTsWoOutliers, 
                                  y=detectedTargetsPredictionErrorsWoOutliers))
    d <- data.frame(x=detectedTargetsRTs, y=detectedTargetsPredictionErrors, 
                                          outFactor=outFactor)
    p <- ggplot(d, aes(x, y)) + 
          geom_point(aes(colour=outFactor, shape=outFactor), size=sizePoints) + 
          scale_colour_manual(values=c(noOut="black", xOut="red", yOut="blue", 
                                                      bOut="green", 
                                                      xyOut="magenta", 
                                                      xbOut="yellow", 
                                                      ybOut="cyan",
                                                      xybOut="violet")) +
          scale_shape_manual(values=c(noOut=1, xOut=16, yOut=16, bOut=16, 
                                               xyOut=16, xbOut=16, ybOut=16,
                                               xybOut=16)) +
          geom_abline(intercept=lmRes$coef[1], slope=lmRes$coef[2], 
                                               colour="red") + 
          annotate("text", x=xCorCoefAnnotation, y=yCorCoefAnnotation,
                   label=corCoefAnnotation, vjust=vjust, hjust=hjust, 
                   size=sizeAnnotations, colour=colourAnnotations) +
          xlab(xlab) + 
          ylab(ylab) + 
          ggtitle(main) +
          theme(legend.position="none")
    print(p)
}
