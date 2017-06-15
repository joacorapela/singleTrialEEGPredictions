plotMediansOfPredictionsErrorsForHitsAndMisses <- 
 function(scaledTransformedPredictions, 
           scaledTransformedDelays, 
           rts, 
           maxRT=600,
           dfpds,
           maxDFPD,
           getStatsDifAnnotationFunction,
           lambda,
           centerDelays,
           scaleDelays,
           ylab="Median Prediction Error",
           main="",
           xDifAnnotation=0.75,
           yDifAnnotation=900,
           sizeAnnotations=4.5,
           errorBarsWidth=.1,
           conf=.95,
           nResamples=2000, 
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
#         delays <- transformedPredictions
#         predictions <- transformedDelays
#     }
    delays <- scaledTransformedPredictions
    predictions <- scaledTransformedDelays

    predictionsErrors <- abs(predictions-delays)
    statsDif <- getStatsDifInMedianValuesForHitsAndMisses(rts=rts,
                                                           maxRT=maxRT,
                                                           dfpds=dfpds,
                                                           maxDFPD=maxDFPD,
                                                           values=
                                                            predictionsErrors,
                                                           maxValue=Inf,
                                                           conf=conf,
                                                           nResamples=
                                                            nResamples, 
                                                           ...)
    statsDifAnnotation <- getStatsDifAnnotationFunction(statsDif=statsDif)

    validIndices <- which(!is.nan(rts) & 
                          !is.na(dfpds) & dfpds<maxDFPD & 
                          !is.na(predictionsErrors))
    rts <- rts[validIndices]
    dfpds <- dfpds[validIndices]
    predictionsErrors <- predictionsErrors[validIndices]
    rts[rts>maxRT] <- Inf
    indicesHits <- which(is.finite(rts))
    indicesMisses <- which(is.infinite(rts))
    if(length(indicesMisses)==0) {
        return()
    }
    hitsPredictionsErrors <- predictionsErrors[indicesHits]
    missesPredictionsErrors <- predictionsErrors[indicesMisses]

    bootResMedianHits <- bootstrapMedian(hitsPredictionsErrors, 
                                              nResamples=nResamples)
    bootCIMedianHits <- getBootstrapCIs(bootRes=bootResMedianHits,
                                             conf=conf)
    bootResMedianMisses <- bootstrapMedian(missesPredictionsErrors, 
                                            nResamples=nResamples)
    bootCIMedianMisses <- getBootstrapCIs(bootRes=bootResMedianMisses,
                                           conf=conf)
    medians <- c(bootCIMedianHits[1], bootCIMedianMisses[1])
    errorBarsMin <- c(bootCIMedianHits[2], bootCIMedianMisses[2])
    errorBarsMax <- c(bootCIMedianHits[3], bootCIMedianMisses[3])
    d <- data.frame(medians=medians, errorBarsMin=errorBarsMin, errorBarsMax)
    p <- ggplot(d, aes(x=factor(1:2), y=medians)) + 
         geom_bar(stat="identity", fill="orange", width=.1) +
         geom_errorbar(aes(ymin=errorBarsMin, ymax=errorBarsMax), 
                        width=errorBarsWidth) +
         scale_x_discrete(breaks = 1:2, labels=c("Hits","Misses")) +
         annotate("text", x=xDifAnnotation, y=yDifAnnotation, label=statsDifAnnotation, hjust=1, vjust=1, size=sizeAnnotations, colour="red") +
         xlab(NULL) +
         ylab(ylab) +
         ggtitle(main)
    print(p)
}
