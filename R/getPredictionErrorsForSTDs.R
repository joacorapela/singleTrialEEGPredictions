getPredictionErrorsForSTDs <- function(scaledTransformedPredictions, 
                                        scaledTransformedSFPDs, 
                                        stdsIDs,
                                        epochEventIDs) {
    predictionsErrors <-
     abs(scaledTransformedPredictions-scaledTransformedSFPDs)
    validIndices <- which(!is.na(match(epochEventIDs, stdsIDs)))
    validPredictionErrors <- predictionsErrors[validIndices]
    return(validPredictionErrors)
}
