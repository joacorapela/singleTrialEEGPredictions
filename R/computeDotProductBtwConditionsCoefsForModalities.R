
computeDotProductBtwConditionsCoefsForModalities <- 
 function(sortvar, modalities, clustersIDs, condition1, condition2, 
                   significance, nResamples, scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    answer <- c()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        res <- 
         computeDotProductBtwConditionsCoefsForClusters(
          sortvar=sortvar,
          modality=modality,
          clustersIDs=clustersIDs,
          condition1=condition1,
          condition2=condition2,
          significance=significance,
          nResamples=nResamples,
          scFilenamePattern=scFilenamePattern,
          minAndMaxSFPDOfBestPredictionsFilenamePattern=
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
          analyzedDataFilenamePattern=
           analyzedDataFilenamePattern, 
          ...)
        answer <- rbind(answer, 
                         cbind(modality=rep(modality, times=nrow(res)), res))
    }
    return(answer)
}
computeDotProductBtwConditionsCoefsForClusters <- 
 function(sortvar, modality, clustersIDs, condition1, condition2,
                   significance, nResamples,
                   scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    answer <- c()
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %02d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                            scFilename=scFilename)
        res <- 
         computeDotProductBtwConditionsCoefsForSubjectsAndComponents(
          sortvar=sortvar,
          modality=modality,
          clusterID=clusterID,
          condition1=condition1,
          condition2=condition2,
          significance=significance,
          nResamples=nResamples,
          subjectsAndComponents=subjectsAndComponents,
          minAndMaxSFPDOfBestPredictionsFilenamePattern=
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
          analyzedDataFilenamePattern=analyzedDataFilenamePattern,
          ...)
        answer <- rbind(answer, 
                         cbind(clusterID=rep(clusterID, times=nrow(res)), res))
    }
    return(answer)
}
computeDotProductBtwConditionsCoefsForSubjectsAndComponents <- 
 function(sortvar, modality, clusterID, condition1, condition2, 
                   significance, nResamples, 
                   subjectsAndComponents=subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    dotProducts <- c()
    pValues <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
         res <- 
          computeDotProductBtwConditionsCoefsForSubjectAndComponent(
           sortvar=sortvar,
           modality=modality,
           clusterID=clusterID,
           condition1=condition1,
           condition2=condition2,
           subjectName=subjectName,
           component=component,
           significance=significance,
           nResamples=nResamples,
           minAndMaxSFPDOfBestPredictionsFilenamePattern=
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
           analyzedDataFilenamePattern=analyzedDataFilenamePattern,
           ...)
        dotProducts <- c(dotProducts, res[1])
        pValues <- c(pValues, res[2])
    }
    return(data.frame(subjectName=subjectsAndComponents[,"subjectName"],
                       component=subjectsAndComponents[,"component"], 
                       dotProduct=dotProducts,
                       pValue=pValues))
}
computeDotProductBtwConditionsCoefsForSubjectAndComponent <- 
 function(sortvar, modality, clusterID, condition1, condition2,
                   subjectName, component, significance,
                   nResamples, minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
if(subjectName=="av118a" && component==3) {
    browser()
}
    c1MinAndMaxSFPDOfBestPredictionsFilename <- 
     sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, clusterID,
              clusterID, condition1, sortvar, modality, subjectName, component)
    res <- readLines(c1MinAndMaxSFPDOfBestPredictionsFilename)
    c1MinSFPD <- as.integer(res[1])
    c1MaxSFPD <- as.integer(res[2])
    c2MinAndMaxSFPDOfBestPredictionsFilename <- 
     sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, clusterID,
              clusterID, condition2, sortvar, modality, subjectName, component)
    res <- readLines(c2MinAndMaxSFPDOfBestPredictionsFilename)
    c2MinSFPD <- as.integer(res[1])
    c2MaxSFPD <- as.integer(res[2])
    c1AnalyzedDataFilename <- sprintf(analyzedDataFilenamePattern,
                                       clusterID,
                                       clusterID,
                                       condition1,
                                       sortvar,
                                       modality,
                                       subjectName,
                                       component,
                                       c1MinSFPD,
                                       c1MaxSFPD)
    c2AnalyzedDataFilename <- sprintf(analyzedDataFilenamePattern,
                                       clusterID,
                                       clusterID,
                                       condition2,
                                       sortvar,
                                       modality,
                                       subjectName,
                                       component,
                                       c2MinSFPD,
                                       c2MaxSFPD)
    c1AnalyzedData <- get(load(c1AnalyzedDataFilename))
    c2AnalyzedData <- get(load(c2AnalyzedDataFilename))

    if(!is.null(c1AnalyzedData$coefsCIs)||!is.null(c2AnalyzedData$coefsCIs)) {
            res <- computeDotProductBtwConditionsCoefs(
                    times1=c1AnalyzedData$times,
                    condition1Coefs=c1AnalyzedData$coefsCIs[-1,1], 
                    times2=c2AnalyzedData$times,
                    condition2Coefs=c2AnalyzedData$coefsCIs[-1,1],
                    nResamples=nResamples)
            dotProduct <- res$dotProduct
            pValue <- res$pValue
    } else {
        dotProduct <- NaN
        pValue <- NaN
    }
    return(c(dotProduct, pValue))
}
computeDotProductBtwConditionsCoefs <- function(times1, condition1Coefs, 
                                                 times2, condition2Coefs, 
                                                 nResamples) {
    matchRes <- match(x=times1, table=times2)
    c1CommonTimeIndices <- which(!is.na(matchRes))
    c2CommonTimeIndices <- matchRes[c1CommonTimeIndices]
    if(length(c1CommonTimeIndices)>2) {
        permRes <- permuteDotProduct(x=condition1Coefs[c1CommonTimeIndices],
                                      y=condition2Coefs[c2CommonTimeIndices],
                                      nResamples=nResamples)
        dotProduct <- permRes$t0
        pValue <- sum(abs(permRes$t)>abs(permRes$t0))/length(permRes$t)
    } else {
        dotProduct <- NaN
        pValue <- NaN
    }
    return(list(dotProduct=dotProduct, pValue=pValue))
}

