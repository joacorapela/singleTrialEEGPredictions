
computeNCrossCorrelationBtwConditionsCoefsForModalities <- 
 function(sortvar, modalities, clustersIDs, condition1, condition2, 
                   minOverlapMS, srate,
                   modelSignificance, nResamples, scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    answer <- c()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        res <- 
         computeNCrossCorrelationBtwConditionsCoefsForClusters(
          sortvar=sortvar,
          modality=modality,
          clustersIDs=clustersIDs,
          condition1=condition1,
          condition2=condition2,
          minOverlapMS=minOverlapMS,,
          srate=srate,,
          modelSignificance=modelSignificance,
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
computeNCrossCorrelationBtwConditionsCoefsForClusters <- 
 function(sortvar, modality, clustersIDs, condition1, condition2,
                   minOverlapMS, srate,
                   modelSignificance, nResamples,
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
         computeNCrossCorrelationBtwConditionsCoefsForSubjectsAndComponents(
          sortvar=sortvar,
          modality=modality,
          clusterID=clusterID,
          condition1=condition1,
          condition2=condition2,
          minOverlapMS=minOverlapMS,,
          srate=srate,,
          modelSignificance=modelSignificance,
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
computeNCrossCorrelationBtwConditionsCoefsForSubjectsAndComponents <- 
 function(sortvar, modality, clusterID, condition1, condition2, 
                   minOverlapMS, srate,
                   modelSignificance, nResamples, 
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    subjectsNames <- c()
    components <- c()
    correlations <- c()
    pValues <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
         res <- 
          computeNCrossCorrelationBtwConditionsCoefsForSubjectAndComponent(
           sortvar=sortvar,
           modality=modality,
           clusterID=clusterID,
           condition1=condition1,
           condition2=condition2,
           minOverlapMS=minOverlapMS,,
           srate=srate,,
           subjectName=subjectName,
           component=component,
           modelSignificance=modelSignificance,
           nResamples=nResamples,
           minAndMaxSFPDOfBestPredictionsFilenamePattern=
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
           analyzedDataFilenamePattern=analyzedDataFilenamePattern,
           ...)
        if(!is.null(res)) {
            subjectsNames <- c(subjectsNames, subjectName)
            components <- c(components, component)
            correlations <- c(correlations, res[1])
            pValues <- c(pValues, res[2])
        }
    }
    if(length(subjectsNames)>0) {
        return(data.frame(subjectName=subjectsNames, component=components,
                                                     correlation=correlations,
                                                     pValue=pValues))
    } else {
        return(data.frame())
    }
}
computeNCrossCorrelationBtwConditionsCoefsForSubjectAndComponent <- 
 function(sortvar, modality, clusterID, condition1, condition2,
                   minOverlapMS, srate,
                   subjectName, component, modelSignificance,
                   nResamples, minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
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
    if(!is.na(c1MinSFPD) && !is.na(c1MaxSFPD) && 
       !is.na(c2MinSFPD) && !is.na(c2MaxSFPD)) {
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

        if(!is.null(c1AnalyzedData$lrtRes) &&
           c1AnalyzedData$lrtRes$pValue<modelSignificance && 
           !is.null(c2AnalyzedData$lrtRes) && 
           c2AnalyzedData$lrtRes$pValue<modelSignificance) {
            res <- computeNCrossCorrelationBtwConditionsCoefs(
                    times1=c1AnalyzedData$times,
                    condition1Coefs=c1AnalyzedData$coefsCIs[-1,1], 
                    times2=c2AnalyzedData$times,
                    minOverlapMS=minOverlapMS,
                    srate=srate,
                    condition2Coefs=c2AnalyzedData$coefsCIs[-1,1],
                    nResamples=nResamples)
            correlation <- res$correlation
            pValue <- res$pValue
            return(c(correlation, pValue))
        }
    }
    return(NULL)
}
computeNCrossCorrelationBtwConditionsCoefs <- function(times1, condition1Coefs, 
                                                 times2, condition2Coefs, 
                                                 minOverlapMS, srate,
                                                 nResamples) {
    matchRes <- match(x=times1, table=times2)
    c1CommonTimeIndices <- which(!is.na(matchRes))
    c2CommonTimeIndices <- matchRes[c1CommonTimeIndices]
    minNumberOfCommonIndices <- minOverlapMS*srate/1000
    if(length(c1CommonTimeIndices)>=minNumberOfCommonIndices) {
        permRes <- 
         permuteNCorrelation(x=condition1Coefs[c1CommonTimeIndices],
                              y=condition2Coefs[c2CommonTimeIndices],
                              nResamples=nResamples)
        correlation <- permRes$t0
        pValue <- sum(abs(permRes$t)>abs(permRes$t0))/length(permRes$t)
    } else {
        correlation <- NaN
        pValue <- NaN
    }
    return(list(correlation=correlation, pValue=pValue))
}

