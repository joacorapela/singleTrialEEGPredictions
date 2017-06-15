
computeCCF0BtwConditionsCoefsForModalities <- 
 function(sortvar, modalities, clustersIDs, condition1, condition2, 
                   minOverlapMS, srate,
                   modelSignificance, nResamples, scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    answer <- c()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        res <- 
         computeCCF0BtwConditionsCoefsForClusters(
          sortvar=sortvar,
          modality=modality,
          clustersIDs=clustersIDs,
          condition1=condition1,
          condition2=condition2,
          minOverlapMS=minOverlapMS,
          srate=srate,
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
computeCCF0BtwConditionsCoefsForClusters <- 
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
         computeCCF0BtwConditionsCoefsForSubjectsAndComponents(
          sortvar=sortvar,
          modality=modality,
          clusterID=clusterID,
          condition1=condition1,
          condition2=condition2,
          minOverlapMS=minOverlapMS,
          srate=srate,
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
computeCCF0BtwConditionsCoefsForSubjectsAndComponents <- 
 function(sortvar, modality, clusterID, condition1, condition2, 
                   minOverlapMS, srate,
                   modelSignificance, nResamples, 
                   subjectsAndComponents=subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    subjectsNames <- c()
    components <- c()
    ccf0s <- c()
    pValues <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
         res <- 
          computeCCF0BtwConditionsCoefsForSubjectAndComponent(
           sortvar=sortvar,
           modality=modality,
           clusterID=clusterID,
           condition1=condition1,
           condition2=condition2,
           minOverlapMS=minOverlapMS,
           srate=srate,
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
            ccf0s <- c(ccf0s, res[1])
            pValues <- c(pValues, res[2])
        }
    }
    if(length(subjectsNames)>0) {
        return(data.frame(subjectName=subjectsNames, component=components,
                                                     ccf0=ccf0s,
                                                     pValue=pValues))
    } else {
        return(data.frame())
    }
}
computeCCF0BtwConditionsCoefsForSubjectAndComponent <- 
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
        res <- computeCCF0BtwConditionsCoefs(
                times1=c1AnalyzedData$times,
                condition1Coefs=c1AnalyzedData$coefsCIs[-1,1], 
                times2=c2AnalyzedData$times,
                condition2Coefs=c2AnalyzedData$coefsCIs[-1,1],
                minOverlapMS=minOverlapMS,
                srate=srate,
                nResamples=nResamples)
        ccf0 <- res$ccf0
        pValue <- res$pValue
        return(c(ccf0, pValue))
    }
    return(NULL)
}
computeCCF0BtwConditionsCoefs <- function(times1, condition1Coefs, 
                                                 times2, condition2Coefs, 
                                                 minOverlapMS, srate,
                                                 nResamples) {
    matchRes <- match(x=times1, table=times2)
    c1CommonTimeIndices <- which(!is.na(matchRes))
    c2CommonTimeIndices <- matchRes[c1CommonTimeIndices]
    minNumberOfCommonIndices <- minOverlapMS*srate/1000
    if(length(c1CommonTimeIndices)>=minNumberOfCommonIndices) {
        permRes <- permuteCCF0(x=condition1Coefs[c1CommonTimeIndices],
                                      y=condition2Coefs[c2CommonTimeIndices],
                                      nResamples=nResamples)
        ccf0 <- permRes$t0
        pValue <- sum(abs(permRes$t)>abs(permRes$t0))/length(permRes$t)
    } else {
        ccf0 <- NaN
        pValue <- NaN
    }
    return(list(ccf0=ccf0, pValue=pValue))
}

