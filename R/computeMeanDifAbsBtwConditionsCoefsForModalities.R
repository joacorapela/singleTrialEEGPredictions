
computeMeanDifAbsBtwConditionsCoefsForModalities <- 
 function(sortvar, modalities, clustersIDs, condition1, condition2, 
                   modelSignificance, nResamples, scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    answer <- c()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        res <- 
         computeMeanDifAbsBtwConditionsCoefsForClusters(
          sortvar=sortvar,
          modality=modality,
          clustersIDs=clustersIDs,
          condition1=condition1,
          condition2=condition2,
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
computeMeanDifAbsBtwConditionsCoefsForClusters <- 
 function(sortvar, modality, clustersIDs, condition1, condition2,
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
         computeMeanDifAbsBtwConditionsCoefsForSubjectsAndComponents(
          sortvar=sortvar,
          modality=modality,
          clusterID=clusterID,
          condition1=condition1,
          condition2=condition2,
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
computeMeanDifAbsBtwConditionsCoefsForSubjectsAndComponents <- 
 function(sortvar, modality, clusterID, condition1, condition2, 
                   modelSignificance, nResamples, 
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    subjectsNames <- c()
    components <- c()
    meanDifAbs <- c()
    pValues <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
         res <- 
          computeMeanDifAbsBtwConditionsCoefsForSubjectAndComponent(
           sortvar=sortvar,
           modality=modality,
           clusterID=clusterID,
           condition1=condition1,
           condition2=condition2,
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
            meanDifAbs <- c(meanDifAbs, res[1])
            pValues <- c(pValues, res[2])
        }
    }
    if(length(subjectsNames)>0) {
        return(data.frame(subjectName=subjectsNames, component=components,
                                                     meanDifAbs=meanDifAbs,
                                                     pValue=pValues))
    } else {
        return(data.frame())
    }
}
computeMeanDifAbsBtwConditionsCoefsForSubjectAndComponent <- 
 function(sortvar, modality, clusterID, condition1, condition2,
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
            res <- permuteDifferenceMeans(
                    x=abs(c1AnalyzedData$coefsCIs[-1,1]), 
                    y=abs(c2AnalyzedData$coefsCIs[-1,1]), 
                    nResamples=nResamples)
            meanDifAbs <- res$t0
            pValue <- mean(abs(res$t)>abs(res$t0))
            return(c(meanDifAbs, pValue))
        }
    }
    return(NULL)
}
