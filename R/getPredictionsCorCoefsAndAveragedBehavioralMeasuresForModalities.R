getPredictionsCorCoefsAndAveragedBehavioralMeasuresForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions,
                   errorRatesAndMeanRTsStats, 
                   getSubjectBehavioralMeasureCIFunc, 
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    predCorCoefsAndABMs <- list()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        mPredCorCoefsAndABMs <- getPredictionsCorCoefsAndAveragedBehavioralMeasuresForClusters(
                                 sortvar=sortvar,
                                 modality=modality,
                                 clustersIDs=clustersIDs,
                                 conditions=conditions,
                                 errorRatesAndMeanRTsStats=errorRatesAndMeanRTsStats,
                                 getSubjectBehavioralMeasureCIFunc=getSubjectBehavioralMeasureCIFunc,
                                 scFilenamePattern=scFilenamePattern,
                                 minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                  minAndMaxSFPDOfBestPredictionsFilenamePattern,
                                 analyzedDataFilenamePattern=analyzedDataFilenamePattern)
        if(length(mPredCorCoefsAndABMs)>0) {
            predCorCoefsAndABMs <- c(predCorCoefsAndABMs, mPredCorCoefsAndABMs)
        }
    }
    return(predCorCoefsAndABMs)
}
getPredictionsCorCoefsAndAveragedBehavioralMeasuresForClusters <- 
 function(sortvar, modality, clustersIDs, conditions,
                   errorRatesAndMeanRTsStats, 
                   getSubjectBehavioralMeasureCIFunc, 
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    predCorCoefsAndABMs <- list()
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID=clusterID,
                                            scFilename=scFilename)
        cPredCorCoefsAndABMs <-getPredictionsCorCoefsAndAveragedBehavioralMeasuresForConditions(
                                sortvar=sortvar,
                                modality=modality,
                                clusterID=clusterID,
                                conditions=conditions,
                                errorRatesAndMeanRTsStats=errorRatesAndMeanRTsStats,
                                getSubjectBehavioralMeasureCIFunc=
                                 getSubjectBehavioralMeasureCIFunc,
                                subjectsAndComponents=subjectsAndComponents,
                                minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                 minAndMaxSFPDOfBestPredictionsFilenamePattern,
                                analyzedDataFilenamePattern=
                                 analyzedDataFilenamePattern)
        if(length(cPredCorCoefsAndABMs)>0) {
            predCorCoefsAndABMs <- c(predCorCoefsAndABMs, cPredCorCoefsAndABMs)
        }
    }
    return(predCorCoefsAndABMs)
}
getPredictionsCorCoefsAndAveragedBehavioralMeasuresForConditions <- 
 function(sortvar, modality, clusterID, conditions,
                   errorRatesAndMeanRTsStats,
                   getSubjectBehavioralMeasureCIFunc, 
                   subjectsAndComponents, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    predCorCoefsAndABMs <- list()
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        cPredCorCoefsAndABMs <- getPredictionsCorCoefsAndAveragedBehavioralMeasuresForSubjects(
                                 sortvar=sortvar,
                                 modality=modality,
                                 clusterID=clusterID,
                                 condition=condition,
                                 errorRatesAndMeanRTsStats=
                                  errorRatesAndMeanRTsStats,
                                 getSubjectBehavioralMeasureCIFunc=
                                  getSubjectBehavioralMeasureCIFunc,
                                 subjectsAndComponents=subjectsAndComponents,
                                 minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                  minAndMaxSFPDOfBestPredictionsFilenamePattern,
                                 analyzedDataFilenamePattern=
                                  analyzedDataFilenamePattern)
        if(length(cPredCorCoefsAndABMs)>0) {
            predCorCoefsAndABMs <- c(predCorCoefsAndABMs, 
                                      list(cPredCorCoefsAndABMs))
        }
    }
    return(predCorCoefsAndABMs)
}
getPredictionsCorCoefsAndAveragedBehavioralMeasuresForSubjects <- 
 function(sortvar, modality, clusterID, condition,
                   errorRatesAndMeanRTsStats,
                   getSubjectBehavioralMeasureCIFunc, 
                   subjectsAndComponents, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    predSFPDurCors <- c()
    behavioralMeasures <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        minAndMaxSFPDOfBestPredictionsFilename <- 
         sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, clusterID,
                  clusterID, condition, sortvar, modality, subjectName, 
                  component)
        res <- readLines(minAndMaxSFPDOfBestPredictionsFilename)
        minSFPD <- as.integer(res[1])
        maxSFPD <- as.integer(res[2])
        if(is.nan(minSFPD) || is.na(maxSFPD)) {
            predSFPDurCors <- c(predSFPDurCors, 0)
        } else {
            analyzedDataFilename <- sprintf(analyzedDataFilenamePattern,
                                             clusterID,
                                             clusterID,
                                             condition,
                                             sortvar,
                                             modality,
                                             subjectName,
                                             component,
                                             minSFPD,
                                             maxSFPD)
            analyzedData <- get(load(analyzedDataFilename))

            if(!is.null(analyzedData$predSFPDurCorCI)) {
                predSFPDurCorCI <- analyzedData$predSFPDurCorCI
                predSFPDurCors <- c(predSFPDurCors, predSFPDurCorCI[1])
            } else {
                predSFPDurCors <- c(predSFPDurCors, 0)
            }
        }
        behavioralMeasure <- getSubjectBehavioralMeasureCIFunc(
                              subjectName=subjectName, 
                              condition=condition, 
                              stats=errorRatesAndMeanRTsStats)[1]
        behavioralMeasures <- c(behavioralMeasures, behavioralMeasure)
    }
    if(length(behavioralMeasures)>2) {
        metaData <- list(modality=modality, clusterID=clusterID,
                                            condition=condition)
        return(list(x=behavioralMeasures, y=predSFPDurCors, metaData=metaData))
    }
    return(list())
}
