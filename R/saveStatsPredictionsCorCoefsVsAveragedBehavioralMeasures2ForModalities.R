saveStatsPredictionsCorCoefsVsAveragedBehavioralMeasures2ForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, nResamples, conf,
                   modelSignificance, errorRatesAndMeanRTsStats, 
                   getSubjectBehavioralMeasureCIFunc, 
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, 
                   statsFilenamePattern, ...) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        saveStatsPredictionsCorCoefsVsAveragedBehavioralMeasures2ForClusters(
         sortvar=sortvar,
         modality=modality,
         clustersIDs=clustersIDs,
         conditions=conditions,
         nResamples=nResamples,
         conf=conf,
         modelSignificance=modelSignificance,
         errorRatesAndMeanRTsStats=errorRatesAndMeanRTsStats,
         getSubjectBehavioralMeasureCIFunc=getSubjectBehavioralMeasureCIFunc,
         scFilenamePattern=scFilenamePattern,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         statsFilenamePattern=statsFilenamePattern, 
         ...)
    }
}
saveStatsPredictionsCorCoefsVsAveragedBehavioralMeasures2ForClusters <- 
 function(sortvar, modality, clustersIDs, conditions, nResamples, conf,
                   modelSignificance, errorRatesAndMeanRTsStats, 
                   getSubjectBehavioralMeasureCIFunc, 
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, 
                   statsFilenamePattern, ...) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID=clusterID,
                                            scFilename=scFilename)
        saveStatsPredictionsCorCoefsVsAveragedBehavioralMeasures2ForConditions(
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         conditions=conditions,
         nResamples=nResamples,  
         conf=conf,
         modelSignificance=modelSignificance,
         errorRatesAndMeanRTsStats=errorRatesAndMeanRTsStats,
         getSubjectBehavioralMeasureCIFunc=getSubjectBehavioralMeasureCIFunc,
         subjectsAndComponents=subjectsAndComponents,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         statsFilenamePattern=statsFilenamePattern,
         ...)
    }
}
saveStatsPredictionsCorCoefsVsAveragedBehavioralMeasures2ForConditions <- 
 function(sortvar, modality, clusterID, conditions, nResamples, conf,
                   modelSignificance, errorRatesAndMeanRTsStats,
                   getSubjectBehavioralMeasureCIFunc, 
                   subjectsAndComponents, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, 
                   statsFilenamePattern, ...) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        saveStatsPredictionsCorCoefsVsAveragedBehavioralMeasures2ForSubjects(
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         nResamples=nResamples,  
         conf=conf,
         modelSignificance=modelSignificance,
         errorRatesAndMeanRTsStats=errorRatesAndMeanRTsStats,
         getSubjectBehavioralMeasureCIFunc=getSubjectBehavioralMeasureCIFunc,
         subjectsAndComponents=subjectsAndComponents,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         statsFilenamePattern=statsFilenamePattern,
         ...)
    }
}
saveStatsPredictionsCorCoefsVsAveragedBehavioralMeasures2ForSubjects <- 
 function(sortvar, modality, clusterID, condition, nResamples, conf,
                   modelSignificance, errorRatesAndMeanRTsStats,
                   getSubjectBehavioralMeasureCIFunc, 
                   subjectsAndComponents, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, 
                   statsFilenamePattern, ...) {
    predSFPDurCors <- c()
    behavioralMeasures <- c()
    nSigPredictions <- 0
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
                if(!is.nan(modelSignificance) &&
                   !is.null(analyzedData$lrtRes) && 
                   analyzedData$lrtRes$pValue<modelSignificance) {
                    nSigPredictions <- nSigPredictions + 1
                }
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
        stats <- getStatsCorCoefValuesVsAveragedBehavioralMeasures(
                  values=predSFPDurCors, 
                  behavioralMeasures=behavioralMeasures,
                  nResamples=nResamples, 
                  conf=conf,...)
    } else {
        stats <- list()
    }
    nModels <- nrow(subjectsAndComponents)
    stats <- c(stats, list(nModels=nModels, nSigPredictions=nSigPredictions))
    statsFilename <- sprintf(statsFilenamePattern, clusterID, clusterID, 
                                                   condition, sortvar, 
                                                   modality)
    save(stats, file=statsFilename)
}
