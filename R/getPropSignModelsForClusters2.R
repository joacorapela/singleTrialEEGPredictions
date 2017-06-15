
getPropSignModelsForClusters2 <- 
 function(sortvar, modality, clustersIDs, condition, modelSignificance, 
                   scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    conditionProps <- c()
    for(clusterID in clustersIDs) {
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                            scFilename=scFilename)
        nSignModels <- 
         getNSignModelsForSubjects2(sortvar=sortvar, 
                                     modality=modality,
                                     clusterID=clusterID,
                                     condition=condition,
                                     modelSignificance=modelSignificance,
                                     subjectsAndComponents=
                                      subjectsAndComponents,
                                     minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                      minAndMaxSFPDOfBestPredictionsFilenamePattern,
                                     analyzedDataFilenamePattern=
                                      analyzedDataFilenamePattern)
        clusterProps <- nSignModels/nrow(subjectsAndComponents)
        conditionProps <- c(conditionProps, clusterProps)
    }
    return(conditionProps)
}

getNSignModelsForSubjects2 <- 
 function(sortvar, modality, clusterID, condition, modelSignificance, 
                   subjectsAndComponents, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    nSignModels <- 0
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
        if(!is.na(minSFPD) && !is.na(maxSFPD)) {
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
            if(!is.null(analyzedData$lrtRes) &&
                analyzedData$lrtRes$pValue<modelSignificance) {
                nSignModels <- nSignModels + 1
            }
        }
    }
    return(nSignModels)
}
