
getPredictionsAndSFPDsForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, 
                   modelSignificance,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    datasets <- list()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        mDataSet <- getPredictionsAndSFPDsForClusters(
                     sortvar=sortvar,
                     modality=modality,
                     clustersIDs=clustersIDs,
                     conditions=conditions,
                     modelSignificance=modelSignificance, 
                     scFilenamePattern=scFilenamePattern,
                     minAndMaxSFPDOfBestPredictionsFilenamePattern=
                      minAndMaxSFPDOfBestPredictionsFilenamePattern,
                     analyzedDataFilenamePattern=analyzedDataFilenamePattern,
                     ...)
        datasets <- c(datasets, mDataSet)
    }
    return(datasets)
}

getPredictionsAndSFPDsForClusters <- 
 function(sortvar, modality, clustersIDs, conditions, 
                   modelSignificance,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    datasets <- list()
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %02d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                            scFilename=scFilename)
        cDataSet <- getPredictionsAndSFPDsForConditions(
                     sortvar=sortvar,
                     modality=modality,
                     clusterID=clusterID,
                     conditions=conditions,
                     modelSignificance=modelSignificance, 
                     subjectsAndComponents=subjectsAndComponents,
                     minAndMaxSFPDOfBestPredictionsFilenamePattern=
                      minAndMaxSFPDOfBestPredictionsFilenamePattern,
                     analyzedDataFilenamePattern=
                      analyzedDataFilenamePattern,
                     ...)
        datasets <- c(datasets, cDataSet)
    }
    return(datasets)
}

getPredictionsAndSFPDsForConditions <- 
 function(sortvar, modality, clusterID, conditions, 
                   modelSignificance,
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    datasets <- list()
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        cDataSet <- getPredictionsAndSFPDsForSubjectsAndComponents(
                     sortvar=sortvar,
                     modality=modality,
                     clusterID=clusterID,
                     condition=condition,
                     subjectsAndComponents=subjectsAndComponents,
                     modelSignificance=modelSignificance, 
                     minAndMaxSFPDOfBestPredictionsFilenamePattern=
                      minAndMaxSFPDOfBestPredictionsFilenamePattern,
                     analyzedDataFilenamePattern=analyzedDataFilenamePattern,
                     ...)
        datasets <- c(datasets, cDataSet)
    }
    return(datasets)
}
getPredictionsAndSFPDsForSubjectsAndComponents <- 
 function(sortvar, modality, clusterID, condition, 
                   subjectsAndComponents, 
                   modelSignificance,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    datasets <- list()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", 
                     subjectName, component))
        minAndMaxSFPDOfBestPredictionsFilename <- 
         sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, 
                  clusterID, clusterID, condition, sortvar, modality, 
                  subjectName, component)
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

                metaData <- list(modality=modality, clusterID=clusterID,
                                                    condition=condition, 
                                                    subjectName=subjectName,
                                                    component=component)
                cDataset <- list(x=analyzedData$predictions, 
                                  y=analyzedData$data[,1], 
                                  metaData=metaData)
                datasets <- c(datasets, list(cDataset))
            }
        }
    }
    return(datasets)
}
