
getPredictionsAndSFPDsBySubject <- 
 function(sortvar, subjects, modalities, clustersIDs, conditions, 
                   modelSignificance, 
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    datasets <- list()
    for(i in 1:length(subjects)) {
        show(sprintf("Processing subject %s", subjects[i]))
        sDataSet <- getPredictionsAndSFPDsBySubjectForModalities(
                     sortvar=sortvar,
                     subject=subjects[i],
                     modalities=modalities,
                     clustersIDs=clustersIDs,
                     conditions=conditions,
                     modelSignificance=modelSignificance, 
                     scFilenamePattern=scFilenamePattern,
                     minAndMaxSFPDOfBestPredictionsFilenamePattern=
                      minAndMaxSFPDOfBestPredictionsFilenamePattern,
                     analyzedDataFilenamePattern=analyzedDataFilenamePattern,
                     ...)
        datasets <- c(datasets, list(list(subjectName=subjects[i], 
                                           datasets=sDataSet)))
    }
    return(datasets)
}

getPredictionsAndSFPDsBySubjectForModalities <- 
 function(sortvar, subject, modalities, clustersIDs, conditions, 
                   modelSignificance,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    datasets <- list()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        mDataSet <- getPredictionsAndSFPDsBySubjectForClusters(
                     sortvar=sortvar,
                     subject=subject,
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

getPredictionsAndSFPDsBySubjectForClusters <- 
 function(sortvar, subject, modality, clustersIDs, conditions, 
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
        indices <- which(subjectsAndComponents[,1]==subject)
        components <- subjectsAndComponents[indices,2]
        if(length(components)>0) {
            cDataSet <- getPredictionsAndSFPDsBySubjectForConditions(
                         sortvar=sortvar,
                         subject=subject,
                         modality=modality,
                         clusterID=clusterID,
                         conditions=conditions,
                         modelSignificance=modelSignificance, 
                         components=components,
                         minAndMaxSFPDOfBestPredictionsFilenamePattern=
                          minAndMaxSFPDOfBestPredictionsFilenamePattern,
                         analyzedDataFilenamePattern=
                          analyzedDataFilenamePattern,
                         ...)
            datasets <- c(datasets, cDataSet)
        }
    }
    return(datasets)
}

getPredictionsAndSFPDsBySubjectForConditions <- 
 function(sortvar, subject, modality, clusterID, conditions, 
                   modelSignificance,
                   components,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    datasets <- list()
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        cDataSet <- getPredictionsAndSFPDsBySubjectForComponents(
                     sortvar=sortvar,
                     subject=subject,
                     modality=modality,
                     clusterID=clusterID,
                     condition=condition,
                     components=components,
                     modelSignificance=modelSignificance, 
                     minAndMaxSFPDOfBestPredictionsFilenamePattern=
                      minAndMaxSFPDOfBestPredictionsFilenamePattern,
                     analyzedDataFilenamePattern=analyzedDataFilenamePattern,
                     ...)
        datasets <- c(datasets, cDataSet)
    }
    return(datasets)
}
getPredictionsAndSFPDsBySubjectForComponents <- 
 function(sortvar, subject, modality, clusterID, condition, components, 
                   modelSignificance,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    datasets <- list()
    for(i in 1:length(components)) {
        subjectName <- subject
        component <- components[i]
        show(sprintf("Processing component %02d", component))
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
