
getStatsPredCorCoefs2ForModalities <- 
 function(sortvar, modalities, conditions, clustersIDs, modelSignificance,
                   nResamples, ciConf,
                   scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    answer <- c()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        stats <- getStatsPredCorCoefs2ForConditions(
                        sortvar=sortvar,
                        modality=modality,
                        conditions=conditions,
                        clustersIDs=clustersIDs,
                        modelSignificance=modelSignificance,
                        nResamples=nResamples,
                        ciConf=ciConf,
                        scFilenamePattern=scFilenamePattern,
                        minAndMaxSFPDOfBestPredictionsFilenamePattern=
                         minAndMaxSFPDOfBestPredictionsFilenamePattern,
                        analyzedDataFilenamePattern=
                         analyzedDataFilenamePattern)
        answer <- c(answer, list(list(modality=modality,
                                       stats=stats)))
    }
    return(answer)
}
getStatsPredCorCoefs2ForConditions <- 
 function(sortvar, modality, conditions, clustersIDs, 
                   modelSignificance, nResamples, ciConf,
                   scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    answer <- c()
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        predCorCoefs <- getPredCorCoefs2ForClusters(
                   sortvar=sortvar,
                   modality=modality,
                   condition=condition,
                   clustersIDs=clustersIDs,
                   modelSignificance=modelSignificance,
                   scFilenamePattern=scFilenamePattern,
                    minAndMaxSFPDOfBestPredictionsFilenamePattern=
                     minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern=
                    analyzedDataFilenamePattern)
        bootRes <- bootstrapMean(predCorCoefs, nResamples=nResamples)
        bootCI <- getBootstrapCIs(bootRes=bootRes, conf=ciConf)
        answer <- c(answer, list(list(condition=condition,
                                       stats=list(range=range(predCorCoefs),
                                                   mean=bootCI[1],
                                                   meanCIL=bootCI[2],
                                                   meanCIU=bootCI[3]))))
    }
    return(answer)
}
getPredCorCoefs2ForClusters <- 
 function(sortvar, modality, condition, clustersIDs, 
                   modelSignificance,
                   scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    predCorCoefs <- c()
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %02d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                            scFilename=scFilename)
        subjectsPredCorCoefs <- getPredCorCoefs2ForSubjects(
                   sortvar=sortvar,
                   modality=modality,
                   condition=condition,
                   clusterID=clusterID,
                   modelSignificance=modelSignificance,
                   subjectsAndComponents=subjectsAndComponents,
                    minAndMaxSFPDOfBestPredictionsFilenamePattern=
                     minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern=
                    analyzedDataFilenamePattern)
        predCorCoefs <- c(predCorCoefs, subjectsPredCorCoefs)
    }
    return(predCorCoefs)
}
getPredCorCoefs2ForSubjects <- 
 function(sortvar, modality, condition, clusterID, 
                   modelSignificance,
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    predCorCoefs <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
#         show(sprintf("Processing subject %s and component %02d", subjectName,
#                      component))
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
                predCorCoefs <- c(predCorCoefs, analyzedData$predSFPDurCorCI[1])
            }
        }
    }
    return(predCorCoefs)
}
