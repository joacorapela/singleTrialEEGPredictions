
getANOVADataFrame2ForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, modelSignificance,
                   getANOVADataForSubjectFunc,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    dataFrame <- c()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        mDataFrame <- getANOVADataFrame2ForClusters(
                   sortvar=sortvar,
                   modality=modality,
                   clustersIDs=clustersIDs,
                   conditions=conditions,
                   modelSignificance=modelSignificance,
                   getANOVADataForSubjectFunc=getANOVADataForSubjectFunc,
                   scFilenamePattern=scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern=
                    minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern=
                    analyzedDataFilenamePattern)
        if(length(mDataFrame)>0) {
            mDataFrame["modality"] <- modality
            dataFrame <- rbind(dataFrame, mDataFrame)
        }
    }
    dataFrame$modality  <- as.factor(dataFrame$modality)
    dataFrame$clusterID <- as.factor(dataFrame$clusterID)
    dataFrame$condition <- as.factor(dataFrame$condition)
    dataFrame$subject   <- as.factor(dataFrame$subject)
    return(dataFrame)
}
getANOVADataFrame2ForClusters <- 
 function(sortvar, modality, clustersIDs, conditions, modelSignificance,
                   getANOVADataForSubjectFunc,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    dataFrame <- list()
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                            scFilename=scFilename)
        cDataFrame <- getANOVADataFrame2ForConditions(
                   sortvar=sortvar,
                   modality=modality,
                   clusterID=clusterID,
                   conditions=conditions,
                   modelSignificance=modelSignificance,
                   getANOVADataForSubjectFunc=getANOVADataForSubjectFunc,
                   subjectsAndComponents=subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern=
                    minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern=
                    analyzedDataFilenamePattern)
        if(length(cDataFrame)>0) {
            cDataFrame["clusterID"] <- clusterID
            dataFrame <- rbind(dataFrame, cDataFrame)
        }
    }
    return(dataFrame)
}
getANOVADataFrame2ForConditions <- 
 function(sortvar, modality, clusterID, conditions, modelSignificance,
                   getANOVADataForSubjectFunc,
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    dataFrame <- list()
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        cDataFrame <- getANOVADataFrame2ForSubjects(
                   sortvar=sortvar,
                   modality=modality,
                   clusterID=clusterID,
                   condition=condition,
                   modelSignificance=modelSignificance,
                   getANOVADataForSubjectFunc=getANOVADataForSubjectFunc,
                   subjectsAndComponents=subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern=
                    minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern=
                    analyzedDataFilenamePattern)
        if(length(cDataFrame)>0) {
            cDataFrame["condition"] <- condition
            dataFrame <- rbind(dataFrame, cDataFrame)
        }
    }
    return(dataFrame)
}
getANOVADataFrame2ForSubjects <- 
 function(sortvar, modality, clusterID, condition, modelSignificance,
                   getANOVADataForSubjectFunc,
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern) {
    dataFrame <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
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
# if(subjectName=="av112a" && component==7) {
#     browser()
# }
            if(!is.null(analyzedData$lrtRes) &&
               analyzedData$lrtRes$pValue<modelSignificance) {
                sDataFrame <- getANOVADataForSubjectFunc(analyzedData=
                                                           analyzedData, 
                                                          minSFPD=minSFPD,
                                                          maxSFPD=maxSFPD)
                sDataFrame["subject"] <- subjectName
                sDataFrame["component"] <- component
                dataFrame <- rbind(dataFrame, sDataFrame)
            }
        }
    }
    return(dataFrame)
}
