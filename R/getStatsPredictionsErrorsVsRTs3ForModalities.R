getStatsPredictionsErrorsVsRTs3ForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, modelSignificance,
                   dsAndPreviousSTDsInfo,
                   rtsInfo,
                   dfpdsInfo,
                   maxRT, maxSTD_D_delay, maxDFPD, 
                   nResamples, conf,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    stats <- list()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        stats <- 
         c(stats, 
            list(list(modality=modality, 
                       stats=getStatsPredictionsErrorsVsRTs3ForClusters(
                              sortvar=sortvar,
                              modality=modality,
                              clustersIDs=clustersIDs,
                              conditions=conditions,
                              modelSignificance=modelSignificance,
                              dsAndPreviousSTDsInfo=dsAndPreviousSTDsInfo,
                              rtsInfo=rtsInfo,
                              dfpdsInfo=dfpdsInfo,
                              maxRT=maxRT,
                              maxSTD_D_delay=maxSTD_D_delay,
                              maxDFPD=maxDFPD,
                              nResamples=nResamples,
                              conf=conf,
                              scFilenamePattern=scFilenamePattern,
                              minAndMaxSFPDOfBestPredictionsFilenamePattern=
                               minAndMaxSFPDOfBestPredictionsFilenamePattern,
                              analyzedDataFilenamePattern=
                               analyzedDataFilenamePattern, ...))))
    }
    stats$modality <- factor(stats$modality)
    stats$clusterID <- factor(stats$clusterID)
    stats$condition <- factor(stats$condition)
    return(stats)
}
getStatsPredictionsErrorsVsRTs3ForClusters <- 
 function(sortvar, modality, clustersIDs, conditions, modelSignificance,
                   dsAndPreviousSTDsInfo,
                   rtsInfo,
                   dfpdsInfo,
                   maxRT, maxSTD_D_delay, maxDFPD, 
                   nResamples, conf,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    stats <- list()
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                            scFilename=scFilename)
        stats <- 
         c(stats, 
            list(list(clusterID=clusterID, 
                       stats=getStatsPredictionsErrorsVsRTs3ForConditions(
                              sortvar=sortvar,
                              modality=modality,
                              clusterID=clusterID,
                              conditions=conditions,
                              modelSignificance=modelSignificance,
                              dsAndPreviousSTDsInfo=dsAndPreviousSTDsInfo,
                              rtsInfo=rtsInfo,
                              dfpdsInfo=dfpdsInfo,
                              maxRT=maxRT,
                              maxSTD_D_delay=maxSTD_D_delay,
                              maxDFPD=maxDFPD,
                              nResamples=nResamples,
                              conf=conf,
                              subjectsAndComponents=subjectsAndComponents,
                              minAndMaxSFPDOfBestPredictionsFilenamePattern=
                               minAndMaxSFPDOfBestPredictionsFilenamePattern,
                              analyzedDataFilenamePattern=
                               analyzedDataFilenamePattern, ...))))
    }
    return(stats)
}
getStatsPredictionsErrorsVsRTs3ForConditions <- 
 function(sortvar, modality, clusterID, conditions, modelSignificance,
                   dsAndPreviousSTDsInfo,
                   rtsInfo,
                   dfpdsInfo,
                   maxRT, maxSTD_D_delay, maxDFPD, 
                   nResamples, conf,
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    stats <- list()
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        stats <- 
         c(stats, 
            list(list(condition=condition, 
                       stats=getStatsPredictionsErrorsVsRTs3ForSubjects(
                              sortvar=sortvar,
                              modality=modality,
                              clusterID=clusterID,
                              condition=condition,
                              modelSignificance=modelSignificance,
                              dsAndPreviousSTDsInfo=dsAndPreviousSTDsInfo,
                              rtsInfo=rtsInfo,
                              dfpdsInfo=dfpdsInfo,
                              maxRT=maxRT,
                              maxSTD_D_delay=maxSTD_D_delay,
                              maxDFPD=maxDFPD,
                              nResamples=nResamples, 
                              conf=conf,
                              subjectsAndComponents=subjectsAndComponents,
                              minAndMaxSFPDOfBestPredictionsFilenamePattern=
                               minAndMaxSFPDOfBestPredictionsFilenamePattern,
                              analyzedDataFilenamePattern=
                               analyzedDataFilenamePattern, ...))))
    }
    return(stats)
}

# We have four dataFrames:
# rtsDF:               dID, rt
# dfpdsDF:             dID, dfpd
# dsAndPreviousSTDsDF: dID, stdID, stdDDelay
# analyzedDataDF:      stdID, sfpd, predictionError
#
# We want to join them into stdsDeviantsDF:
# stdsDeviantsDF:      stdID, dID, rt, dfpd, predictionError, stdDDelay
# and subset it with rt<maxRT, stdDDelay<maxSTD_D_delay, dfpd<maxDFPD
getStatsPredictionsErrorsVsRTs3ForSubjects <- 
 function(sortvar, modality, clusterID, condition, modelSignificance,
                   dsAndPreviousSTDsInfo,
                   rtsInfo, 
                   dfpdsInfo, 
                   maxRT, maxSTD_D_delay, maxDFPD,
                   nResamples, conf, 
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    stats <- list()
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
            if(!is.null(analyzedData$lrtRes) &&
               analyzedData$lrtRes$pValue<modelSignificance) {
                predictionErrors <-
                 abs(analyzedData$predictions-analyzedData$data[,1])
                analyzedDataDF <- data.frame(stdID=analyzedData$epochEventIDs,
                                              sfpd=analyzedData$data[,1],
                                              predictionError=predictionErrors)

                rtsTable <- 
                 getItemInAVShiftList(listOfItems=rtsInfo,
                                       listFieldName="rtsInfo",
                                       keyFieldNames=c("subjectName",
                                                       "condition"),
                                       keyFieldValues=c(subjectName,
                                                         condition))
                rtsDF <- data.frame(dID=rtsTable[,1], rt=rtsTable[,2])

                dfpdsTable <-
                 getItemInAVShiftList(listOfItems=dfpdsInfo,
                                       listFieldName="dfpdsInfo",
                                       keyFieldNames=c("subjectName", 
                                                       "condition"),
                                       keyFieldValues=c(subjectName, condition))
                dfpdsDF <- data.frame(dID=dfpdsTable[,1], dfpd=dfpdsTable[,2])

                dsAndPreviousSTDsTable <- 
                 getItemInAVShiftList(listOfItems=dsAndPreviousSTDsInfo,
                                       listFieldName="dsAndPreviousSTDsInfo",
                                       keyFieldNames=c("subjectName",
                                                       "modality",
                                                       "condition"),
                                       keyFieldValues=c(subjectName,
                                                         modality,
                                                         condition))
                dsAndPreviousSTDsTable <- dsAndPreviousSTDsTable[which(!is.na(dsAndPreviousSTDsTable[,2])),]
                dsAndPreviousSTDsDF <-
                 data.frame(dID=dsAndPreviousSTDsTable[,1], 
                             stdID=dsAndPreviousSTDsTable[,2],
                             stdDDelay=dsAndPreviousSTDsTable[,3])

                stdsDeviantsDF <- merge(x=rtsDF, y=dfpdsDF, by="dID")
                stdsDeviantsDF <- merge(x=stdsDeviantsDF, 
                                         y=dsAndPreviousSTDsDF, 
                                         by="dID")
                stdsDeviantsDF <- merge(x=stdsDeviantsDF, y=analyzedDataDF, 
                                                          by="stdID")
                subsetSTDsDeviantsDF <- subset(stdsDeviantsDF, 
                                                rt<maxRT & 
                                                stdDDelay<maxSTD_D_delay & 
                                                dfpd<maxDFPD)
                stats <- 
                 c(stats, 
                    list(list(subjectName=subjectName,
                               component=component,
                               stats=getStatsPredictionsErrorsVsRTs3(
                                      predictionErrors=subsetSTDsDeviantsDF$predictionError,
                                      rts=subsetSTDsDeviantsDF$rt,
                                      dfpds=subsetSTDsDeviantsDF$dfpd,
                                      maxRT=maxRT,
                                      maxDFPD=maxDFPD,
                                      nResamples=nResamples, 
                                      conf=conf,
                                      ...))))
            }
        }
    }
    return(stats)
}
