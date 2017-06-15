
plotMediansOfPredictionsErrorsForHitsAndMisses2ForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions,
                   getStatsDifAnnotationFunction,
                   rtsForSTDsInfo,
                   dfpdsForSTDsInfo,
                   maxRT, 
                   maxDFPD,
                   nResamples, conf,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, 
                   plotsFilenamePattern,
                   width, height,
                   ...) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotMediansOfPredictionsErrorsForHitsAndMisses2ForClusters(
         sortvar=sortvar, 
         modality=modality,
         clustersIDs=clustersIDs, 
         conditions=conditions,
         getStatsDifAnnotationFunction=getStatsDifAnnotationFunction,
         rtsForSTDsInfo=rtsForSTDsInfo,
         dfpdsForSTDsInfo=dfpdsForSTDsInfo,
         maxRT=maxRT,
         maxDFPD=maxDFPD,
         nResamples=nResamples,
         conf=conf,
         scFilenamePattern=scFilenamePattern,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         width=width, height=height,
         ...)
    }
}

plotMediansOfPredictionsErrorsForHitsAndMisses2ForClusters <- 
 function(sortvar, modality, clustersIDs, conditions,
                   getStatsDifAnnotationFunction,
                   rtsForSTDsInfo,
                   dfpdsForSTDsInfo,
                   maxRT, 
                   maxDFPD, 
                   nResamples, conf,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   plotsFilenamePattern,
                   width, height,
                   ...) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing clusterID %s", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                            scFilename=scFilename)
        plotMediansOfPredictionsErrorsForHitsAndMisses2ForConditions(
         sortvar=sortvar, 
         modality=modality,
         clusterID=clusterID, 
         conditions=conditions,
         getStatsDifAnnotationFunction=getStatsDifAnnotationFunction,
         rtsForSTDsInfo=rtsForSTDsInfo,
         dfpdsForSTDsInfo=dfpdsForSTDsInfo,
         maxRT=maxRT,
         maxDFPD=maxDFPD,
         nResamples=nResamples,
         conf=conf,
         subjectsAndComponents=subjectsAndComponents,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         width=width, height=height,
         ...)
    }
}

plotMediansOfPredictionsErrorsForHitsAndMisses2ForConditions <- 
 function(sortvar, modality, clusterID, conditions,
                   getStatsDifAnnotationFunction,
                   rtsForSTDsInfo,
                   dfpdsForSTDsInfo,
                   maxRT, 
                   maxDFPD,
                   nResamples,
                   conf,
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   plotsFilenamePattern,
                   width, height,
                   ...) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        plotMediansOfPredictionsErrorsForHitsAndMisses2ForSubjects(
         sortvar=sortvar, 
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         getStatsDifAnnotationFunction=getStatsDifAnnotationFunction,
         rtsForSTDsInfo=rtsForSTDsInfo,
         dfpdsForSTDsInfo=dfpdsForSTDsInfo,
         maxRT=maxRT,
         maxDFPD=maxDFPD,
         nResamples=nResamples,
         conf=conf,
         subjectsAndComponents=subjectsAndComponents,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         width=width, height=height,
         ...)
    }
}

plotMediansOfPredictionsErrorsForHitsAndMisses2ForSubjects <- 
 function(sortvar, modality, clusterID, condition,
                   getStatsDifAnnotationFunction,
                   rtsForSTDsInfo,
                   dfpdsForSTDsInfo,
                   maxRT, 
                   maxDFPD, 
                   nResamples,
                   conf,
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   plotsFilenamePattern,
                   width, height,
                   ...) { 
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
            scoRTsForSTDsInfo <- 
             getSubjectAndConditionRTsForSTDsInfo(subjectName=subjectName,
                                                  condition=condition, 
                                                  rtsForSTDsInfo=rtsForSTDsInfo)
            scoDFPDsForSTDsInfo <- 
             getSubjectAndConditionDFPDsForSTDsInfo(subjectName=subjectName,
                                                     modality=modality,
                                                     condition=condition, 
                                                     dfpdsForSTDsInfo=
                                                      dfpdsForSTDsInfo)
            stdsUREvents <- analyzedData$epochEventIDs
            rts <- getRTsForSTDsUREvents(stdsUREvents=stdsUREvents,
                                          rtsForSTDsInfo=scoRTsForSTDsInfo)
            dfpds <- getRTsForSTDsUREvents(stdsUREvents=stdsUREvents,
                                            rtsForSTDsInfo=scoDFPDsForSTDsInfo)
            plotFilename <- sprintf(plotsFilenamePattern, 
                                     modality, 
                                     sortvar, 
                                     clusterID, 
                                     condition,
                                     subjectName,
                                     component)
            trellis.device("postscript", color=TRUE, width=width, height=height,
                            onefile=FALSE, horizontal=FALSE, file=plotFilename)
            trellis.par.set(theme=canonical.theme("X11"))
            if(!is.null(analyzedData$predictions)) {
                plotMediansOfPredictionsErrorsForHitsAndMisses(
                 scaledTransformedPredictions=
                  analyzedData$predictions, 
                 scaledTransformedDelays=
                  analyzedData$data[,1],
                 rts=rts,
                 maxRT=maxRT,
                 dfpds=dfpds,
                 maxDFPD=maxDFPD,
                 getStatsDifAnnotationFunction=getStatsDifAnnotationFunction,
                 centerDelays=attr(analyzedData$data, 
                                    "scaled:center")[1],
                 scaleDelays=attr(analyzedData$data, 
                                   "scaled:scale")[1],
                 conf=conf,
                 nResamples=nResamples, ...)
            } else {
                print(getEmptyPlot())
            }
            dev.off()
        }
    }
}
