
plotPredictionsErrorsVsRTs3ForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions,
                   getStatsCorCoefAnnotationFunction,
                   dsAndPreviousSTDsInfo,
                   rtsInfo,
                   dfpdsInfo,
                   maxRT, 
                   maxDFPD,
                   nResamples, conf,
                   subjectsAndComponents,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   plotsFilenamePattern,
                   width, height,
                   ...) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotPredictionsErrorsVsRTs3ForClusters(sortvar=sortvar, 
                                                modality=modality,
                                                clustersIDs=clustersIDs, 
                                                conditions=conditions,
                                                getStatsCorCoefAnnotationFunction=
                                                 getStatsCorCoefAnnotationFunction,
                                                dsAndPreviousSTDsInfo=
                                                 dsAndPreviousSTDsInfo,
                                                rtsInfo=rtsInfo,
                                                dfpdsInfo=dfpdsInfo,
                                                maxRT=maxRT,
                                                maxDFPD=maxDFPD,
                                                nResamples=nResamples, 
                                                conf=conf,
                                                subjectsAndComponents=
                                                 subjectsAndComponents,
                                                scFilenamePattern=scFilenamePattern,
                                                minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                                 minAndMaxSFPDOfBestPredictionsFilenamePattern,
                                                analyzedDataFilenamePattern=
                                                 analyzedDataFilenamePattern,
                                                plotsFilenamePattern=
                                                 plotsFilenamePattern,
                                                width=width, 
                                                height=height,
                                               ...)
    }
}

plotPredictionsErrorsVsRTs3ForClusters <- 
 function(sortvar, modality, clustersIDs, conditions,
                   getStatsCorCoefAnnotationFunction,
                   dsAndPreviousSTDsInfo,
                   rtsInfo,
                   dfpdsInfo,
                   maxRT, 
                   maxDFPD,
                   nResamples, conf,
                   subjectsAndComponents,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   plotsFilenamePattern,
                   width, height,
                   ...) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %d", clusterID))
        if(is.null(subjectsAndComponents)) {
            scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
            if(is.null(scFilename)) {
                stop("arguments subjectsAndComponents or scFilenamePattern should be provided")
            }
            subjectsAndComponentsInCluster <-
             getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                                scFilename=scFilename)
        } else {
            subjectsAndComponentsInCluster <- subjectsAndComponents
        }
        plotPredictionsErrorsVsRTs3ForConditions(
         sortvar=sortvar, 
         modality=modality,
         clusterID=clusterID, 
         conditions=conditions,
         getStatsCorCoefAnnotationFunction=getStatsCorCoefAnnotationFunction,
         dsAndPreviousSTDsInfo=dsAndPreviousSTDsInfo,
         rtsInfo=rtsInfo,
         dfpdsInfo=dfpdsInfo,
         maxRT=maxRT,
         maxDFPD=maxDFPD,
         nResamples=nResamples, conf=conf,
         subjectsAndComponents=subjectsAndComponentsInCluster,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         width=width, height=height, ...)
    }
}

plotPredictionsErrorsVsRTs3ForConditions <- 
 function(sortvar, modality, clusterID, conditions,
                   getStatsCorCoefAnnotationFunction,
                   dsAndPreviousSTDsInfo,
                   rtsInfo,
                   dfpdsInfo,
                   maxRT, 
                   maxDFPD,
                   nResamples, conf,
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   plotsFilenamePattern,
                   width, height, ...) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        plotPredictionsErrorsVsRTs3ForCondition(
         sortvar=sortvar, 
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         getStatsCorCoefAnnotationFunction=getStatsCorCoefAnnotationFunction,
         dsAndPreviousSTDsInfo=dsAndPreviousSTDsInfo,
         rtsInfo=rtsInfo,
         dfpdsInfo=dfpdsInfo,
         maxRT=maxRT, 
         maxDFPD=maxDFPD,
         nResamples=nResamples, conf=conf,
         subjectsAndComponents=subjectsAndComponents,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         width=width, height=height, ...)
    }
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
plotPredictionsErrorsVsRTs3ForCondition <- 
 function(sortvar, modality, clusterID, condition,
                   getStatsCorCoefAnnotationFunction,
                   dsAndPreviousSTDsInfo,
                   rtsInfo,
                   dfpdsInfo,
                   maxRT, maxSTD_D_delay, maxDFPD,
                   nResamples, conf,
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, 
                   plotsFilenamePattern, 
                   width, height, ...) {
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
            if(!is.null(analyzedData$data)) {
                subsetSTDsDeviantsDF <- 
                 buildSubjectAndComponentSingleTrialANOVADF(
                  subjectName=subjectName,
                  modality=modality,
                  condition=condition,
                  analyzedData=analyzedData, 
                  rtsInfo=rtsInfo,
                  dfpdsInfo=dfpdsInfo,
                  dsAndPreviousSTDsInfo=dsAndPreviousSTDsInfo,
                  maxRT=maxRT,
                  maxSTD_D_delay=maxSTD_D_delay,
                  maxDFPD=maxDFPD)
                plotFilename <- sprintf(plotsFilenamePattern, 
                                         modality, 
                                         sortvar, 
                                         clusterID, 
                                         condition,
                                         subjectName,
                                         component)

                trellis.device("postscript", color=TRUE, width=width, 
                               height=height, onefile=FALSE, horizontal=FALSE, 
                               file=plotFilename)
                trellis.par.set(theme=canonical.theme("X11"))
                if(!is.null(analyzedData$predictions)) {
                    plotPredictionsErrorsVsRTs3(
                     predictionErrors=subsetSTDsDeviantsDF$predictionError,
                     rts=subsetSTDsDeviantsDF$rt,
                     dfpds=subsetSTDsDeviantsDF$dfpd,
                     maxRT=maxRT,
                     maxDFPD=maxDFPD,
                     nResamples=nResamples,
                     conf=conf,
                     getStatsCorCoefAnnotationFunction=
                      getStatsCorCoefAnnotationFunction,
                     ...)
                }
                dev.off()
            }
        }
    }
}
