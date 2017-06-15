
plotPredictionsVsSFPDs3ForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, 
                   adjustedPValuesDF,
                   corCoefAnnotationFunction,
                   subjectsAndComponents, scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   plotsFilenamePattern,
                   width, height, ...) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotPredictionsVsSFPDs3ForClusters(sortvar=sortvar, 
                                            modality=modality,
                                            clustersIDs=clustersIDs, 
                                            conditions=conditions,
                                            adjustedPValuesDF=adjustedPValuesDF,
                                            corCoefAnnotationFunction=
                                             corCoefAnnotationFunction,
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

plotPredictionsVsSFPDs3ForClusters <- 
 function(sortvar, modality, clustersIDs, conditions, 
                   adjustedPValuesDF,
                   corCoefAnnotationFunction,
                   subjectsAndComponents, scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   plotsFilenamePattern,
                   width, height, ...) {
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
        plotPredictionsVsSFPDs3ForConditions(
         sortvar=sortvar, 
         modality=modality,
         clusterID=clusterID, 
         conditions=conditions,
         adjustedPValuesDF=adjustedPValuesDF,
         corCoefAnnotationFunction=
          corCoefAnnotationFunction,
         subjectsAndComponents=subjectsAndComponentsInCluster,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         plotsFilenamePattern=
         plotsFilenamePattern,
         width=width, 
         height=height,
         ...)
    }
}

plotPredictionsVsSFPDs3ForConditions <- 
 function(sortvar, modality, clusterID, conditions, 
                   adjustedPValuesDF,
                   corCoefAnnotationFunction,
                   subjectsAndComponents, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   plotsFilenamePattern,
                   width, height, ...) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        plotPredictionsVsSFPDs3ForSubjects(sortvar=sortvar, 
                                           modality=modality,
                                           clusterID=clusterID, 
                                           condition=condition,
                                           adjustedPValuesDF=adjustedPValuesDF,
                                           corCoefAnnotationFunction=
                                            corCoefAnnotationFunction,
                                           subjectsAndComponents=
                                            subjectsAndComponents,
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

plotPredictionsVsSFPDs3ForSubjects <- 
 function(sortvar, modality, clusterID, condition, 
                   adjustedPValuesDF,
                   corCoefAnnotationFunction,
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
        plotFilename <- sprintf(plotsFilenamePattern, 
                                 clusterID, 
                                 sortvar, 
                                 modality, 
                                 condition,
                                 clusterID, 
                                 subjectName,
                                 component)
        trellis.device("postscript", width=width, height=height, 
                       onefile=FALSE, horizontal=FALSE, file=plotFilename)
        if(!is.nan(minSFPD) && !is.na(maxSFPD)) {
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
            if(length(analyzedData$predictions)>0) {
                dfIndex <- which(adjustedPValuesDF$modality==modality &
                                 adjustedPValuesDF$clusterID==clusterID &
                                 adjustedPValuesDF$condition==condition &
                                 adjustedPValuesDF$subjectName==subjectName &
                                 adjustedPValuesDF$component==component)
                if(length(dfIndex)>0) {
                    statsCorCoef <- c(list(ci=analyzedData$predSFPDurCorCI),
                                       adjustedPValuesDF[dfIndex,])
                    plotPredictionsVsSFPDs(scaledTransformedPredictions=
                                             analyzedData$predictions,
                                            scaledTransformedSFPDs=
                                             analyzedData$data[,1],
                                            statsCorCoef=statsCorCoef,
                                            corCoefAnnotationFunction=
                                             corCoefAnnotationFunction,
                                            ...)
                } else {
                print(getEmptyPlot())
                }
            } else {
                print(getEmptyPlot())
            }
        } else {
            print(getEmptyPlot())
        }
        dev.off()
    }
}
