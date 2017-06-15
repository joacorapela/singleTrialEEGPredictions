
plotRegLMCoefficientsAtBestMinAndMaxSFPDsForModalities <- 
 function(sortvar, 
           modalities, 
           clustersIDs,
           conditions,
           unstandardize,
           modelSignificance,
           subjectsAndComponents,
           scFilenamePattern,
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
           analyzedDataFilenamePattern,
           plotsFilenamePattern,
           ...) {
    if(!is.null(subjectsAndComponents) && !is.null(scFilenamePattern)) {
        warning("Both subjectsAndComponents and scFilenamePattern are given.  Using only subjectsAndComponents")
    }
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotRegLMCoefficientsAtBestMinAndMaxSFPDsForClusters(
         sortvar=sortvar, 
         modality=modality,
         clustersIDs=clustersIDs, 
         conditions=conditions,
         unstandardize=unstandardize,
         modelSignificance=modelSignificance,
         subjectsAndComponents=subjectsAndComponents,
         scFilenamePattern=scFilenamePattern,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         ...)
    }
}

plotRegLMCoefficientsAtBestMinAndMaxSFPDsForClusters <- 
 function(sortvar, 
           modality, 
           clustersIDs,
           conditions,
           unstandardize,
           modelSignificance,
           subjectsAndComponents,
           scFilenamePattern,
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
           analyzedDataFilenamePattern,
           plotsFilenamePattern,
           ...) {
    if(is.na(subjectsAndComponents[1]) && is.na(scFilenamePattern[1])) {
        stop("One of subjectsAndComponents or scFilenamePattern should be provided")
    }
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %s", clusterID))
        if(is.null(subjectsAndComponents)) {
            scFilename <- sprintf(scFilenamePattern, clusterID)
            if(is.null(scFilename)) {
                stop("arguments subjectsAndComponents or scFilenamePattern should be provided")
            }
            subjectsAndComponentsInCluster <-
             getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                                scFilename=scFilename)
        } else {
            subjectsAndComponentsInCluster <- subjectsAndComponents
        }
        plotRegLMCoefficientsAtBestMinAndMaxSFPDsForConditions(
         sortvar=sortvar, 
         modality=modality,
         clusterID=clusterID, 
         conditions=conditions,
         unstandardize=unstandardize,
         modelSignificance=modelSignificance,
         subjectsAndComponents=subjectsAndComponentsInCluster,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         ...)
    }
}

plotRegLMCoefficientsAtBestMinAndMaxSFPDsForConditions <- 
 function(sortvar, 
           modality, 
           clusterID,
           conditions,
           unstandardize,
           modelSignificance,
           subjectsAndComponents,
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
           analyzedDataFilenamePattern,
           plotsFilenamePattern,
           ...) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        plotRegLMCoefficientsAtBestMinAndMaxSFPDsForSubjectsAndComponents(
         sortvar=sortvar, 
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         unstandardize=unstandardize,
         modelSignificance=modelSignificance,
         subjectsAndComponents=subjectsAndComponents,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         ...)
    }
}

plotRegLMCoefficientsAtBestMinAndMaxSFPDsForSubjectsAndComponents <- 
 function(sortvar, 
           modality, 
           clusterID, 
           condition, 
           unstandardize,
           modelSignificance,
           subjectsAndComponents,
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
           analyzedDataFilenamePattern,
           plotsFilenamePattern,
           ...) { 
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and comonent %02d", 
                     subjectName, component))
        minAndMaxSFPDOfBestPredictionsFilename <- 
         sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, clusterID,
                  clusterID, condition, sortvar, modality, subjectName, 
                  component)
        res <- readLines(minAndMaxSFPDOfBestPredictionsFilename)
        minSFPD <- as.integer(res[1])
        maxSFPD <- as.integer(res[2])
        plotFilename <- sprintf(plotsFilenamePattern, 
                                 clusterID,
                                 clusterID,
                                 condition,
                                 sortvar,
                                 modality,
                                 subjectName,
                                 component)
        if(!is.na(minSFPD) && !is.na(maxSFPD)) {
            analyzedDataFilename <- sprintf(analyzedDataFilenamePattern, 
                                             clusterID, clusterID, condition, 
                                             sortvar, modality, 
                                             subjectName, component,
                                             minSFPD, maxSFPD)
            analyzedData <- get(load(analyzedDataFilename))
            meanTimeRegressors <- analyzedData$meanTimeRegressors
            if(!is.infinite(modelSignificance) ||
               (!is.null(analyzedData$lrtRes) &&
                analyzedData$lrtRes$pValue<modelSignificance)) {
                coefsCIs <- analyzedData$coefsCIs
                if(unstandardize) {
                    scale <- attr(analayzedData$data, "scaled:scale")
                    scaleY <- scale[1]
                    scaleX <- scale[-1]
                    center <- attr(analayzedData$data, "scaled:center")
                    centerY <- center[1]
                    centerX <- center[-1]
                    coefsCIs <- unstandardizeCoefs(standardizedCoefs=coefsCIs, 
                                                    centerX=centerX,
                                                    scaleX=scaleX,
                                                    centerY=centerY,
                                                    scaleY=scaleY)
                }
                plotRegLMCoefficients(coefsCIs=coefsCIs,
                                       times=analyzedData$times,
                                       itc=analyzedData$itc,
                                       meanDirection=analyzedData$meanDirection,
                                       ...)
            } else {
                plot(getEmptyPlot())
            }
        } else {
            plot(getEmptyPlot())
        }
        ggsave(filename=plotFilename)
    }
}
