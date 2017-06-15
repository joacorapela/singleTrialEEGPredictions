
plotRegLMCoefficientsForModalities <- 
 function(sortvar, 
           modalities, 
           clustersIDs,
           conditions,
           unstandardize,
           subjectsAndComponents,
           scFilenamePattern,
           minSFPDs, maxSFPDs,
           analyzedConditionsFilenamePattern,
           plotsFilenamePattern,
           xlim,
           ...) {
    if(!is.null(subjectsAndComponents) && !is.null(scFilenamePattern)) {
        warning("Both subjectsAndComponents and scFilenamePattern are given.  Using only subjectsAndComponents")
    }
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotRegLMCoefficientsForClusters(sortvar=sortvar, 
                                            modality=modality,
                                            clustersIDs=clustersIDs, 
                                            conditions=conditions,
                                            unstandardize=unstandardize,
                                            subjectsAndComponents=
                                             subjectsAndComponents,
                                            scFilenamePattern=scFilenamePattern,
                                            minSFPDs=minSFPDs, 
                                            maxSFPDs=maxSFPDs,
                                            analyzedConditionsFilenamePattern=
                                             analyzedConditionsFilenamePattern,
                                            plotsFilenamePattern=
                                             plotsFilenamePattern,
                                            xlim=xlim, 
                                            ...)
    }
}

plotRegLMCoefficientsForClusters <- 
 function(sortvar, 
           modality, 
           clustersIDs,
           conditions,
           unstandardize,
           subjectsAndComponents,
           scFilenamePattern,
           minSFPDs, maxSFPDs,
           analyzedConditionsFilenamePattern,
           plotsFilenamePattern,
           xlim,
           ...) {
    for(clusterID in clustersIDs) {
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
        show(sprintf("Processing cluster %s", clusterID))
        plotRegLMCoefficientsForConditions(sortvar=sortvar, 
                                        modality=modality,
                                        clusterID=clusterID, 
                                        conditions=conditions,
                                        unstandardize=unstandardize,
                                        subjectsAndComponents=
                                         subjectsAndComponentsInCluster,
                                        minSFPDs=minSFPDs, maxSFPDs=maxSFPDs,
                                        analyzedConditionsFilenamePattern=
                                         analyzedConditionsFilenamePattern,
                                        plotsFilenamePattern=
                                         plotsFilenamePattern,
                                        xlim=xlim,
                                        ...)
    }
}

plotRegLMCoefficientsForConditions <- 
 function(sortvar, 
           modality, 
           clusterID,
           conditions,
           unstandardize,
           subjectsAndComponents,
           minSFPDs, maxSFPDs,
           analyzedConditionsFilenamePattern,
           plotsFilenamePattern,
           xlim,
           ...) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        plotRegLMCoefficientsForSubjectsAndComponents(
         sortvar=sortvar, 
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         unstandardize=unstandardize,
         subjectsAndComponents=subjectsAndComponents,
         minSFPDs=minSFPDs, maxSFPDs=maxSFPDs,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         xlim=xlim,
         ...)
    }
}

plotRegLMCoefficientsForSubjectsAndComponents <- 
 function(sortvar, 
           modality, 
           clusterID, 
           condition, 
           unstandardize,
           subjectsAndComponents,
           minSFPDs, maxSFPDs,
           analyzedConditionsFilenamePattern,
           plotsFilenamePattern,
           xlim,
           ...) { 
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        for(minSFPD in minSFPDs) {
            for(maxSFPD in maxSFPDs) {
                analyzedConditionFilename <- 
                 sprintf(analyzedConditionsFilenamePattern,
                          clusterID,
                          clusterID,
                          condition,
                          sortvar,
                          modality,
                          subjectName,
                          component,
                          minSFPD,
                          maxSFPD)
                if(!file.exists(analyzedConditionFilename)) {
                    break
                }
                show(sprintf("Processing %s%02d: minSFPD %d, maxSFPD %d",
                             subjectName, component, minSFPD, maxSFPDs[i]))
                sAnalyzedCondition <- get(load(analyzedConditionFilename))
                meanTimeRegressors <- sAnalyzedCondition$meanTimeRegressors
                show(sprintf("Processing subject %s and comonent %02d", 
                             subjectName, component))
                plotFilename <- sprintf(plotsFilenamePattern, 
                                         clusterID,
                                         condition,
                                         sortvar,
                                         modality,
                                         subjectName,
                                         component,
                                         minSFPD,
                                         maxSFPD)
                coefsCIs <- sAnalyzedCondition$coefsCIs
                if(!is.null(coefsCIs)) {
                    if(unstandardize) {
                        scale <- attr(sAnalyzedCondition$data, "scaled:scale")
                        scaleY <- scale[1]
                        scaleX <- scale[-1]
                        center <- attr(sAnalyzedCondition$data, "scaled:center")
                        centerY <- center[1]
                        centerX <- center[-1]
                        coefsCIs <- unstandardizeCoefs(standardizedCoefs=
                                                         coefsCIs[-1,], 
                                                        centerX=centerX,
                                                        scaleX=scaleX,
                                                        centerY=centerY,
                                                        scaleY=scaleY)
                    }
                    plotRegLMCoefficients(coefsCIs=coefsCIs,
                                           times=sAnalyzedCondition$times,
                                           itc=sAnalyzedCondition$itc,
                                           meanDirection=
                                            sAnalyzedCondition$meanDirection,
                                           xlim=xlim,
                                           ...)
                    ggsave(filename=plotFilename)
               }
            }
        }
    }
}
