
analyzeDataRegularizedRegressionForModalities <-
 function(modalities, sortvar, clustersIDs, conditions, 
                      subjectsAndComponents, 
                      scFilenamePattern,
                      preProcessedPhaseERPIFilenamePattern, 
                      analyzedConditionsFilenamePattern, 
                      minSFPDs, maxSFPDs, lambda, order, scaleData, 
                      nGroups, 
                      a0, b0, c0, d0, computeCoefsCIs, maxIter, convergenceTol,
                      nResamplesCoefs, nResamplesPredictions, 
                      confCIs) {
    if(!is.null(subjectsAndComponents) && !is.null(scFilenamePattern)) {
        warning("Both subjectsAndComponents and scFilenamePattern are given.  Using only subjectsAndComponents")
    }
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        analyzeDataRegularizedRegressionForClusters(
         modality=modality, 
         sortvar=sortvar, 
         clustersIDs=clustersIDs, 
         conditions=conditions, 
         subjectsAndComponents=subjectsAndComponents,
         scFilenamePattern=scFilenamePattern,
         preProcessedPhaseERPIFilenamePattern=preProcessedPhaseERPIFilenamePattern,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         interactions=interactions,
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, computeCoefsCIs=computeCoefsCIs,
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nResamplesCoefs=nResamplesCoefs,
         nResamplesPredictions=nResamplesPredictions, 
         confCIs=confCIs)
    }
}

analyzeDataRegularizedRegressionForClusters <-
 function(modality, sortvar, clustersIDs, conditions, 
                    subjectsAndComponents, 
                    scFilenamePattern,
                    preProcessedPhaseERPIFilenamePattern, 
                    analyzedConditionsFilenamePattern, 
                    minSFPDs, maxSFPDs, lambda, order, interactions, scaleData, 
                    nGroups, 
                    a0, b0, c0, d0, computeCoefsCIs, maxIter, convergenceTol,
                    nResamplesCoefs, nResamplesPredictions, 
                    confCIs) {
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
        show(sprintf("Processing cluster %2d", clusterID))
        analyzeDataRegularizedRegressionForConditions(
         modality=modality, 
         sortvar=sortvar, 
         clusterID=clusterID, 
         conditions=conditions, 
         subjectsAndComponents=subjectsAndComponentsInCluster,
         preProcessedPhaseERPIFilenamePattern=
          preProcessedPhaseERPIFilenamePattern,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         interactions=interactions,
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, computeCoefsCIs=computeCoefsCIs,
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nResamplesCoefs=nResamplesCoefs,
         nResamplesPredictions=nResamplesPredictions, 
         confCIs=confCIs)
    }
}

analyzeDataRegularizedRegressionForConditions <-
 function(modality, sortvar, clusterID, conditions, 
                    subjectsAndComponents,
                    preProcessedPhaseERPIFilenamePattern, 
                    analyzedConditionsFilenamePattern, 
                    minSFPDs, maxSFPDs, lambda, order, interactions, scaleData, 
                    nGroups, 
                    a0, b0, c0, d0, computeCoefsCIs, maxIter, convergenceTol,
                    nResamplesCoefs, nResamplesPredictions, 
                    confCIs) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        analyzeDataRegularizedRegressionForSubjectsAndComponents(
         modality=modality, 
         sortvar=sortvar, 
         clusterID=clusterID, 
         condition=condition, 
         subjectsAndComponents=subjectsAndComponents,
         preProcessedPhaseERPIFilenamePattern=
          preProcessedPhaseERPIFilenamePattern,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         interactions=interactions,
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, computeCoefsCIs=computeCoefsCIs,
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nResamplesCoefs=nResamplesCoefs,
         nResamplesPredictions=nResamplesPredictions, 
         confCIs=confCIs)
    }
}

analyzeDataRegularizedRegressionForSubjectsAndComponents <-
 function(modality, sortvar, clusterID, condition, 
                    subjectsAndComponents,
                    preProcessedPhaseERPIFilenamePattern, 
                    analyzedConditionsFilenamePattern, 
                    minSFPDs, maxSFPDs, lambda, order, interactions, scaleData, 
                    nGroups, 
                    a0, b0, c0, d0, computeCoefsCIs, maxIter, convergenceTol,
                    nResamplesCoefs, nResamplesPredictions, 
                    confCIs) {
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
        analyzeDataRegularizedRegressionForSubjectAndComponent(
         modality=modality, 
         sortvar=sortvar, 
         clusterID=clusterID, 
         condition=condition, 
         subjectName=subjectName,
         component=component,
         preProcessedPhaseERPIFilenamePattern=preProcessedPhaseERPIFilenamePattern,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         interactions=interactions,
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, computeCoefsCIs=computeCoefsCIs,
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nResamplesCoefs=nResamplesCoefs,
         nResamplesPredictions=nResamplesPredictions, 
         confCIs=confCIs)
    }
}
analyzeDataRegularizedRegressionForSubjectAndComponent <-
 function(modality, sortvar, clusterID, condition, 
                    subjectName,
                    component,
                    preProcessedPhaseERPIFilenamePattern, 
                    analyzedConditionsFilenamePattern, 
                    minSFPDs, maxSFPDs, lambda, order, interactions, scaleData, 
                    nGroups, 
                    a0, b0, c0, d0, computeCoefsCIs, maxIter, convergenceTol,
                    nResamplesCoefs, nResamplesPredictions, 
                    confCIs) {
    preProcessedPhaseERPIFilename <- 
     sprintf(preProcessedPhaseERPIFilenamePattern, 
              clusterID,
              condition,
              sortvar,
              modality,
              subjectName,
              component)
    preProcessedPhaseERPI <- get(load(preProcessedPhaseERPIFilename))
    for(minSFPD in minSFPDs) {
        i <- 1
        while(i<=length(maxSFPDs) && 
               maxSFPDs[i]<=max(preProcessedPhaseERPI$sfpds)) {
            if(maxSFPDs[i]<minSFPD) {
                next
            }
            show(sprintf("Processing %s%02d: minSFPD %d, maxSFPD %d",
                         subjectName, component, minSFPD, maxSFPDs[i]))
            maxSFPD <- maxSFPDs[i]
            analyzedConditionsFilename <- 
             sprintf(analyzedConditionsFilenamePattern, clusterID, clusterID, 
                                                        condition,
                                                        sortvar, modality,
                                                        subjectName, component,
                                                        minSFPD, maxSFPD)
            res <- analyzeDataRegularizedRegression(
                    erpImage=preProcessedPhaseERPI, 
                    minSFPD=minSFPD,
                    maxSFPD=maxSFPD,
                    lambda=lambda,
                    order=order,
                    interactions=interactions, 
                    scaleData=scaleData, 
                    nGroups=nGroups,
                    a0=a0, b0=b0, c0=c0, d0=d0, 
                    computeCoefsCIs=computeCoefsCIs,
                    maxIter=maxIter,
                    convergenceTol=convergenceTol,
                    nResamplesCoefs=nResamplesCoefs, 
                    nResamplesPredictions=nResamplesPredictions, 
                    confCIs=confCIs)
            show(sprintf("predSFPDurCorCI=[%f, %f, %f]", 
                         res$predSFPDurCorCI[1],
                         res$predSFPDurCorCI[2], 
                         res$predSFPDurCorCI[3]))
            save(res, file=analyzedConditionsFilename)
            i <- i+1
        }
    }
}
