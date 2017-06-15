
shuffleAndEstimateCorCoefsRegularizedRegressionForModalities <-
 function(modalities, sortvar, clustersIDs, conditions, 
                      subjectsAndComponents, 
                      preProcessedPhaseERPIFilenamePattern, 
                      corCoefsFilenamePattern, 
                      minSFPDs, maxSFPDs, lambda, order, scaleData, 
                      nGroups, 
                      a0, b0, c0, d0, maxIter, convergenceTol, nShuffles) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        shuffleAndEstimateCorCoefsRegularizedRegressionForClusters(
         modality=modality, 
         sortvar=sortvar, 
         clustersIDs=clustersIDs, 
         conditions=conditions, 
         subjectsAndComponents=subjectsAndComponents,
         preProcessedPhaseERPIFilenamePattern=preProcessedPhaseERPIFilenamePattern,
         corCoefsFilenamePattern=corCoefsFilenamePattern,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, 
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nShuffles=nShuffles)
    }
}

shuffleAndEstimateCorCoefsRegularizedRegressionForClusters <-
 function(modality, sortvar, clustersIDs, conditions, 
                    subjectsAndComponents, 
                    preProcessedPhaseERPIFilenamePattern, 
                    corCoefsFilenamePattern, 
                    minSFPDs, maxSFPDs, lambda, order, scaleData, 
                    nGroups, 
                    a0, b0, c0, d0, maxIter, convergenceTol, nShuffles) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %2d", clusterID))
        shuffleAndEstimateCorCoefsRegularizedRegressionForConditions(
         modality=modality, 
         sortvar=sortvar, 
         clusterID=clusterID, 
         conditions=conditions, 
         subjectsAndComponents=subjectsAndComponents,
         preProcessedPhaseERPIFilenamePattern=
          preProcessedPhaseERPIFilenamePattern,
         corCoefsFilenamePattern=corCoefsFilenamePattern,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, 
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nShuffles=nShuffles)
    }
}

shuffleAndEstimateCorCoefsRegularizedRegressionForConditions <-
 function(modality, sortvar, clusterID, conditions, 
                    subjectsAndComponents,
                    preProcessedPhaseERPIFilenamePattern, 
                    corCoefsFilenamePattern, 
                    minSFPDs, maxSFPDs, lambda, order, scaleData, 
                    nGroups, 
                    a0, b0, c0, d0, maxIter, convergenceTol, nShuffles) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        shuffleAndEstimateCorCoefsRegularizedRegressionForSubjectsAndComponents(
         modality=modality, 
         sortvar=sortvar, 
         clusterID=clusterID, 
         condition=condition, 
         subjectsAndComponents=subjectsAndComponents,
         preProcessedPhaseERPIFilenamePattern=
          preProcessedPhaseERPIFilenamePattern,
         corCoefsFilenamePattern=corCoefsFilenamePattern,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, 
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nShuffles=nShuffles)
    }
}

shuffleAndEstimateCorCoefsRegularizedRegressionForSubjectsAndComponents <-
 function(modality, sortvar, clusterID, condition, 
                    subjectsAndComponents,
                    preProcessedPhaseERPIFilenamePattern, 
                    corCoefsFilenamePattern, 
                    minSFPDs, maxSFPDs, lambda, order, scaleData, 
                    nGroups, 
                    a0, b0, c0, d0, maxIter, convergenceTol, nShuffles) {
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
        shuffleAndEstimateCorCoefsRegularizedRegressionForSubjectAndComponent(
         modality=modality, 
         sortvar=sortvar, 
         clusterID=clusterID, 
         condition=condition, 
         subjectName=subjectName,
         component=component,
         preProcessedPhaseERPIFilenamePattern=preProcessedPhaseERPIFilenamePattern,
         corCoefsFilenamePattern=corCoefsFilenamePattern,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, 
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nShuffles=nShuffles)
    }
}
shuffleAndEstimateCorCoefsRegularizedRegressionForSubjectAndComponent <-
 function(modality, sortvar, clusterID, condition, 
                    subjectName,
                    component,
                    preProcessedPhaseERPIFilenamePattern, 
                    corCoefsFilenamePattern, 
                    minSFPDs, maxSFPDs, lambda, order, scaleData, 
                    nGroups, 
                    a0, b0, c0, d0, maxIter, convergenceTol, nShuffles) {
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
            corCoefsFilename <- 
             sprintf(corCoefsFilenamePattern, clusterID, condition,
                                              sortvar, modality, subjectName, 
                                              component, minSFPD, maxSFPD)
            corCoefs <- shuffleAndEstimateCorCoefsRegularizedRegression(
                         erpImage=preProcessedPhaseERPI, 
                         minSFPD=minSFPD,
                         maxSFPD=maxSFPD,
                         lambda=lambda,
                         order=order,
                         scaleData=scaleData, 
                         nGroups=nGroups,
                         a0=a0, b0=b0, c0=c0, d0=d0, 
                         maxIter=maxIter,
                         convergenceTol=convergenceTol,
                         nShuffles=nShuffles)
            save(corCoefs, file=corCoefsFilename)
            i <- i+1
        }
    }
}

shuffleAndEstimateCorCoefsRegularizedRegression <- 
 function(erpImage, minSFPD, maxSFPD, lambda, order, scaleData, 
                    nGroups, a0, b0, c0, d0, maxIter, 
                    convergenceTol, nShuffles) {
    corCoefs <- c()
    for(i in 1:nShuffles) {
        erpImage$sfpds <- sample(erpImage$sfpds)
        corCoef <- estimateCorCoefRegularizedRegression(erpImage=erpImage, 
                                                         minSFPD=minSFPD,
                                                         maxSFPD=maxSFPD,
                                                         lambda=lambda, 
                                                         order=order, 
                                                         scaleData=scaleData, 
                                                         nGroups=nGroups, 
                                                         a0=a0, 
                                                         b0=b0, 
                                                         c0=c0, 
                                                         d0=d0, 
                                                         maxIter=maxIter,
                                                         convergenceTol=
                                                          convergenceTol)
        show(sprintf("corCoef %d (out of %d): %f", i, nShuffles, corCoef))
        corCoefs <- c(corCoefs, corCoef)
    }
    return(corCoefs)
}
