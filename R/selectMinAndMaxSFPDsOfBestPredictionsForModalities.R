
selectMinAndMaxSFPDsOfBestPredictionsForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, scFilenamePattern,
                   minSFPDs, maxSFPDs, analyzedDataFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        selectMinAndMaxSFPDsOfBestPredictionsForClusters(sortvar=sortvar, 
                                    modality=modality, 
                                    clustersIDs=clustersIDs, 
                                    conditions=conditions, 
                                    scFilenamePattern=
                                     scFilenamePattern,
                                    minSFPDs=minSFPDs, 
                                    maxSFPDs=maxSFPDs, 
                                    analyzedDataFilenamePattern=
                                     analyzedDataFilenamePattern,
                                    minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                     minAndMaxSFPDOfBestPredictionsFilenamePattern)
    }
}

selectMinAndMaxSFPDsOfBestPredictionsForClusters <- 
 function(sortvar, modality, clustersIDs, conditions, scFilenamePattern,
                   minSFPDs, maxSFPDs, analyzedDataFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %2d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID, scFilename)
        selectMinAndMaxSFPDsOfBestPredictionsForConditions(sortvar=sortvar, 
                                        modality=modality, 
                                        clusterID=clusterID, 
                                        conditions=conditions, 
                                        subjectsAndComponents=
                                         subjectsAndComponents,
                                        minSFPDs=minSFPDs, 
                                        maxSFPDs=maxSFPDs, 
                                        analyzedDataFilenamePattern=
                                         analyzedDataFilenamePattern,
                                        minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                         minAndMaxSFPDOfBestPredictionsFilenamePattern)
    }
}

selectMinAndMaxSFPDsOfBestPredictionsForConditions <-
 function(sortvar, modality, clusterID, conditions, subjectsAndComponents,
                   minSFPDs, maxSFPDs, analyzedDataFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        selectMinAndMaxSFPDsOfBestPredictionsForSubjectsAndComponents(sortvar=sortvar, 
                                                   modality=modality, 
                                                   clusterID=clusterID, 
                                                   condition=condition, 
                                                   subjectsAndComponents=
                                                    subjectsAndComponents,
                                                   minSFPDs=minSFPDs, 
                                                   maxSFPDs=maxSFPDs, 
                                                   analyzedDataFilenamePattern=
                                                    analyzedDataFilenamePattern,
                                                   minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                                    minAndMaxSFPDOfBestPredictionsFilenamePattern)
    }
}

selectMinAndMaxSFPDsOfBestPredictionsForSubjectsAndComponents <-
 function(sortvar, modality, clusterID, condition, subjectsAndComponents, 
                   minSFPDs, maxSFPDs, analyzedDataFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern) {
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                                          component))
        selectMinAndMaxSFPDsOfBestPredictionsForSubjectAndComponent(sortvar=sortvar, 
                                                 modality=modality, 
                                                 clusterID=clusterID, 
                                                 condition=condition, 
                                                 subjectName=subjectName, 
                                                 component=component, 
                                                 minSFPDs=minSFPDs, 
                                                 maxSFPDs=maxSFPDs, 
                                                 analyzedDataFilenamePattern=
                                                  analyzedDataFilenamePattern, 
                                                 minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                                  minAndMaxSFPDOfBestPredictionsFilenamePattern)
    }
}

selectMinAndMaxSFPDsOfBestPredictionsForSubjectAndComponent <-
 function(sortvar, modality, clusterID, condition, subjectName, component,
                   minSFPDs, maxSFPDs, analyzedDataFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern) {
    minAndMaxSFPDOfBestPredictionsFilename <- 
     sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, 
              clusterID, clusterID, condition, sortvar, modality, 
              subjectName, component)
    largestCorCoef <- -Inf
    selectedMinSFPD <- NaN
    selectedMaxSFPD <- NaN
    for(i in 1:length(minSFPDs)) {
        minSFPD <- minSFPDs[i]
        for(j in 1:length(maxSFPDs)) {
            maxSFPD <- maxSFPDs[j]
            analyzedDataFilename <- 
             sprintf(analyzedDataFilenamePattern, 
                      clusterID, clusterID, condition, sortvar, modality, 
                      subjectName, component, minSFPD, maxSFPD)
            if(!file.exists(analyzedDataFilename)) {
                next
            }
            analyzedData <- get(load(analyzedDataFilename))
            if(!is.null(analyzedData$predSFPDurCorCI) &&
                 analyzedData$predSFPDurCorCI[1]>largestCorCoef) {
                largestCorCoef <- analyzedData$predSFPDurCorCI[1]
                selectedMinSFPD <- minSFPD
                selectedMaxSFPD <- maxSFPD
            }
        }
    }
    writeLines(c(toString(selectedMinSFPD), toString(selectedMaxSFPD)), 
                con=minAndMaxSFPDOfBestPredictionsFilename)
}

