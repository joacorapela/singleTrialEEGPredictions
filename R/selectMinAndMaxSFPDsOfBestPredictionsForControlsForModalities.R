
selectMinAndMaxSFPDsOfBestPredictionsForControlsForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, scFilenamePattern,
                   minSFPDs, maxSFPDs, corCoefsFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        selectMinAndMaxSFPDsOfBestPredictionsForControlsForClusters(sortvar=sortvar, 
                                    modality=modality, 
                                    clustersIDs=clustersIDs, 
                                    conditions=conditions, 
                                    scFilenamePattern=
                                     scFilenamePattern,
                                    minSFPDs=minSFPDs, 
                                    maxSFPDs=maxSFPDs, 
                                    corCoefsFilenamePattern=
                                     corCoefsFilenamePattern,
                                    minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                     minAndMaxSFPDOfBestPredictionsFilenamePattern)
    }
}

selectMinAndMaxSFPDsOfBestPredictionsForControlsForClusters <- 
 function(sortvar, modality, clustersIDs, conditions, scFilenamePattern,
                   minSFPDs, maxSFPDs, corCoefsFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %2d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID, scFilename)
        selectMinAndMaxSFPDsOfBestPredictionsForControlsForConditions(sortvar=sortvar, 
                                        modality=modality, 
                                        clusterID=clusterID, 
                                        conditions=conditions, 
                                        subjectsAndComponents=
                                         subjectsAndComponents,
                                        minSFPDs=minSFPDs, 
                                        maxSFPDs=maxSFPDs, 
                                        corCoefsFilenamePattern=
                                         corCoefsFilenamePattern,
                                        minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                         minAndMaxSFPDOfBestPredictionsFilenamePattern)
    }
}

selectMinAndMaxSFPDsOfBestPredictionsForControlsForConditions <-
 function(sortvar, modality, clusterID, conditions, subjectsAndComponents,
                   minSFPDs, maxSFPDs, corCoefsFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        selectMinAndMaxSFPDsOfBestPredictionsForControlsForSubjectsAndComponents(sortvar=sortvar, 
                                                   modality=modality, 
                                                   clusterID=clusterID, 
                                                   condition=condition, 
                                                   subjectsAndComponents=
                                                    subjectsAndComponents,
                                                   minSFPDs=minSFPDs, 
                                                   maxSFPDs=maxSFPDs, 
                                                   corCoefsFilenamePattern=
                                                    corCoefsFilenamePattern,
                                                   minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                                    minAndMaxSFPDOfBestPredictionsFilenamePattern)
    }
}

selectMinAndMaxSFPDsOfBestPredictionsForControlsForSubjectsAndComponents <-
 function(sortvar, modality, clusterID, condition, subjectsAndComponents, 
                   minSFPDs, maxSFPDs, corCoefsFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern) {
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                                          component))
        selectMinAndMaxSFPDsOfBestPredictionsForControlsForSubjectAndComponent(sortvar=sortvar, 
                                                 modality=modality, 
                                                 clusterID=clusterID, 
                                                 condition=condition, 
                                                 subjectName=subjectName, 
                                                 component=component, 
                                                 minSFPDs=minSFPDs, 
                                                 maxSFPDs=maxSFPDs, 
                                                 corCoefsFilenamePattern=
                                                  corCoefsFilenamePattern, 
                                                 minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                                  minAndMaxSFPDOfBestPredictionsFilenamePattern)
    }
}

selectMinAndMaxSFPDsOfBestPredictionsForControlsForSubjectAndComponent <-
 function(sortvar, modality, clusterID, condition, subjectName, component,
                   minSFPDs, maxSFPDs, corCoefsFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern) {
    minAndMaxSFPDOfBestPredictionsFilename <- 
     sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, 
              clusterID, clusterID, condition, sortvar, modality, 
              subjectName, component)
    largestMedianCorCoef <- -Inf
    selectedMinSFPD <- minSFPDs[1]
    selectedMaxSFPD <- maxSFPDs[1]
# if(subjectName=="av112a" && component==11) {
#     browser()
# }
    for(i in 1:length(minSFPDs)) {
        minSFPD <- minSFPDs[i]
        for(j in 1:length(maxSFPDs)) {
            maxSFPD <- maxSFPDs[j]
            corCoefsFilename <- 
             sprintf(corCoefsFilenamePattern, clusterID, clusterID, condition, 
                                              sortvar, modality, 
                                              subjectName, component,
                                              minSFPD, maxSFPD)
            if(!file.exists(corCoefsFilename)) {
                next
            }
            corCoefs <- get(load(corCoefsFilename))
            medianCorCoefs <- median(corCoefs)
            if(medianCorCoefs>largestMedianCorCoef) {
                largestMedianCorCoef <- medianCorCoefs
                selectedMinSFPD <- minSFPD
                selectedMaxSFPD <- maxSFPD
            }
        }
    }
    writeLines(c(toString(selectedMinSFPD), toString(selectedMaxSFPD)), 
                con=minAndMaxSFPDOfBestPredictionsFilename)
}

