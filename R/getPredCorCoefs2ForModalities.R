
getPredCorCoefs2ForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, minSFPD, maxSFPD,
                   significance,
                   scFilenamePattern, analyzedConditionsFilenamePattern) {
    predCorCoefs <- data.frame()
    for(modality in modalities) {
        modalityPredCorCoefs <- getPredCorCoefs2ForClusters(
                        sortvar=sortvar,
                        modality=modality,
                        clustersIDs=clustersIDs,
                        conditions=conditions,
                        minSFPD=minSFPD,
                        maxSFPD=maxSFPD,
                        significance=significance,
                        scFilenamePattern=scFilenamePattern,
                        analyzedConditionsFilenamePattern=
                         analyzedConditionsFilenamePattern)
        if(length(modalityPredCorCoefs)>0) {
            modalityPredCorCoefs["modality"] <- modality
            predCorCoefs <- rbind(predCorCoefs, modalityPredCorCoefs)
        }
    }
    predCorCoefs$modality  <- as.factor(predCorCoefs$modality)
    predCorCoefs$clusterID <- as.factor(predCorCoefs$clusterID)
    predCorCoefs$condition <- as.factor(predCorCoefs$condition)
    predCorCoefs$subject   <- as.factor(predCorCoefs$subject)
    return(predCorCoefs)
}
getPredCorCoefs2ForClusters <- 
 function(sortvar, modality, clustersIDs, conditions, minSFPD, maxSFPD,
                   significance,
                   scFilenamePattern, analyzedConditionsFilenamePattern) {
    predCorCoefs <- data.frame()
    for(clusterID in clustersIDs) {
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID, scFilename)
        clusterCorCoefs <- getPredCorCoefs2ForConditions(
                            sortvar=sortvar,
                            modality=modality,
                            clusterID=clusterID,
                            conditions=conditions,
                            minSFPD=minSFPD,
                            maxSFPD=maxSFPD,
                            significance=significance,
                            subjectsAndComponents=subjectsAndComponents,
                            analyzedConditionsFilenamePattern=
                             analyzedConditionsFilenamePattern)
        if(length(clusterCorCoefs)>0) {
            clusterCorCoefs["clusterID"] <- clusterID
            predCorCoefs <- rbind(predCorCoefs, clusterCorCoefs)
        }
    }
    return(predCorCoefs)
}
getPredCorCoefs2ForConditions <- 
 function(sortvar, modality, clusterID, conditions, minSFPD, maxSFPD,
                   significance,
                   subjectsAndComponents=subjectsAndComponents,
                   analyzedConditionsFilenamePattern) {
    predCorCoefs <- data.frame()
    for(condition in conditions) {
        conditionPredCorCoefs <- getPredCorCoefs2ForSubjects(
                                  sortvar=sortvar,
                                  modality=modality,
                                  clusterID=clusterID,
                                  condition=condition,
                                  minSFPD=minSFPD,
                                  maxSFPD=maxSFPD,
                                  significance=significance,
                                  subjectsAndComponents=subjectsAndComponents,
                                  analyzedConditionsFilenamePattern=
                                   analyzedConditionsFilenamePattern)
        if(length(conditionPredCorCoefs)>0) {
            conditionPredCorCoefs["condition"] <- condition
            predCorCoefs <- rbind(predCorCoefs, conditionPredCorCoefs)
        }
    }
    return(predCorCoefs)
}
getPredCorCoefs2ForSubjects <- 
 function(sortvar, modality, clusterID, condition, minSFPD, maxSFPD,
                   significance,
                   subjectsAndComponents=subjectsAndComponents,
                   analyzedConditionsFilenamePattern) {
    predCorCoefs <- data.frame()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
        analyzedConditionFilename <- sprintf(analyzedConditionsFilenamePattern,
                                              clusterID,
                                              condition,
                                              sortvar,
                                              modality,
                                              subjectName,
                                              component,
                                              minSFPD,
                                              maxSFPD)
        sAnalyzedCondition <- get(load(analyzedConditionFilename))
        if(!is.null(sAnalyzedCondition$predSFPDurCorCI)) {
            sPredCorCoefs <- 
             data.frame(subject=subjectName,
                         component=component,
                         corCoef=sAnalyzedCondition$predSFPDurCorCI[1],
                         corCoef95CIL=sAnalyzedCondition$predSFPDurCorCI[2],
                         corCoef95CIH=sAnalyzedCondition$predSFPDurCorCI[3])
        } else {
            sPredCorCoefs <- 
             data.frame(subject=subjectName,
                         component=component,
                         corCoef=0,
                         corCoef95CIL=0,
                         corCoef95CIH=0)
        }
        predCorCoefs <- rbind(predCorCoefs, sPredCorCoefs)
    }
    return(predCorCoefs)
}

