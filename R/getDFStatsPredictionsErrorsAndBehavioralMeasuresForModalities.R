
getDFStatsPredictionsErrorsAndBehavioralMeasuresForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, 
                   filterSFPStatsFunc,
                   filterDFPStatsFunc, 
                   getUnselectedSFPStats, 
                   getUnselectedDFPStats, 
                   getSFPAndDFPStatsForSubjectAndComponentFunc,
                   getSFPStatsForSubjectAndComponentFunc,
                   sFPStatsForModalities, 
                   dFPStatsForSubjects,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   modelSignificance) {
    df <- data.frame()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        sFPStatsForClusters <- 
         getItemInAVShiftList(listOfItems=sFPStatsForModalities,
                               listFieldName="stats",
                               keyFieldNames=c("modality"),
                               keyFieldValues=c(modality))
        clustersDF <- 
        getDFStatsPredictionsErrorsAndBehavioralMeasuresForClusters( 
         sortvar=sortvar,
         modality=modality,
         clustersIDs=clustersIDs,
         conditions=conditions,
         filterSFPStatsFunc=filterSFPStatsFunc,
         filterDFPStatsFunc=filterDFPStatsFunc,
         getUnselectedSFPStats=getUnselectedSFPStats,
         getUnselectedDFPStats=getUnselectedDFPStats,
         getSFPAndDFPStatsForSubjectAndComponentFunc=
          getSFPAndDFPStatsForSubjectAndComponentFunc,
         getSFPStatsForSubjectAndComponentFunc=
          getSFPStatsForSubjectAndComponentFunc,
         sFPStatsForClusters=sFPStatsForClusters, 
         dFPStatsForSubjects=dFPStatsForSubjects,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         modelSignificance=modelSignificance)
        df <- rbind(df, clustersDF)
    }
    df$modality <- factor(df$modality)
    df$clusterID <- factor(df$clusterID)
    df$condition <- factor(df$condition)
    return(df)
}
getDFStatsPredictionsErrorsAndBehavioralMeasuresForClusters <- 
 function(sortvar, modality, clustersIDs, conditions,
                   filterSFPStatsFunc,
                   filterDFPStatsFunc, 
                   getUnselectedSFPStats, 
                   getUnselectedDFPStats, 
                   getSFPAndDFPStatsForSubjectAndComponentFunc,
                   getSFPStatsForSubjectAndComponentFunc,
                   sFPStatsForClusters, 
                   dFPStatsForSubjects, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   modelSignificance) {
    df <- data.frame()
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %02d", clusterID))
        sFPStatsForConditions <- 
         getItemInAVShiftList(listOfItems=sFPStatsForClusters,
                               listFieldName="stats",
                               keyFieldNames=c("clusterID"),
                               keyFieldValues=c(clusterID))
        conditionsDF <- 
        getDFStatsPredictionsErrorsAndBehavioralMeasuresForConditions( 
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         conditions=conditions,
         filterSFPStatsFunc=filterSFPStatsFunc,
         filterDFPStatsFunc=filterDFPStatsFunc,
         getUnselectedSFPStats=getUnselectedSFPStats,
         getUnselectedDFPStats=getUnselectedDFPStats,
         getSFPAndDFPStatsForSubjectAndComponentFunc=
          getSFPAndDFPStatsForSubjectAndComponentFunc,
         getSFPStatsForSubjectAndComponentFunc=
          getSFPStatsForSubjectAndComponentFunc,
         sFPStatsForConditions=sFPStatsForConditions,
         dFPStatsForSubjects=dFPStatsForSubjects,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         modelSignificance=modelSignificance)
        df <- rbind(df, conditionsDF)
    }
    return(df)
}
getDFStatsPredictionsErrorsAndBehavioralMeasuresForConditions <- 
 function(sortvar, modality, clusterID, conditions,
                   filterSFPStatsFunc,
                   filterDFPStatsFunc, 
                   getUnselectedSFPStats, 
                   getUnselectedDFPStats, 
                   getSFPAndDFPStatsForSubjectAndComponentFunc,
                   getSFPStatsForSubjectAndComponentFunc,
                   sFPStatsForConditions,
                   dFPStatsForSubjects, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   modelSignificance) {
    df <- data.frame()
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        sFPStatsForSubjects <- 
         getItemInAVShiftList(listOfItems=sFPStatsForConditions,
                               listFieldName="stats",
                               keyFieldNames=c("condition"),
                               keyFieldValues=c(condition))
        if(length(sFPStatsForSubjects)>0) {
            subjectsDF <- 
            getDFStatsPredictionsErrorsAndBehavioralMeasuresForSubjectsAndComponents( 
             sortvar=sortvar,
             modality=modality,
             clusterID=clusterID,
             condition=condition,
             filterSFPStatsFunc=filterSFPStatsFunc,
             filterDFPStatsFunc=filterDFPStatsFunc,
             getUnselectedSFPStats=getUnselectedSFPStats,
             getUnselectedDFPStats=getUnselectedDFPStats,
             getSFPAndDFPStatsForSubjectAndComponentFunc=
              getSFPAndDFPStatsForSubjectAndComponentFunc,
             getSFPStatsForSubjectAndComponentFunc=
              getSFPStatsForSubjectAndComponentFunc,
             sFPStatsForSubjects=sFPStatsForSubjects,
             dFPStatsForSubjects=dFPStatsForSubjects,
             minAndMaxSFPDOfBestPredictionsFilenamePattern=
              minAndMaxSFPDOfBestPredictionsFilenamePattern,
             analyzedDataFilenamePattern=analyzedDataFilenamePattern,
             modelSignificance=modelSignificance)
            df <- rbind(df, subjectsDF)
        }
    }
    return(df)
}
getDFStatsPredictionsErrorsAndBehavioralMeasuresForSubjectsAndComponents <- 
 function(sortvar, modality, clusterID, condition, 
                   filterSFPStatsFunc,
                   filterDFPStatsFunc, 
                   getUnselectedSFPStats, 
                   getUnselectedDFPStats, 
                   getSFPAndDFPStatsForSubjectAndComponentFunc,
                   getSFPStatsForSubjectAndComponentFunc,
                   sFPStatsForSubjects, dFPStatsForSubjects, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   modelSignificance) {
    answer <- data.frame()
    for(i in 1:length(sFPStatsForSubjects)) {
        subjectName <- sFPStatsForSubjects[[i]]$subjectName
        component <- sFPStatsForSubjects[[i]]$component
#         show(sprintf("Processing subject %s and component %02d", 
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
            if(analyzedData$lrtRes$pValue<modelSignificance) {
                scSFPStats <- sFPStatsForSubjects[[i]]$stats
                scDFPStats <- selectDFPStats(subjectsDFPStats=
                                               dFPStatsForSubjects, 
                                              subjectName=subjectName,
                                              condition=condition)
                filteredSFPStats <- filterSFPStatsFunc(stats=scSFPStats)
                filteredDFPStats <- filterDFPStatsFunc(stats=scDFPStats)
                if((filteredSFPStats && filteredDFPStats) || 
                   (filteredSFPStats && !filteredDFPStats && getUnselectedDFPStats) ||
                   (!filteredSFPStats && filteredDFPStats && getUnselectedSFPStats) ||
                   (!filteredSFPStats && !filteredDFPStats && getUnselectedSFPStats &&
                    getUnselectedDFPStats)) {
                    scDF <- getSFPAndDFPStatsForSubjectAndComponentFunc(
                             scSFPStats=scSFPStats, 
                             scDFPStats=scDFPStats)
                    scDF$subjectName <- subjectName
                    scDF$component <- component
                    answer <- rbind(answer, scDF)
                } else {
                    if((filteredSFPStats && !filteredDFPStats && !getUnselectedDFPStats) ||
                       (!filteredSFPStats && !filteredDFPStats && getUnselectedSFPStats && !getUnselectedDFPStats)) {
                        scDF <- getSFPStatsForSubjectAndComponentFunc(
                                 scSFPStats=scSFPStats, 
                                 scDFPStats=scDFPStats)
                        scDF$subjectName <- subjectName
                        scDF$component <- component
                        answer <- rbind(answer, scDF)
                    }
                }
            }
        }
    }
    if(nrow(answer)>0) {
        answer$modality <- modality
        answer$clusterID <- clusterID
        answer$condition <- condition
    }
    return(answer)
}

