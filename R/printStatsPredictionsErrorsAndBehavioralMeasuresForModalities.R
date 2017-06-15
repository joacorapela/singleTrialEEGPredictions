
printStatsPredictionsErrorsAndBehavioralMeasuresForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions,
                      sFPStatsForModalities, 
                      dFPStats,
                      modelSignificance,
                      minAndMaxSFPDOfBestPredictionsFilenamePattern,
                      analyzedDataFilenamePattern,
                      filterSFPStatsFunc,
                      filterDFPStatsFunc, 
                      printUnselectedSFPStats,
                      printUnselectedDFPStats,
                      printSFPAndDFPStatsFunc,
                      printSFPStatsFunc,
                      printFilename) {
    con <- file(printFilename, open="wt")
    for(modality in modalities) {
        sFPStatsForClusters <-
         getItemInAVShiftList(listOfItems=sFPStatsForModalities,
                               listFieldName="stats",
                               keyFieldNames=c("modality"),
                               keyFieldValues=c(modality))
        printStatsPredictionsErrorsAndBehavioralMeasuresForClusters( 
         sortvar=sortvar,
         modality=modality,
         clustersIDs=clustersIDs,
         conditions=conditions, 
         sFPStatsForClusters=sFPStatsForClusters, 
         dFPStats=dFPStats,
         modelSignificance=modelSignificance,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         filterSFPStatsFunc=filterSFPStatsFunc,
         filterDFPStatsFunc=filterDFPStatsFunc,
         printUnselectedSFPStats=printUnselectedSFPStats,
         printUnselectedDFPStats=printUnselectedDFPStats,
         printSFPAndDFPStatsFunc=printSFPAndDFPStatsFunc,
         printSFPStatsFunc=printSFPStatsFunc,
         con=con)
    }
    close(con)
}
printStatsPredictionsErrorsAndBehavioralMeasuresForClusters <- 
 function(sortvar, modality, clustersIDs, conditions,
                    sFPStatsForClusters, 
                    dFPStats, 
                    modelSignificance,
                    minAndMaxSFPDOfBestPredictionsFilenamePattern,
                    analyzedDataFilenamePattern,
                    filterSFPStatsFunc, 
                    filterDFPStatsFunc,
                    printUnselectedSFPStats,
                    printUnselectedDFPStats,
                    printSFPAndDFPStatsFunc,
                    printSFPStatsFunc,
                    con) {
    for(clusterID in clustersIDs) {
        sFPStatsForConditions <-
         getItemInAVShiftList(listOfItems=sFPStatsForClusters,
                               listFieldName="stats",
                               keyFieldNames=c("clusterID"),
                               keyFieldValues=c(clusterID))
        printStatsPredictionsErrorsAndBehavioralMeasuresForConditions( 
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         conditions=conditions, 
         sFPStatsForConditions=sFPStatsForConditions,
         dFPStats=dFPStats,
         modelSignificance=modelSignificance,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         filterSFPStatsFunc=filterSFPStatsFunc,
         filterDFPStatsFunc=filterDFPStatsFunc,
         printUnselectedSFPStats=printUnselectedSFPStats,
         printUnselectedDFPStats=printUnselectedDFPStats,
         printSFPAndDFPStatsFunc=printSFPAndDFPStatsFunc,
         printSFPStatsFunc=printSFPStatsFunc,
         con=con)
    }
}
printStatsPredictionsErrorsAndBehavioralMeasuresForConditions <- 
 function(sortvar, modality, clusterID, conditions,
                    sFPStatsForConditions,
                    dFPStats, 
                    modelSignificance,
                    minAndMaxSFPDOfBestPredictionsFilenamePattern,
                    analyzedDataFilenamePattern,
                    filterSFPStatsFunc, 
                    filterDFPStatsFunc,
                    printUnselectedSFPStats,
                    printUnselectedDFPStats,
                    printSFPAndDFPStatsFunc,
                    printSFPStatsFunc,
                    con) {
    for(condition in conditions) {
        sFPStatsForSubjects <-
         getItemInAVShiftList(listOfItems=sFPStatsForConditions,
                               listFieldName="stats",
                               keyFieldNames=c("condition"),
                               keyFieldValues=c(condition))
        if(length(sFPStatsForSubjects)>0) {
            printStatsPredictionsErrorsAndBehavioralMeasuresForSubjects( 
            sortvar=sortvar,
            modality=modality,
            clusterID=clusterID,
            condition=condition, 
            sFPStatsForSubjects=sFPStatsForSubjects,
            dFPStats=dFPStats,
            modelSignificance=modelSignificance,
            minAndMaxSFPDOfBestPredictionsFilenamePattern=
            minAndMaxSFPDOfBestPredictionsFilenamePattern,
            analyzedDataFilenamePattern=analyzedDataFilenamePattern,
            filterSFPStatsFunc=filterSFPStatsFunc,
            filterDFPStatsFunc=filterDFPStatsFunc,
            printUnselectedSFPStats=printUnselectedSFPStats,
            printUnselectedDFPStats=printUnselectedDFPStats,
            printSFPAndDFPStatsFunc=printSFPAndDFPStatsFunc,
            printSFPStatsFunc=printSFPStatsFunc,
            con=con)
        }
    }
}
printStatsPredictionsErrorsAndBehavioralMeasuresForSubjects <- 
 function(sortvar, modality, clusterID, condition,
                    sFPStatsForSubjects, dFPStats, 
                    modelSignificance,
                    minAndMaxSFPDOfBestPredictionsFilenamePattern,
                    analyzedDataFilenamePattern,
                    filterSFPStatsFunc, filterDFPStatsFunc, 
                    printUnselectedSFPStats,
                    printUnselectedDFPStats,
                    printSFPAndDFPStatsFunc,
                    printSFPStatsFunc,
                    con) {
    for(i in 1:length(sFPStatsForSubjects)) {
        sFPStatsForSubject <- sFPStatsForSubjects[[i]]
        selectedDFPStats <- 
         selectDFPStats(subjectsDFPStats=dFPStats, 
                         subjectName=sFPStatsForSubject$subjectName,
                         condition=condition)
        minAndMaxSFPDOfBestPredictionsFilename <- 
         sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, 
                  clusterID, clusterID, condition, sortvar, modality, 
                  sFPStatsForSubject$subjectName, sFPStatsForSubject$component)
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
                                             sFPStatsForSubject$subjectName,
                                             sFPStatsForSubject$component,
                                             minSFPD,
                                             maxSFPD)
            analyzedData <- get(load(analyzedDataFilename))
            if(length(analyzedData$lrtRes)>0 &&
                       analyzedData$lrtRes$pValue<modelSignificance &&
                       length(sFPStatsForSubject$stats)>0) {
                printStatsPredictionsErrorsAndBehavioralMeasuresForSubject(
                 modality=modality, clusterID=clusterID, condition=condition,
                 subjectName=sFPStatsForSubject$subjectName,
                 component=sFPStatsForSubject$component,
                 sFPStatsForSubject=sFPStatsForSubject$stats, 
                 selectedDFPStats=selectedDFPStats,
                 modelPredictionCI=analyzedData$predSFPDurCorCI,
                 filterSFPStatsFunc=filterSFPStatsFunc, 
                 filterDFPStatsFunc=filterDFPStatsFunc, 
                 printUnselectedSFPStats=printUnselectedSFPStats,
                 printUnselectedDFPStats=printUnselectedDFPStats,
                 printSFPAndDFPStatsFunc=printSFPAndDFPStatsFunc,
                 printSFPStatsFunc=printSFPStatsFunc,
                 con=con)
            }
        }
    }
}
printStatsPredictionsErrorsAndBehavioralMeasuresForSubject <-
 function(modality, clusterID, condition, subjectName, component,
                    sFPStatsForSubject, 
                    selectedDFPStats, 
                    modelPredictionCI,
                    filterSFPStatsFunc,
                    filterDFPStatsFunc, 
                    printUnselectedSFPStats, 
                    printUnselectedDFPStats, 
                    printSFPAndDFPStatsFunc,
                    printSFPStatsFunc,
                    con) {
    filteredSFPStats <- filterSFPStatsFunc(sFPStatsForSubject)
    filteredDFPStats <- filterDFPStatsFunc(selectedDFPStats)
    if((filteredSFPStats && filteredDFPStats) || 
       (filteredSFPStats && !filteredDFPStats && printUnselectedDFPStats) ||
       (!filteredSFPStats && filteredDFPStats && printUnselectedSFPStats) ||
       (!filteredSFPStats && !filteredDFPStats && printUnselectedSFPStats &&
        printUnselectedDFPStats)) {
        printSFPAndDFPStatsFunc(modality=modality, 
                                 clusterID=clusterID,
                                 condition=condition,
                                 subjectName=subjectName,
                                 component=component,
                                 sFPStatsForSubject=sFPStatsForSubject, 
                                 selectedDFPStats=selectedDFPStats,
                                 filteredSFPStats=filteredSFPStats,
                                 filteredDFPStats=filteredDFPStats, 
                                 con=con)
    } else {
        if((filteredSFPStats && !filteredDFPStats && 
                                !printUnselectedDFPStats) ||
           (!filteredSFPStats && !filteredDFPStats &&
                                   printUnselectedSFPStats && 
                                   !printUnselectedDFPStats)) {
            printSFPStatsFunc(modality=modality, 
                               clusterID=clusterID,
                               condition=condition,
                               subjectName=subjectName,
                               component=component,
                               sFPStatsForSubject=sFPStatsForSubject, 
                               filteredSFPStats=filteredSFPStats,
                               con=con)
        }
    }
}
