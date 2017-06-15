printStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasures3ForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, filterFunc,
                   getConditionStatsDescFunc, statsFilenamePattern, con) {
    for(modality in modalities) {
        printStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasures3ForClusters(
         sortvar=sortvar, 
         modality=modality, 
         clustersIDs=clustersIDs,
         conditions=conditions,
         filterFunc=filterFunc,
         getConditionStatsDescFunc=getConditionStatsDescFunc,
         statsFilenamePattern=statsFilenamePattern,
         con=con)
    }
}

printStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasures3ForClusters <-
 function(sortvar, modality, clustersIDs, conditions, filterFunc, 
                   getConditionStatsDescFunc, statsFilenamePattern, con) {
    for(clusterID in clustersIDs) {
        printStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasures3ForConditions(
         sortvar=sortvar, 
         modality=modality, 
         clusterID=clusterID,
         conditions=conditions,
         filterFunc=filterFunc,
         getConditionStatsDescFunc=getConditionStatsDescFunc,
         statsFilenamePattern=statsFilenamePattern,
         con=con)
    }
}

printStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasures3ForConditions <-
 function(sortvar, modality, clusterID, conditions, filterFunc, 
                   getConditionStatsDescFunc, statsFilenamePattern, con) {
    for(condition in conditions) {
        printStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasures3ForCondition(
         sortvar=sortvar, 
         modality=modality, 
         clusterID=clusterID,
         condition=condition,
         filterFunc=filterFunc,
         getConditionStatsDescFunc=getConditionStatsDescFunc,
         statsFilenamePattern=statsFilenamePattern,
         con=con)
    }
}

printStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasures3ForCondition <-
 function(sortvar, modality, clusterID, condition, filterFunc, 
                   getConditionStatsDescFunc, statsFilenamePattern, con) {
    statsFilename <- sprintf(statsFilenamePattern, clusterID, clusterID, 
                                                   condition, sortvar, modality)
    stats <- get(load(statsFilename))
    if(filterFunc(stats=stats)) {
        desc <- getConditionStatsDescFunc(stats=stats)
        line <- sprintf("%s, C%02d, %s, %s", modality, clusterID, condition, 
                        desc)
        writeLines(line, con)
    }
}
