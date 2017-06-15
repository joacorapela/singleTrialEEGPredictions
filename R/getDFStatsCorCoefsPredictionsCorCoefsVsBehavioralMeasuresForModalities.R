getDFStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasuresForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, filterFunc,
                   getDFStatsForConditionFunc, statsFilenamePattern) {
    dataFrame <- c()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        mDataFrame <- 
         getDFStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasuresForClusters(
          sortvar=sortvar, 
          modality=modality, 
          clustersIDs=clustersIDs,
          conditions=conditions,
          filterFunc=filterFunc,
          getDFStatsForConditionFunc=getDFStatsForConditionFunc,
          statsFilenamePattern=statsFilenamePattern)
        if(length(mDataFrame)>0) {
            mDataFrame["modality"] <- modality
            dataFrame <- rbind(dataFrame, mDataFrame)
        }
    }
    if(length(dataFrame)>0) {
        dataFrame$modality  <- as.factor(dataFrame$modality)
        dataFrame$clusterID <- as.factor(dataFrame$clusterID)
        dataFrame$condition <- as.factor(dataFrame$condition)
    }
    return(dataFrame)
}

getDFStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasuresForClusters <-
 function(sortvar, modality, clustersIDs, conditions, filterFunc,
                   getDFStatsForConditionFunc, statsFilenamePattern) {
    dataFrame <- c()
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %d", clusterID))
        cDataFrame <-
         getDFStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasuresForConditions(
          sortvar=sortvar, 
          modality=modality, 
          clusterID=clusterID,
          conditions=conditions,
          filterFunc=filterFunc,
          getDFStatsForConditionFunc=getDFStatsForConditionFunc,
          statsFilenamePattern=statsFilenamePattern)
        if(length(cDataFrame)>0) {
            cDataFrame["clusterID"] <- clusterID
            dataFrame <- rbind(dataFrame, cDataFrame)
        }
    }
    return(dataFrame)
}

getDFStatsCorCoefsPredictionsCorCoefsVsBehavioralMeasuresForConditions <-
 function(sortvar, modality, clusterID, conditions, filterFunc,
                   getDFStatsForConditionFunc, statsFilenamePattern) {
    dataFrame <- c()
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        statsFilename <- sprintf(statsFilenamePattern, clusterID, clusterID, 
                                                       condition, sortvar, 
                                                       modality)
        stats <- get(load(statsFilename))
        cDataFrame <- NULL
        if(filterFunc(stats=stats)) {
            cDataFrame <- getDFStatsForConditionFunc(stats=stats)
        }
        if(!is.null(cDataFrame)) {
            cDataFrame["condition"] <- condition
            dataFrame <- rbind(dataFrame, cDataFrame)
        }
    }
    return(dataFrame)
}
