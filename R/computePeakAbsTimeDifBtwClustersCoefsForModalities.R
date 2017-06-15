
computePeakAbsTimeDifBtwClustersCoefsForModalities <- 
 function(sortvar, modalities, clustersIDs1, clustersIDs2, conditions,
                   modelSignificance, srate, lowpassFilterOrder, margin,
                   minPeakTime, onlySignificantPeaks,
                   scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    answer <- c()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        res <- 
         computePeakAbsTimeDifBtwClustersCoefsForClusters(
          sortvar=sortvar,
          modality=modality,
          clustersIDs1=clustersIDs1,
          clustersIDs2=clustersIDs2,
          conditions=conditions,
          modelSignificance=modelSignificance,
          srate=srate,
          lowpassFilterOrder=lowpassFilterOrder,
          margin=margin,
          minPeakTime=minPeakTime,
          onlySignificantPeaks=onlySignificantPeaks,
          scFilenamePattern=scFilenamePattern,
          minAndMaxSFPDOfBestPredictionsFilenamePattern=
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
          analyzedDataFilenamePattern=
           analyzedDataFilenamePattern, 
          ...)
        answer <- rbind(answer, 
                         cbind(modality=rep(modality, times=nrow(res)), res))
    }
    return(answer)
}
computePeakAbsTimeDifBtwClustersCoefsForClusters <- 
 function(sortvar, modality, clustersIDs1, clustersIDs2, conditions,
                   modelSignificance, srate, lowpassFilterOrder, margin,
                   minPeakTime, onlySignificantPeaks,
                   scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    answer <- c()
    for(clusterID1 in clustersIDs1) {
        show(sprintf("Processing cluster1 %02d", clusterID1))
        for(clusterID2 in clustersIDs2) {
            show(sprintf("Processing cluster2 %02d", clusterID2))
            scFilename1 <- sprintf(scFilenamePattern, clusterID1, sortvar)
            subjectsAndComponents1 <-
             getSubjectsAndComponentsInCluster(clusterID=clusterID1, 
                                                scFilename=scFilename1)
            scFilename2 <- sprintf(scFilenamePattern, clusterID2, sortvar)
            subjectsAndComponents2 <-
             getSubjectsAndComponentsInCluster(clusterID=clusterID2, 
                                                scFilename=scFilename2)
            res <- 
             computePeakAbsTimeDifBtwClustersCoefsForConditions(
              sortvar=sortvar,
              modality=modality,
              clusterID1=clusterID1,
              clusterID2=clusterID2,
              conditions=conditions,
              modelSignificance=modelSignificance,
              srate=srate,
              lowpassFilterOrder=lowpassFilterOrder,
              margin=margin,
              minPeakTime=minPeakTime,
              onlySignificantPeaks=onlySignificantPeaks,
              subjectsAndComponents1=subjectsAndComponents1,
              subjectsAndComponents2=subjectsAndComponents2,
              minAndMaxSFPDOfBestPredictionsFilenamePattern=
               minAndMaxSFPDOfBestPredictionsFilenamePattern,
              analyzedDataFilenamePattern=analyzedDataFilenamePattern,
              ...)
            answer <- rbind(answer, 
                             cbind(clusterID1=rep(clusterID1, times=nrow(res)), 
                                    clusterID2=rep(clusterID2, times=nrow(res)),
                                    res))
        }
    }
    return(answer)
}
computePeakAbsTimeDifBtwClustersCoefsForConditions <- 
 function(sortvar, modality, clusterID1, clusterID2, conditions,
                   modelSignificance, srate, lowpassFilterOrder, margin,
                   minPeakTime, onlySignificantPeaks,
                   subjectsAndComponents1, subjectsAndComponents2, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    answer <- c()
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        res <- 
         computePeakAbsTimeDifBtwClustersCoefsForSubjectsAndComponents(
          sortvar=sortvar,
          modality=modality,
          clusterID1=clusterID1,
          clusterID2=clusterID2,
          condition=condition,
          modelSignificance=modelSignificance,
          srate=srate,
          lowpassFilterOrder=lowpassFilterOrder,
          margin=margin,
          minPeakTime=minPeakTime,
          onlySignificantPeaks=onlySignificantPeaks,
          subjectsAndComponents1=subjectsAndComponents1,
          subjectsAndComponents2=subjectsAndComponents2,
          minAndMaxSFPDOfBestPredictionsFilenamePattern=
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
          analyzedDataFilenamePattern=analyzedDataFilenamePattern,
          ...)
        answer <- rbind(answer, 
                         cbind(condition=rep(condition, times=nrow(res)), res))
    }
    return(answer)
}
computePeakAbsTimeDifBtwClustersCoefsForSubjectsAndComponents <- 
 function(sortvar, modality, clusterID1, clusterID2, condition,
                   modelSignificance, srate, lowpassFilterOrder, margin,
                   minPeakTime, onlySignificantPeaks,
                   subjectsAndComponents1, subjectsAndComponents2,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    subjectsNames <- c()
    component1s <- c()
    component2s <- c()
    difs <- c()
    pValues <- c()
    for(i in 1:nrow(subjectsAndComponents1)) {
        subjectName <- subjectsAndComponents1[i, "subjectName"]
        component1 <- subjectsAndComponents1[i, "component"]
        indicesOf1In2 <- 
         which(subjectsAndComponents2[, "subjectName"]==subjectName)
        j <- 1
        while(j<length(indicesOf1In2)) {
            component2 <- subjectsAndComponents2[indicesOf1In2[j], "component"]
            show(sprintf("Processing subject %s component %02d in cluster %02d and component %02d in cluster %02d", 
                         subjectName, 
                         component1, clusterID1, 
                         component2, clusterID2))
            dif <- 
             computePeakAbsTimeDifBtwClustersCoefsForSubjectAndComponent(
              sortvar=sortvar,
              modality=modality,
              clusterID1=clusterID1,
              clusterID2=clusterID2,
              condition=condition,
              subjectName=subjectName,
              component1=component1,
              component2=component2,
              modelSignificance=modelSignificance,
              srate=srate,
              lowpassFilterOrder=lowpassFilterOrder,
              margin=margin,
              minPeakTime=minPeakTime,
              onlySignificantPeaks=onlySignificantPeaks,
              minAndMaxSFPDOfBestPredictionsFilenamePattern=
               minAndMaxSFPDOfBestPredictionsFilenamePattern,
              analyzedDataFilenamePattern=analyzedDataFilenamePattern,
              ...)
            if(!is.null(dif)) {
                subjectsNames <- c(subjectsNames, subjectName)
                component1s <- c(component1s, component1)
                component2s <- c(component2s, component2)
                difs <- c(difs, dif)
            }
            j <- j+1
        }
    }
    if(length(subjectsNames)>0) {
        return(data.frame(subjectName=subjectsNames, 
                           component1=component1s,
                           component2=component2s,
                           dif=difs))
    } else {
        return(data.frame())
    }
}
computePeakAbsTimeDifBtwClustersCoefsForSubjectAndComponent <- 
 function(sortvar, modality, clusterID1, clusterID2, condition,
                   subjectName, component1, component2, modelSignificance,
                   srate, lowpassFilterOrder, margin, minPeakTime,
                   onlySignificantPeaks,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    c1MinAndMaxSFPDOfBestPredictionsFilename <- 
     sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, clusterID1,
              clusterID1, condition, sortvar, modality, subjectName, component1)
    res <- readLines(c1MinAndMaxSFPDOfBestPredictionsFilename)
    c1MinSFPD <- as.integer(res[1])
    c1MaxSFPD <- as.integer(res[2])
    c2MinAndMaxSFPDOfBestPredictionsFilename <- 
     sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, clusterID2,
              clusterID2, condition, sortvar, modality, subjectName, component2)
    res <- readLines(c2MinAndMaxSFPDOfBestPredictionsFilename)
    c2MinSFPD <- as.integer(res[1])
    c2MaxSFPD <- as.integer(res[2])
    dif <- NULL
    if(!is.na(c1MinSFPD) && !is.na(c1MaxSFPD) && 
       !is.na(c2MinSFPD) && !is.na(c2MaxSFPD)) {
        c1AnalyzedDataFilename <- sprintf(analyzedDataFilenamePattern,
                                       clusterID1,
                                       clusterID1,
                                       condition,
                                       sortvar,
                                       modality,
                                       subjectName,
                                       component1,
                                       c1MinSFPD,
                                       c1MaxSFPD)
        c2AnalyzedDataFilename <- sprintf(analyzedDataFilenamePattern,
                                       clusterID2,
                                       clusterID2,
                                       condition,
                                       sortvar,
                                       modality,
                                       subjectName,
                                       component2,
                                       c2MinSFPD,
                                       c2MaxSFPD)
        c1AnalyzedData <- get(load(c1AnalyzedDataFilename))
        c2AnalyzedData <- get(load(c2AnalyzedDataFilename))

        if(!is.null(c1AnalyzedData$lrtRes) &&
           c1AnalyzedData$lrtRes$pValue<modelSignificance && 
           !is.null(c2AnalyzedData$lrtRes) && 
           c2AnalyzedData$lrtRes$pValue<modelSignificance) {
            peakCoefInfo1 <- getPeakCoefInfo(analyzedData=c1AnalyzedData, 
                                              srate=srate, 
                                              lowpassFilterOrder=
                                               lowpassFilterOrder, 
                                              margin=margin, 
                                              minPeakTime=minPeakTime, 
                                              onlySignificantPeaks=
                                               onlySignificantPeaks)
            peakCoefInfo2 <- getPeakCoefInfo(analyzedData=c2AnalyzedData, 
                                              srate=srate, 
                                              lowpassFilterOrder=
                                               lowpassFilterOrder, 
                                              margin=margin, 
                                              minPeakTime=minPeakTime, 
                                              onlySignificantPeaks=
                                               onlySignificantPeaks)
            if(!is.nan(peakCoefInfo1$absPeakTime) && 
               !is.nan(peakCoefInfo2$absPeakTime)) {
                dif <- peakCoefInfo1$absPeakTime-peakCoefInfo2$absPeakTime
            }
        }
    }
    return(dif)
}
