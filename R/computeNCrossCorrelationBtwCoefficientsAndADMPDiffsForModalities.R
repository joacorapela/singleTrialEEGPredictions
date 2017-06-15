
computeNCrossCorrelationBtwCoefficientsAndADMPDiffsForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions,
                   earlyFromPercentage, earlyToPercentage,
                   lateFromPercentage, lateToPercentage,
                   modelSignificance, minTimeSpanCoefsMS, srate,
                   scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   preProcessedPhaseERPIFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    answer <- c()
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        res <- 
         computeNCrossCorrelationBtwCoefficientsAndADMPDiffsForClusters(
          sortvar=sortvar,
          modality=modality,
          clustersIDs=clustersIDs,
          conditions=conditions,
          earlyFromPercentage=earlyFromPercentage,
          earlyToPercentage=earlyToPercentage,
          lateFromPercentage=lateFromPercentage,
          lateToPercentage=lateToPercentage,
          modelSignificance=modelSignificance,
          minTimeSpanCoefsMS=minTimeSpanCoefsMS,
          srate=srate,
          scFilenamePattern=scFilenamePattern,
          minAndMaxSFPDOfBestPredictionsFilenamePattern=
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
          preProcessedPhaseERPIFilenamePattern=
           preProcessedPhaseERPIFilenamePattern,
          analyzedDataFilenamePattern=
           analyzedDataFilenamePattern, 
          ...)
        answer <- rbind(answer, 
                         cbind(modality=rep(modality, times=nrow(res)), res))
    }
    return(answer)
}
computeNCrossCorrelationBtwCoefficientsAndADMPDiffsForClusters <- 
 function(sortvar, modality, clustersIDs, conditions,
                   earlyFromPercentage, earlyToPercentage,
                   lateFromPercentage, lateToPercentage,
                   modelSignificance, minTimeSpanCoefsMS, srate,
                   scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   preProcessedPhaseERPIFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    answer <- c()
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %02d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                            scFilename=scFilename)
        res <- 
         computeNCrossCorrelationBtwCoefficientsAndADMPDiffsForConditions(
          sortvar=sortvar,
          modality=modality,
          clusterID=clusterID,
          conditions=conditions,
          earlyFromPercentage=earlyFromPercentage,
          earlyToPercentage=earlyToPercentage,
          lateFromPercentage=lateFromPercentage,
          lateToPercentage=lateToPercentage,
          modelSignificance=modelSignificance,
          minTimeSpanCoefsMS=minTimeSpanCoefsMS,
          srate=srate,
          subjectsAndComponents=subjectsAndComponents,
          minAndMaxSFPDOfBestPredictionsFilenamePattern=
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
          preProcessedPhaseERPIFilenamePattern,
          analyzedDataFilenamePattern=analyzedDataFilenamePattern,
          ...)
        answer <- rbind(answer, 
                         cbind(clusterID=rep(clusterID, times=nrow(res)), res))
    }
    return(answer)
}
computeNCrossCorrelationBtwCoefficientsAndADMPDiffsForConditions <- 
 function(sortvar, modality, clusterID, conditions,
                   earlyFromPercentage, earlyToPercentage,
                   lateFromPercentage, lateToPercentage,
                   modelSignificance, minTimeSpanCoefsMS, srate,
                   subjectsAndComponents, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   preProcessedPhaseERPIFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    answer <- c()
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        res <- 
         computeNCrossCorrelationBtwCoefficientsAndADMPDiffsForSubjectsAndComponents(
          sortvar=sortvar,
          modality=modality,
          clusterID=clusterID,
          condition=condition,
          earlyFromPercentage=earlyFromPercentage,
          earlyToPercentage=earlyToPercentage,
          lateFromPercentage=lateFromPercentage,
          lateToPercentage=lateToPercentage,
          modelSignificance=modelSignificance,
          subjectsAndComponents=subjectsAndComponents,
          minTimeSpanCoefsMS=minTimeSpanCoefsMS,
          srate=srate,
          minAndMaxSFPDOfBestPredictionsFilenamePattern=
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
          preProcessedPhaseERPIFilenamePattern,
          analyzedDataFilenamePattern=analyzedDataFilenamePattern,
          ...)
        answer <- rbind(answer, 
                         cbind(condition=rep(condition, times=nrow(res)), res))
    }
    return(answer)
}
computeNCrossCorrelationBtwCoefficientsAndADMPDiffsForSubjectsAndComponents <- 
 function(sortvar, modality, clusterID, condition,
                   earlyFromPercentage, earlyToPercentage,
                   lateFromPercentage, lateToPercentage,
                   modelSignificance, minTimeSpanCoefsMS, srate,
                   subjectsAndComponents,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   preProcessedPhaseERPIFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    subjectsNames <- c()
    components <- c()
    correlations <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
         correlation <- 
          computeNCrossCorrelationBtwCoefficientsAndADMPDiffsForSubjectAndComponent(
           sortvar=sortvar,
           modality=modality,
           clusterID=clusterID,
           condition=condition,
           earlyFromPercentage=earlyFromPercentage,
           earlyToPercentage=earlyToPercentage,
           lateFromPercentage=lateFromPercentage,
           lateToPercentage=lateToPercentage,
           subjectName=subjectName,
           component=component,
           modelSignificance=modelSignificance,
           minTimeSpanCoefsMS=minTimeSpanCoefsMS,
           srate=srate,
           minAndMaxSFPDOfBestPredictionsFilenamePattern=
            minAndMaxSFPDOfBestPredictionsFilenamePattern,
           preProcessedPhaseERPIFilenamePattern=
            preProcessedPhaseERPIFilenamePattern,
           analyzedDataFilenamePattern=analyzedDataFilenamePattern,
           ...)
        if(!is.null(correlation)) {
            subjectsNames <- c(subjectsNames, subjectName)
            components <- c(components, component)
            correlations <- c(correlations, correlation)
        }
    }
    if(length(subjectsNames)>0) {
        return(data.frame(subjectName=subjectsNames, 
                           component=components,
                           correlation=correlations))
    } else {
        return(data.frame())
    }
}
computeNCrossCorrelationBtwCoefficientsAndADMPDiffsForSubjectAndComponent <- 
 function(sortvar, modality, clusterID, condition, 
                   earlyFromPercentage, earlyToPercentage,
                   lateFromPercentage, lateToPercentage,
                   subjectName, component, modelSignificance,
                   minTimeSpanCoefsMS, srate,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   preProcessedPhaseERPIFilenamePattern,
                   analyzedDataFilenamePattern, ...) {
    minAndMaxSFPDOfBestPredictionsFilename <- 
     sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, clusterID,
              clusterID, condition, sortvar, modality, subjectName, component)
    res <- readLines(minAndMaxSFPDOfBestPredictionsFilename)
    minSFPD <- as.integer(res[1])
    maxSFPD <- as.integer(res[2])
    if(!is.na(minSFPD) && !is.na(maxSFPD)) {
        ppPhaseERPImageFilename <- sprintf(preProcessedPhaseERPIFilenamePattern,
                                            clusterID,
                                            condition,
                                            sortvar,
                                            modality,
                                            subjectName,
                                            component)
        ppPhaseERPImage <- get(load(ppPhaseERPImageFilename))
        if(!is.null(ppPhaseERPImage$erpImage)) {
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
            minTimeSpanCoefsSamples <- minTimeSpanCoefsMS/1000*srate
            if(!is.null(analyzedData$lrtRes) &&
               analyzedData$lrtRes$pValue<modelSignificance && 
               !is.null(analyzedData$coefsCIs) &&
                (nrow(analyzedData$coefsCIs)-1)>minTimeSpanCoefsSamples)  {
                avgDMPEarlyMinusLateTrials <- 
                 computeAvgDMPDiffsLateMinusEarlyTrials(dmps=ppPhaseERPImage$erpImage,
                                                         sfpds=ppPhaseERPImage$sfpds,
                                                         earlyFromPercentage=earlyFromPercentage,
                                                         earlyToPercentage=earlyToPercentage,
                                                         lateFromPercentage=lateFromPercentage,
                                                         lateToPercentage=lateToPercentage)
                crossCorrelation <- ccf(analyzedData$coefsCIs[-1,1],
                                         avgDMPEarlyMinusLateTrials,
                                         lag.max=0, plot=FALSE)$acf[1,1,1]
#                 nCoefs <- analyzedData$coefsCIs[-1,1]/
#                            sqrt(cor(analyzedData$coefsCIs[-1,1],
#                                      analyzedData$coefsCIs[-1,1]))
#                 navgDMPEarlyMinusLateTrials <- 
#                     avgDMPEarlyMinusLateTrials/
#                      sqrt(cor(avgDMPEarlyMinusLateTrials,
#                                avgDMPEarlyMinusLateTrials))
#                 crossCorrelation <- cor(nCoefs, navgDMPEarlyMinusLateTrials)
                return(crossCorrelation)
            }
        }
    }
    return(NULL)
}
