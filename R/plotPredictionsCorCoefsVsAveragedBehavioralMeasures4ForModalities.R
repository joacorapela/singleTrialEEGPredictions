plotPredictionsCorCoefsVsAveragedBehavioralMeasures4ForModalities <-
 function(sortvar, modalities, clustersIDs, conditions, 
                   modelSignificance,
                   getSubjectBehavioralMeasureCIFunc,
                   annotationPattern, 
                   scFilenamePattern, 
                   errorRatesAndMeanRTsStats,
                   analyzedConditionsFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   statsCorCoefFilenamePattern,
                   plotsFilenamePattern, 
                   xCorCoefAnnotation, yCorCoefAnnotation, 
                   hjust, vjust,
                   sizeLabels, sizeAnnotations, xlab, ylab, main, 
                   width, height, ...) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotPredictionsCorCoefsVsAveragedBehavioralMeasures4ForClusters(
         sortvar=sortvar, 
         modality=modality, 
         clustersIDs=clustersIDs, 
         conditions=conditions, 
         modelSignificance=modelSignificance,
         getSubjectBehavioralMeasureCIFunc=getSubjectBehavioralMeasureCIFunc,
         annotationPattern=annotationPattern, 
         scFilenamePattern=scFilenamePattern,
         errorRatesAndMeanRTsStats=errorRatesAndMeanRTsStats,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         statsCorCoefFilenamePattern=statsCorCoefFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         xCorCoefAnnotation=xCorCoefAnnotation, 
         yCorCoefAnnotation=yCorCoefAnnotation, 
         hjust=hjust, vjust=vjust,
         sizeLabels=sizeLabels, 
         sizeAnnotations=
         sizeAnnotations, 
         xlab=xlab, 
         ylab=ylab, 
         main=main, 
         width=width, 
         height=height, ...)
    }
}

plotPredictionsCorCoefsVsAveragedBehavioralMeasures4ForClusters <-
 function(sortvar, modality, clustersIDs, conditions, 
                   modelSignificance,
                   getSubjectBehavioralMeasureCIFunc,
                   annotationPattern, 
                   scFilenamePattern, 
                   errorRatesAndMeanRTsStats,
                   analyzedConditionsFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   statsCorCoefFilenamePattern,
                   plotsFilenamePattern,
                   xCorCoefAnnotation, yCorCoefAnnotation, 
                   hjust, vjust,
                   sizeLabels, sizeAnnotations, xlab, ylab, main, 
                   width, height, ...) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID, scFilename)
        plotPredictionsCorCoefsVsAveragedBehavioralMeasures4ForConditions(
         sortvar=sortvar, 
         modality=modality, 
         clusterID=clusterID, 
         conditions=conditions, 
         modelSignificance=modelSignificance,
         getSubjectBehavioralMeasureCIFunc=getSubjectBehavioralMeasureCIFunc,
         annotationPattern=annotationPattern, 
         subjectsAndComponents=subjectsAndComponents,
         errorRatesAndMeanRTsStats=errorRatesAndMeanRTsStats,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern, 
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         statsCorCoefFilenamePattern=statsCorCoefFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         xCorCoefAnnotation=xCorCoefAnnotation, 
         yCorCoefAnnotation=yCorCoefAnnotation, 
         hjust=hjust, vjust=vjust,
         sizeLabels=sizeLabels, 
         sizeAnnotations=sizeAnnotations, 
         xlab=xlab, 
         ylab=ylab, 
         main=main, 
         width=width, 
         height=height, ...)
    }
}

plotPredictionsCorCoefsVsAveragedBehavioralMeasures4ForConditions <-
 function(sortvar, modality, clusterID, conditions, 
                   modelSignificance,
                   getSubjectBehavioralMeasureCIFunc,
                   annotationPattern, 
                   subjectsAndComponents, 
                   errorRatesAndMeanRTsStats,
                   analyzedConditionsFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   statsCorCoefFilenamePattern,
                   plotsFilenamePattern,
                   xCorCoefAnnotation, yCorCoefAnnotation, 
                   hjust, vjust,
                   sizeLabels, sizeAnnotations, xlab, ylab, main, 
                   width, height, ...) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        plotFilename <- sprintf(plotsFilenamePattern, 
                                 clusterID,
                                 modality, 
                                 sortvar, 
                                 clusterID, 
                                 condition)
        trellis.device("postscript", width=width, height=height, onefile=FALSE,
                                      horizontal=FALSE, file=plotFilename)
        plotPredictionsCorCoefsVsAveragedBehavioralMeasures4ForSubjects(
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         modelSignificance=modelSignificance,
         getSubjectBehavioralMeasureCIFunc=getSubjectBehavioralMeasureCIFunc,
         annotationPattern=annotationPattern,
         subjectsAndComponents=subjectsAndComponents,
         errorRatesAndMeanRTsStats=errorRatesAndMeanRTsStats,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern, 
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         statsCorCoefFilenamePattern=statsCorCoefFilenamePattern,
         xlab=xlab,
         ylab=ylab,
         main=main,
         xCorCoefAnnotation=xCorCoefAnnotation,
         yCorCoefAnnotation=yCorCoefAnnotation,
         hjust=hjust, vjust=vjust,
         sizeLabels=sizeLabels, 
         sizeAnnotations=sizeAnnotations,
         ...)
        dev.off()
    }
}

plotPredictionsCorCoefsVsAveragedBehavioralMeasures4ForSubjects <-
 function(sortvar, modality, clusterID, condition, 
                   adjustedPValuesDF,
                   modelSignificance,
                   getSubjectBehavioralMeasureCIFunc,
                   annotationPattern,
                   subjectsAndComponents, 
                   errorRatesAndMeanRTsStats,
                   analyzedConditionsFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   statsCorCoefFilenamePattern,
                   xlab,
                   ylab="Correlation Coefficient",
                   main="", 
                   xCorCoefAnnotation=0.05, 
                   yCorCoefAnnotation=0.3, 
                   hjust, vjust,
                   sizeLabels=2.5, 
                   sizeAnnotations=4,
                   ...) {
    predSFPDurCorsCIs <- c()
    behavioralMeasuresCIs <- c()
    scNames <- c()
    nValidPredSFPDurCorsCIs <- 0
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
        minAndMaxSFPDOfBestPredictionsFilename <- 
         sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, 
                  clusterID, clusterID, condition, sortvar, modality, 
                  subjectName, component)
        res <- readLines(minAndMaxSFPDOfBestPredictionsFilename)
        minSFPD <- as.integer(res[1])
        maxSFPD <- as.integer(res[2])
        scName <- sprintf("%s%02d", subjectName, component)
        scNames <- c(scNames, scName)
        if(is.nan(minSFPD) || is.na(maxSFPD)) {
            predSFPDurCorsCIs <- rbind(predSFPDurCorsCIs, c(0, 0, 0))
        } else {
            analyzedConditionFilename <- sprintf(analyzedConditionsFilenamePattern,
                                              clusterID,
                                              clusterID,
                                              condition,
                                              sortvar,
                                              modality,
                                              subjectName,
                                              component,
                                              minSFPD,
                                              maxSFPD)
            analyzedCondition <- get(load(analyzedConditionFilename))
            if(!is.null(analyzedCondition$predSFPDurCorCI)) {
                if(is.nan(modelSignificance)) {
                    nValidPredSFPDurCorsCIs <- nValidPredSFPDurCorsCIs + 1 
                    predSFPDurCorsCIs <- rbind(predSFPDurCorsCIs, 
                                                analyzedCondition$
                                                 predSFPDurCorCI) 
                } else {
                    if(analyzedCondition$lrtRes$pValue<modelSignificance) {
                        nValidPredSFPDurCorsCIs <- nValidPredSFPDurCorsCIs + 1 
                        predSFPDurCorsCIs <- rbind(predSFPDurCorsCIs, 
                                                    analyzedCondition$
                                                    predSFPDurCorCI) 
                    } else {
                        predSFPDurCorsCIs <- rbind(predSFPDurCorsCIs, c(0, 0, 0))
                    }
                }
            } else {
                predSFPDurCorsCIs <- rbind(predSFPDurCorsCIs, c(0, 0, 0))
            }
        }
        behavioralMeasureCI <- 
         getSubjectBehavioralMeasureCIFunc(subjectName=subjectName, 
                                            condition=condition, 
                                            stats=errorRatesAndMeanRTsStats)
        behavioralMeasuresCIs <- rbind(behavioralMeasuresCIs, 
                                        behavioralMeasureCI)
    }
    if(length(scNames)>1 && nValidPredSFPDurCorsCIs>1) {
        statsCorCoefFilename <- sprintf(statsCorCoefFilenamePattern, clusterID,
                                                                     clusterID,
                                                                     condition,
                                                                     sortvar,
                                                                     modality)
        statsCorCoef <- get(load(statsCorCoefFilename))
        r <- statsCorCoef$bootCI[1]
        dfIndex <- which(adjustedPValuesDF$modality==modality &
                          adjustedPValuesDF$clusterID==clusterID &
                          adjustedPValuesDF$condition==condition)
        if(length(dfIndex)>0) {
            p <- adjustedPValuesDF[dfIndex,"pValue"]
            adjP <- adjustedPValuesDF[dfIndex, "adjPValue"]
            statsCorCoefAnnotation <- sprintf(annotationPattern, r, p, adjP)
            plotValuesCIsVsAveragedBehavioralMeasures(
             valuesCIs=predSFPDurCorsCIs, 
             behavioralMeasuresCIs=behavioralMeasuresCIs, 
             scNames=scNames,
             statsCorCoefAnnotation=statsCorCoefAnnotation,
             xlab=xlab, ylab=ylab, main=main,
             xCorCoefAnnotation=xCorCoefAnnotation,
             yCorCoefAnnotation=yCorCoefAnnotation,
             hjust=hjust,
             vjust=vjust,
             sizeLabels=sizeLabels,
             sizeAnnotations=sizeAnnotations,
             ...)
        } else {
            print(getEmptyPlot())
        }
    } else {
        print(getEmptyPlot())
    }
}
