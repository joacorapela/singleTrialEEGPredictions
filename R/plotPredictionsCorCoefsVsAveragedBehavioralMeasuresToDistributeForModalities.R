plotPredictionsCorCoefsVsAveragedBehavioralMeasuresToDistributeForModalities <-
 function(sortvar, modalities, clustersIDs, conditions, 
                   modelSignificance,
                   annotationPattern, 
                   scFilenamePattern, 
                   analyzedConditionsFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   behavioralMeasuresFilenamePattern,
                   plotsFilenamePattern, 
                   xCorCoefAnnotation, yCorCoefAnnotation, 
                   hjust, vjust,
                   sizeLabels, sizeAnnotations, xlab, ylab, main, 
                   width, height, ...) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotPredictionsCorCoefsVsAveragedBehavioralMeasuresToDistributeForClusters(
         sortvar=sortvar, 
         modality=modality, 
         clustersIDs=clustersIDs, 
         conditions=conditions, 
         modelSignificance=modelSignificance,
         annotationPattern=annotationPattern, 
         scFilenamePattern=scFilenamePattern,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         behavioralMeasuresFilenamePattern=behavioralMeasuresFilenamePattern,
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

plotPredictionsCorCoefsVsAveragedBehavioralMeasuresToDistributeForClusters <-
 function(sortvar, modality, clustersIDs, conditions, 
                   modelSignificance,
                   annotationPattern, 
                   scFilenamePattern, 
                   analyzedConditionsFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   behavioralMeasuresFilenamePattern,
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
        plotPredictionsCorCoefsVsAveragedBehavioralMeasuresToDistributeForConditions(
         sortvar=sortvar, 
         modality=modality, 
         clusterID=clusterID, 
         conditions=conditions, 
         modelSignificance=modelSignificance,
         annotationPattern=annotationPattern, 
         subjectsAndComponents=subjectsAndComponents,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern, 
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         behavioralMeasuresFilenamePattern=behavioralMeasuresFilenamePattern,
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

plotPredictionsCorCoefsVsAveragedBehavioralMeasuresToDistributeForConditions <-
 function(sortvar, modality, clusterID, conditions, 
                   modelSignificance,
                   annotationPattern, 
                   subjectsAndComponents, 
                   analyzedConditionsFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   behavioralMeasuresFilenamePattern,
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
        plotPredictionsCorCoefsVsAveragedBehavioralMeasuresToDistributeForSubjects(
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         modelSignificance=modelSignificance,
         annotationPattern=annotationPattern,
         subjectsAndComponents=subjectsAndComponents,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern, 
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         behavioralMeasuresFilenamePattern=behavioralMeasuresFilenamePattern,
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

plotPredictionsCorCoefsVsAveragedBehavioralMeasuresToDistributeForSubjects <-
 function(sortvar, modality, clusterID, condition, 
                   modelSignificance,
                   annotationPattern,
                   subjectsAndComponents, 
                   analyzedConditionsFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   behavioralMeasuresFilenamePattern,
                   xlab,
                   ylab="Correlation Coefficient",
                   main="", 
                   xCorCoefAnnotation=0.05, 
                   yCorCoefAnnotation=0.3, 
                   hjust, vjust,
                   sizeLabels=2.5, 
                   sizeAnnotations=4,
                   nResamples=2000,
                   ...) {
    behavioralMeasuresFilename <- sprintf(behavioralMeasuresFilenamePattern, 
                                           condition)
    behavioralMeasures <- read.table(behavioralMeasuresFilename, 
                                      col.names=c("subjectName", 
                                                  "errorRate", 
                                                  "errorRate95CIL", 
                                                  "errorRate95CIU"))
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
        behavioralMeasureCIIndex <- which(behavioralMeasures$subjectName==
                                           subjectName)
        behavioralMeasureCI <- c(behavioralMeasures[behavioralMeasureCIIndex, 
                                                     "errorRate"], 
                                  behavioralMeasures[behavioralMeasureCIIndex,
                                                      "errorRate95CIL"], 
                                  behavioralMeasures[behavioralMeasureCIIndex,
                                                      "errorRate95CIU"])
        behavioralMeasuresCIs <- rbind(behavioralMeasuresCIs, 
                                        behavioralMeasureCI)
    }
    if(length(scNames)>1 && nValidPredSFPDurCorsCIs>1) {
        permRes <- permuteSkippedSpearmanCorCoef(x=predSFPDurCorsCIs[,1],
                                                  y=behavioralMeasuresCIs[,1],
                                                  nResamples=nResamples)
        r <- permRes$t0
        pValue <- sum(permRes$t<permRes$t0)/length(permRes$t)
        statsCorCoefAnnotation <- sprintf(annotationPattern, r, pValue)
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
}
