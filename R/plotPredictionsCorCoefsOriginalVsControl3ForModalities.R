
plotPredictionsCorCoefsOriginalVsControl3ForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, minSFPD, maxSFPD,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   scFilenamePattern, 
                   analyzedDataFilenamePattern,
                   controlCorCoefsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(modality in modalities) {
        modalityPredictionsCorCoefs <- plotPredictionsCorCoefsOriginalVsControl3ForClusters(
                        sortvar=sortvar,
                        modality=modality,
                        clustersIDs=clustersIDs,
                        conditions=conditions,
                        minSFPD=minSFPD,
                        maxSFPD=maxSFPD,
                        significance=significance,
                        nResamples=nResamples,
                        ciConf=ciConf,
                        annotationPattern=annotationPattern,
                        scFilenamePattern=scFilenamePattern,
                        analyzedDataFilenamePattern=
                         analyzedDataFilenamePattern,
                        controlCorCoefsFilenamePattern=
                         controlCorCoefsFilenamePattern,
                        plotFilenamePattern=plotFilenamePattern,
                        ...)
    }
}
plotPredictionsCorCoefsOriginalVsControl3ForClusters <- 
 function(sortvar, modality, clustersIDs, conditions, minSFPD, maxSFPD,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   scFilenamePattern, 
                   analyzedDataFilenamePattern,
                   controlCorCoefsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(clusterID in clustersIDs) {
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID, scFilename)
        clusterCorCoefs <- plotPredictionsCorCoefsOriginalVsControl3ForConditions(
                            sortvar=sortvar,
                            modality=modality,
                            clusterID=clusterID,
                            conditions=conditions,
                            minSFPD=minSFPD,
                            maxSFPD=maxSFPD,
                            significance=significance,
                            nResamples=nResamples,
                            ciConf=ciConf,
                            annotationPattern=annotationPattern,
                            subjectsAndComponents=subjectsAndComponents,
                            analyzedDataFilenamePattern=
                             analyzedDataFilenamePattern,
                            controlCorCoefsFilenamePattern=
                             controlCorCoefsFilenamePattern,
                            plotFilenamePattern=plotFilenamePattern,
                            ...)
    }
}
plotPredictionsCorCoefsOriginalVsControl3ForConditions <- 
 function(sortvar, modality, clusterID, conditions, minSFPD, maxSFPD,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   subjectsAndComponents=subjectsAndComponents,
                   analyzedDataFilenamePattern,
                   controlCorCoefsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(condition in conditions) {
        conditionPredictionsCorCoefs <- plotPredictionsCorCoefsOriginalVsControl3ForSubjects(
                                  sortvar=sortvar,
                                  modality=modality,
                                  clusterID=clusterID,
                                  condition=condition,
                                  minSFPD=minSFPD,
                                  maxSFPD=maxSFPD,
                                  significance=significance,
                                  nResamples=nResamples,
                                  ciConf=ciConf,
                                  annotationPattern=annotationPattern,
                                  subjectsAndComponents=subjectsAndComponents,
                                  analyzedDataFilenamePattern=
                                   analyzedDataFilenamePattern,
                                  controlCorCoefsFilenamePattern=
                                   controlCorCoefsFilenamePattern,
                                  plotFilenamePattern=plotFilenamePattern,
                                  ...)
    }
}
plotPredictionsCorCoefsOriginalVsControl3ForSubjects <- 
 function(sortvar, modality, clusterID, condition, minSFPD, maxSFPD,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   subjectsAndComponents=subjectsAndComponents,
                   analyzedDataFilenamePattern,
                   controlCorCoefsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    scNames <- c()
    originalCIs <- c()
    controlCIs <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
        analyzedDataFilename <- sprintf(analyzedDataFilenamePattern,
                                         clusterID,
                                         condition,
                                         sortvar,
                                         modality,
                                         subjectName,
                                         component,
                                         minSFPD,
                                         maxSFPD)
        controlCorCoefsFilename <- sprintf(controlCorCoefsFilenamePattern,
                                            clusterID,
                                            condition,
                                            sortvar,
                                            modality,
                                            subjectName,
                                            component,
                                            minSFPD,
                                            maxSFPD)
        sAnalyzedData <- get(load(analyzedDataFilename))
        sControlCorCoefs <- get(load(controlCorCoefsFilename))

        scNames <- c(scNames, sprintf("%sC%02d", subjectName, component))
        originalCI <- sAnalyzedData$predSFPDurCorCI
        if(originalCI[1]<0) {
            originalCI[2] <- originalCI[2]-originalCI[1]
            originalCI[3] <- originalCI[3]-originalCI[1]
            originalCI[1] <- 0
        }
        originalCIs <- rbind(originalCIs, originalCI)
        bootRes <- bootstrapMedian(sControlCorCoefs, nResamples=nResamples)
        controlCI <- getBootstrapCIs(bootRes, conf=ciConf)
        if(controlCI[1]<0) {
            controlCI[2] <- controlCI[2]-controlCI[1]
            controlCI[3] <- controlCI[3]-controlCI[1]
            controlCI[1] <- 0
        }
        controlCIs <- rbind(controlCIs, controlCI)
    }
# browser()
    plotPairsCIs(labels=scNames, cis1=controlCIs, cis2=originalCIs,
                                    nResamples=nResamples, ciConf=ciConf,
                                    annotationPattern=annotationPattern, ...)
    plotFilename <- sprintf(plotFilenamePattern, clusterID, condition, sortvar, 
                                                 modality, minSFPD, maxSFPD)
    ggsave(filename=plotFilename)
}

