
plotPredictionsCorCoefsOriginalVsControl4ForModalities <- 
 function(sortvar, modalities, clustersIDs, conditions, 
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   bootPairedDifferencesFunction, 
                   scFilenamePattern, 
                   oMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                   cMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   controlCorCoefsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(modality in modalities) {
        modalityPredictionsCorCoefs <- plotPredictionsCorCoefsOriginalVsControl4ForClusters(
                        sortvar=sortvar,
                        modality=modality,
                        clustersIDs=clustersIDs,
                        conditions=conditions,
                        significance=significance,
                        nResamples=nResamples,
                        ciConf=ciConf,
                        annotationPattern=annotationPattern,
                        bootPairedDifferencesFunction=
                         bootPairedDifferencesFunction,
                        scFilenamePattern=scFilenamePattern,
                        oMinAndMaxSFPDOfBestPredictionsFilenamePattern=
                         oMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                        cMinAndMaxSFPDOfBestPredictionsFilenamePattern=
                         cMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                        analyzedDataFilenamePattern=
                         analyzedDataFilenamePattern,
                        controlCorCoefsFilenamePattern=
                         controlCorCoefsFilenamePattern,
                        plotFilenamePattern=plotFilenamePattern,
                        ...)
    }
}
plotPredictionsCorCoefsOriginalVsControl4ForClusters <- 
 function(sortvar, modality, clustersIDs, conditions,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   bootPairedDifferencesFunction, 
                   scFilenamePattern, 
                   oMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                   cMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   controlCorCoefsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(clusterID in clustersIDs) {
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID, scFilename)
        clusterCorCoefs <- plotPredictionsCorCoefsOriginalVsControl4ForConditions(
                            sortvar=sortvar,
                            modality=modality,
                            clusterID=clusterID,
                            conditions=conditions,
                            significance=significance,
                            nResamples=nResamples,
                            ciConf=ciConf,
                            annotationPattern=annotationPattern,
                            subjectsAndComponents=subjectsAndComponents,
                            bootPairedDifferencesFunction=
                             bootPairedDifferencesFunction,
                            oMinAndMaxSFPDOfBestPredictionsFilenamePattern=
                             oMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                            cMinAndMaxSFPDOfBestPredictionsFilenamePattern=
                             cMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                            analyzedDataFilenamePattern=
                             analyzedDataFilenamePattern,
                            controlCorCoefsFilenamePattern=
                             controlCorCoefsFilenamePattern,
                            plotFilenamePattern=plotFilenamePattern,
                            ...)
    }
}
plotPredictionsCorCoefsOriginalVsControl4ForConditions <- 
 function(sortvar, modality, clusterID, conditions,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   subjectsAndComponents,
                   bootPairedDifferencesFunction, 
                   oMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                   cMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern,
                   controlCorCoefsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(condition in conditions) {
        conditionPredictionsCorCoefs <- 
         plotPredictionsCorCoefsOriginalVsControl4ForSubjects(
          sortvar=sortvar,
          modality=modality,
          clusterID=clusterID,
          condition=condition,
          significance=significance,
          nResamples=nResamples,
          ciConf=ciConf,
          annotationPattern=annotationPattern,
          subjectsAndComponents=subjectsAndComponents,
          bootPairedDifferencesFunction=bootPairedDifferencesFunction,
          oMinAndMaxSFPDOfBestPredictionsFilenamePattern=
           oMinAndMaxSFPDOfBestPredictionsFilenamePattern,
          cMinAndMaxSFPDOfBestPredictionsFilenamePattern=
           cMinAndMaxSFPDOfBestPredictionsFilenamePattern,
          analyzedDataFilenamePattern=analyzedDataFilenamePattern,
          controlCorCoefsFilenamePattern=controlCorCoefsFilenamePattern,
          plotFilenamePattern=plotFilenamePattern,
          ...)
    }
}
plotPredictionsCorCoefsOriginalVsControl4ForSubjects <- 
 function(sortvar, modality, clusterID, condition, 
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   subjectsAndComponents,
                   bootPairedDifferencesFunction, 
                   oMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                   cMinAndMaxSFPDOfBestPredictionsFilenamePattern,
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
        oMinAndMaxSFPDOfBestPredictionsFilename <- 
         sprintf(oMinAndMaxSFPDOfBestPredictionsFilenamePattern, clusterID, clusterID, condition, sortvar, 
                                            modality, subjectName, component)
        res <- readLines(oMinAndMaxSFPDOfBestPredictionsFilename)
        oMinSFPD <- as.integer(res[1])
        oMaxSFPD <- as.integer(res[2])
        cMinAndMaxSFPDOfBestPredictionsFilename <- 
         sprintf(cMinAndMaxSFPDOfBestPredictionsFilenamePattern, clusterID, clusterID, condition, sortvar, 
                                            modality, subjectName, component)
        res <- readLines(cMinAndMaxSFPDOfBestPredictionsFilename)
        cMinSFPD <- as.integer(res[1])
        cMaxSFPD <- as.integer(res[2])
        analyzedDataFilename <- sprintf(analyzedDataFilenamePattern,
                                         clusterID,
                                         clusterID,
                                         condition,
                                         sortvar,
                                         modality,
                                         subjectName,
                                         component,
                                         oMinSFPD,
                                         oMaxSFPD)
        controlCorCoefsFilename <- sprintf(controlCorCoefsFilenamePattern,
                                            clusterID,
                                            clusterID,
                                            condition,
                                            sortvar,
                                            modality,
                                            subjectName,
                                            component,
                                            cMinSFPD,
                                            cMaxSFPD)
        sAnalyzedData <- get(load(analyzedDataFilename))
        sControlCorCoefs <- get(load(controlCorCoefsFilename))

        scNames <- c(scNames, sprintf("%sC%02d", subjectName, component))
        if(!is.null(sAnalyzedData$predSFPDurCorCI)) {
            originalCI <- sAnalyzedData$predSFPDurCorCI
#             if(originalCI[1]<0) {
#                 originalCI[2] <- originalCI[2]-originalCI[1]
#                 originalCI[3] <- originalCI[3]-originalCI[1]
#                 originalCI[1] <- 0
#             }
        } else {
            originalCI <- c(0, 0, 0)
        }
# if(subjectName=="av1042b") {
#     browser()
# }
        originalCIs <- rbind(originalCIs, originalCI)
        if(length(sControlCorCoefs)>0 && 
            length(which(!is.na(sControlCorCoefs)))>0) {
            bootRes <- bootstrapMedian(sControlCorCoefs, nResamples=nResamples)
            controlCI <- getBootstrapCIs(bootRes, conf=ciConf)
#             if(controlCI[1]<0) {
#                 controlCI[2] <- controlCI[2]-controlCI[1]
#                 controlCI[3] <- controlCI[3]-controlCI[1]
#                 controlCI[1] <- 0
#             }
        } else {
            controlCI <- c(0, 0, 0)
        }
        controlCIs <- rbind(controlCIs, controlCI)
    }
# browser()
    plotPairsCIs(labels=scNames, cis1=controlCIs, cis2=originalCIs, 
                                    nResamples=nResamples, ciConf=ciConf,
                                    annotationPattern=annotationPattern, 
                                    bootPairedDifferencesFunction=
                                     bootPairedDifferencesFunction,
                                    ...)
    plotFilename <- sprintf(plotFilenamePattern, clusterID, clusterID, 
                                                 condition, sortvar, modality)
    ggsave(filename=plotFilename)
}

