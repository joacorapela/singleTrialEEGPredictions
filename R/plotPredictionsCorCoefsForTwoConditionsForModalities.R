
plotPredictionsCorCoefsForTwoConditionsForModalities <- 
 function(sortvar, modalities, clustersIDs, condition1, condition2,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   bootPairedDifferencesFunction,
                   scFilenamePattern, 
                   analyzedDataFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   unAttMinAndMaxSFPDOfBestPredictionsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(modality in modalities) {
        modalityPredictionsCorCoefs <- plotPredictionsCorCoefsForTwoConditionsForClusters(
                        sortvar=sortvar,
                        modality=modality,
                        clustersIDs=clustersIDs,
                        condition1=condition1,
                        condition2=condition2,
                        minSFPD=minSFPD,
                        maxSFPD=maxSFPD,
                        significance=significance,
                        nResamples=nResamples,
                        ciConf=ciConf,
                        annotationPattern=annotationPattern,
                        bootPairedDifferencesFunction=
                         bootPairedDifferencesFunction,
                        scFilenamePattern=scFilenamePattern,
                        analyzedDataFilenamePattern=
                         analyzedDataFilenamePattern,
                        minAndMaxSFPDOfBestPredictionsFilenamePattern=
                         minAndMaxSFPDOfBestPredictionsFilenamePattern,
                        plotFilenamePattern=plotFilenamePattern,
                        ...)
    }
}
plotPredictionsCorCoefsForTwoConditionsForClusters <- 
 function(sortvar, modality, clustersIDs, condition1, condition2, 
                   minSFPD, maxSFPD,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   bootPairedDifferencesFunction,
                   scFilenamePattern, 
                   analyzedDataFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(clusterID in clustersIDs) {
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID, scFilename)
        clusterCorCoefs <- 
         plotPredictionsCorCoefsForTwoConditionsForSubjectsAndComponents(
          sortvar=sortvar,
          modality=modality,
          clusterID=clusterID,
          condition1=condition1,
          condition2=condition2,
          minSFPD=minSFPD,
          maxSFPD=maxSFPD,
          significance=significance,
          nResamples=nResamples,
          ciConf=ciConf,
          annotationPattern=annotationPattern,
          bootPairedDifferencesFunction=bootPairedDifferencesFunction,
          subjectsAndComponents=subjectsAndComponents,
          analyzedDataFilenamePattern=
           analyzedDataFilenamePattern,
          minAndMaxSFPDOfBestPredictionsFilenamePattern=
           minAndMaxSFPDOfBestPredictionsFilenamePattern,
          plotFilenamePattern=plotFilenamePattern,
          ...)
    }
}
plotPredictionsCorCoefsForTwoConditionsForSubjectsAndComponents <- 
 function(sortvar, modality, clusterID, condition1, condition2, 
                   minSFPD, maxSFPD,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   bootPairedDifferencesFunction,
                   subjectsAndComponents=subjectsAndComponents,
                   analyzedDataFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    scNames <- c()
    cis1 <- c()
    cis2 <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
        minAndMaxSFPDOfBestPredictions1Filename <- 
         sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, 
                  clusterID, condition1, sortvar, modality, subjectName, 
                  component)
        res <- readLines(minAndMaxSFPDOfBestPredictions1Filename)
        minSFPD1 <- as.integer(res[1])
        maxSFPD1 <- as.integer(res[2])
        analyzedData1Filename <- sprintf(analyzedDataFilenamePattern,
                                          clusterID,
                                          condition1,
                                          sortvar,
                                          modality,
                                          subjectName,
                                          component,
                                          minSFPD1,
                                          maxSFPD1)
        analyzedData1 <- get(load(analyzedData1Filename))
        minAndMaxSFPDOfBestPredictions2Filename <- 
         sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, 
                  clusterID, condition2, sortvar, modality, subjectName, 
                  component)
        res <- readLines(minAndMaxSFPDOfBestPredictions2Filename)
        minSFPD2 <- as.integer(res[1])
        mAxSFPD2 <- as.integer(res[2])
        analyzedData2Filename <- sprintf(analyzedDataFilenamePattern,
                                          clusterID,
                                          condition2,
                                          sortvar,
                                          modality,
                                          subjectName,
                                          component,
                                          minSFPD2,
                                          mAxSFPD2)
        analyzedData2 <- get(load(analyzedData2Filename))

        scNames <- c(scNames, sprintf("%sC%02d", subjectName, component))
        ci1 <- analyzedData1$predSFPDurCorCI
        if(!is.null(ci1)) {
            if(ci1[1]<0) {
                ci1[2] <- ci1[2]-ci1[1]
                ci1[3] <- ci1[3]-ci1[1]
                ci1[1] <- 0
            }
        } else {
            ci1 <- c(0, 0, 0)
        }
        cis1 <- rbind(cis1, ci1)
        ci2 <- analyzedData2$predSFPDurCorCI
        if(!is.null(ci2)) {
            if(ci2[1]<0) {
                ci2[2] <- ci2[2]-ci2[1]
                ci2[3] <- ci2[3]-ci2[1]
                ci2[1] <- 0
            }
        } else {
            ci2 <- c(0, 0, 0)
        }
        cis2 <- rbind(cis2, ci2)
    }
# browser()
    plotPairsCIs(labels=scNames, cis1=cis1, cis2=cis2, 
                                    nResamples=nResamples, ciConf=ciConf,
                                    annotationPattern=annotationPattern,
                                    bootPairedDifferencesFunction=
                                     bootPairedDifferencesFunction,
                                    ...)
    plotFilename <- sprintf(plotFilenamePattern, clusterID, 
                                                 condition1, condition2, 
                                                 sortvar, modality)
    ggsave(filename=plotFilename)
}

