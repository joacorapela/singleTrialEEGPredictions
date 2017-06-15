
plotPredictionsCorCoefsAttendedVsUnattendedForModalities <- 
 function(sortvar, modalities, clustersIDs, attCondition, unattCondition,
                   minSFPD, maxSFPD,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   scFilenamePattern, 
                   analyzedDataFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(modality in modalities) {
        modalityPredictionsCorCoefs <- plotPredictionsCorCoefsAttendedVsUnattendedForClusters(
                        sortvar=sortvar,
                        modality=modality,
                        clustersIDs=clustersIDs,
                        attCondition=attCondition,
                        unattCondition=unattCondition,
                        minSFPD=minSFPD,
                        maxSFPD=maxSFPD,
                        significance=significance,
                        nResamples=nResamples,
                        ciConf=ciConf,
                        annotationPattern=annotationPattern,
                        scFilenamePattern=scFilenamePattern,
                        analyzedDataFilenamePattern=
                         analyzedDataFilenamePattern,
                        plotFilenamePattern=plotFilenamePattern,
                        ...)
    }
}
plotPredictionsCorCoefsAttendedVsUnattendedForClusters <- 
 function(sortvar, modality, clustersIDs, attCondition, unattCondition, 
                   minSFPD, maxSFPD,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   scFilenamePattern, 
                   analyzedDataFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(clusterID in clustersIDs) {
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID, scFilename)
        clusterCorCoefs <- 
         plotPredictionsCorCoefsAttendedVsUnattendedForSubjectsAndComponents(
          sortvar=sortvar,
          modality=modality,
          clusterID=clusterID,
          attCondition=attCondition,
          unattCondition=unattCondition,
          minSFPD=minSFPD,
          maxSFPD=maxSFPD,
          significance=significance,
          nResamples=nResamples,
          ciConf=ciConf,
          annotationPattern=annotationPattern,
          subjectsAndComponents=subjectsAndComponents,
          analyzedDataFilenamePattern=
           analyzedDataFilenamePattern,
          plotFilenamePattern=plotFilenamePattern,
          ...)
    }
}
plotPredictionsCorCoefsAttendedVsUnattendedForSubjectsAndComponents <- 
 function(sortvar, modality, clusterID, attCondition, unattCondition, 
                   minSFPD, maxSFPD,
                   significance,
                   nResamples, ciConf, annotationPattern, 
                   subjectsAndComponents=subjectsAndComponents,
                   analyzedDataFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    scNames <- c()
    attCIs <- c()
    unattCIs <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
        analyzedDataFilename <- sprintf(analyzedDataFilenamePattern,
                                            clusterID,
                                            attCondition,
                                            sortvar,
                                            modality,
                                            subjectName,
                                            component,
                                            minSFPD,
                                            maxSFPD)
        unattAnalyzedDataFilename <- sprintf(analyzedDataFilenamePattern,
                                              clusterID,
                                              unattCondition,
                                              sortvar,
                                              modality,
                                              subjectName,
                                              component,
                                              minSFPD,
                                              maxSFPD)
        attAnalyzedData <- get(load(analyzedDataFilename))
        unattAnalyzedData <- get(load(unattAnalyzedDataFilename))

        scNames <- c(scNames, sprintf("%sC%02d", subjectName, component))
        attCI <- attAnalyzedData$predSFPDurCorCI
        if(attCI[1]<0) {
            attCI[2] <- attCI[2]-attCI[1]
            attCI[3] <- attCI[3]-attCI[1]
            attCI[1] <- 0
        }
        attCIs <- rbind(attCIs, attCI)
        unattCI <- unattAnalyzedData$predSFPDurCorCI
        if(unattCI[1]<0) {
            unattCI[2] <- unattCI[2]-unattCI[1]
            unattCI[3] <- unattCI[3]-unattCI[1]
            unattCI[1] <- 0
        }
        unattCIs <- rbind(unattCIs, unattCI)
    }
# browser()
    plotPairsCIs(labels=scNames, cis1=unattCIs, cis2=attCIs, 
                                    nResamples=nResamples, ciConf=ciConf,
                                    annotationPattern=annotationPattern,
                                    ...)
    plotFilename <- sprintf(plotFilenamePattern, clusterID, attCondition, 
                                                 unattCondition, sortvar, 
                                                 modality, minSFPD, maxSFPD)
    ggsave(filename=plotFilename)
}

