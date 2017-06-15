
plotMaxSFPDsForTwoConditionsForModalities <- 
 function(sortvar, modalities, clustersIDs, condition1, condition2,
                   nResamples, ciConf, annotationPattern, 
                   scFilenamePattern, 
                   maxSFPDsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(modality in modalities) {
        modalityMaxSFPDs <- plotMaxSFPDsForTwoConditionsForClusters(
                        sortvar=sortvar,
                        modality=modality,
                        clustersIDs=clustersIDs,
                        condition1=condition1,
                        condition2=condition2,
                        nResamples=nResamples,
                        ciConf=ciConf,
                        annotationPattern=annotationPattern,
                        scFilenamePattern=scFilenamePattern,
                        maxSFPDsFilenamePattern=
                         maxSFPDsFilenamePattern,
                        plotFilenamePattern=plotFilenamePattern,
                        ...)
    }
}
plotMaxSFPDsForTwoConditionsForClusters <- 
 function(sortvar, modality, clustersIDs, condition1, condition2, 
                   nResamples, ciConf, annotationPattern, 
                   scFilenamePattern, 
                   maxSFPDsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    for(clusterID in clustersIDs) {
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID, scFilename)
        clusterCorCoefs <- 
         plotMaxSFPDsForTwoConditionsForSubjectsAndComponents(
          sortvar=sortvar,
          modality=modality,
          clusterID=clusterID,
          condition1=condition1,
          condition2=condition2,
          nResamples=nResamples,
          ciConf=ciConf,
          annotationPattern=annotationPattern,
          subjectsAndComponents=subjectsAndComponents,
          maxSFPDsFilenamePattern=
           maxSFPDsFilenamePattern,
          plotFilenamePattern=plotFilenamePattern,
          ...)
    }
}
plotMaxSFPDsForTwoConditionsForSubjectsAndComponents <- 
 function(sortvar, modality, clusterID, condition1, condition2, 
                   nResamples, ciConf, annotationPattern, 
                   subjectsAndComponents=subjectsAndComponents,
                   maxSFPDsFilenamePattern,
                   plotFilenamePattern,
                   ...) {
    scNames <- c()
    c1MaxSFPDs <- c()
    c2MaxSFPDs <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
        c1MaxAndMinSFPDsFilename <- 
         sprintf(maxSFPDsFilenamePattern, clusterID,
                                          condition1,
                                          sortvar,
                                          modality,
                                          subjectName,
                                          component)
        c2MaxAndMinSFPDsFilename <- 
         sprintf(maxSFPDsFilenamePattern, clusterID,
                                          condition2,
                                          sortvar,
                                          modality,
                                          subjectName,
                                          component)
        c1MinAndMaxSFPDs <- readLines(con=c1MaxAndMinSFPDsFilename, n=2)
        c2MinAndMaxSFPDs <- readLines(con=c2MaxAndMinSFPDsFilename, n=2)

        scNames <- c(scNames, sprintf("%sC%02d", subjectName, component))
        c1MaxSFPDs <- c(c1MaxSFPDs, as.integer(c1MinAndMaxSFPDs[2]))
        c2MaxSFPDs <- c(c2MaxSFPDs, as.integer(c2MinAndMaxSFPDs[2]))
    }
# browser()
    plotPairsValues(labels=scNames, values1=c1MaxSFPDs, values2=c2MaxSFPDs, 
                                    nResamples=nResamples, ciConf=ciConf,
                                    annotationPattern=annotationPattern, 
                                       ...)
    plotFilename <- sprintf(plotFilenamePattern, clusterID, condition1, 
                                                 condition2, sortvar, 
                                                 modality)
    ggsave(filename=plotFilename)
}

