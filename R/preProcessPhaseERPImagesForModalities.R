preProcessPhaseERPImagesForModalities <- 
 function(sortvar, 
           modalities, 
           clustersIDs, 
           conditions, 
           srate,
           clipFrom, clipTo,
           alignmentSignificance,
           plotSteps,
           averageTrialsWinSize,
           plotsFilenamePattern,
           width, height,
           xlim, zlim,
           scFilenamePattern,
           phaseERPImageFilenamePattern,
           preProcessedPhaseERPIFilenamePattern) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        preProcessPhaseERPImagesForClusters(
         sortvar=sortvar, 
         modality=modality,
         clustersIDs=clustersIDs,
         conditions=conditions,
         srate=srate,
         clipFrom=clipFrom,
         clipTo=clipTo,
         alignmentSignificance=alignmentSignificance,
         plotSteps=plotSteps,
         averageTrialsWinSize=averageTrialsWinSize,
         plotsFilenamePattern=plotsFilenamePattern,
         width=width, height=height,
         xlim=xlim, zlim=zlim, 
         scFilenamePattern=scFilenamePattern,
         phaseERPImageFilenamePattern=phaseERPImageFilenamePattern,
         preProcessedPhaseERPIFilenamePattern=
          preProcessedPhaseERPIFilenamePattern)
    }
}
preProcessPhaseERPImagesForClusters <- 
 function(sortvar, 
           modality, 
           clustersIDs, 
           conditions, 
           srate,
           clipFrom, clipTo,
           alignmentSignificance,
           plotSteps,
           averageTrialsWinSize,
           plotsFilenamePattern,
           width, height,
           xlim, zlim,
           scFilenamePattern,
           phaseERPImageFilenamePattern,
           preProcessedPhaseERPIFilenamePattern) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %02d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID)
        subjectsAndComponents <- getSubjectsAndComponentsInCluster(clusterID, 
                                                                    scFilename)
        preProcessPhaseERPImagesForConditions(
         sortvar=sortvar, 
         modality=modality,
         clusterID=clusterID,
         conditions=conditions,
         subjectsAndComponents=subjectsAndComponents,
         srate=srate,
         clipFrom=clipFrom,
         clipTo=clipTo,
         alignmentSignificance=alignmentSignificance,
         plotSteps=plotSteps,
         averageTrialsWinSize=averageTrialsWinSize,
         plotsFilenamePattern=plotsFilenamePattern,
         width=width, height=height,
         xlim=xlim, zlim=zlim, 
         phaseERPImageFilenamePattern=phaseERPImageFilenamePattern,
         preProcessedPhaseERPIFilenamePattern=
          preProcessedPhaseERPIFilenamePattern)
    }
}
preProcessPhaseERPImagesForConditions <- 
 function(sortvar, 
           modality, 
           clusterID, 
           conditions, 
           subjectsAndComponents,
           srate,
           clipFrom, clipTo,
           alignmentSignificance,
           plotSteps,
           averageTrialsWinSize,
           plotsFilenamePattern,
           width, height,
           xlim, zlim,
           phaseERPImageFilenamePattern,
           preProcessedPhaseERPIFilenamePattern) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        apERPImages <- 
         preProcessPhaseERPImagesForCondition(
          sortvar=sortvar, 
          modality=modality,
          clusterID=clusterID,
          condition=condition,
          subjectsAndComponents=subjectsAndComponents,
          srate=srate,
          clipFrom=clipFrom,
          clipTo=clipTo,
          alignmentSignificance=alignmentSignificance,
          plotSteps=plotSteps,
          averageTrialsWinSize=averageTrialsWinSize,
          plotsFilenamePattern=plotsFilenamePattern,
          width=width, height=height,
          xlim=xlim, zlim=zlim, 
          phaseERPImageFilenamePattern=phaseERPImageFilenamePattern,
          preProcessedPhaseERPIFilenamePattern=
           preProcessedPhaseERPIFilenamePattern)
    }
}
preProcessPhaseERPImagesForCondition <- 
 function(sortvar, 
           modality, 
           clusterID, 
           condition, 
           subjectsAndComponents,
           srate,
           clipFrom, clipTo,
           alignmentSignificance,
           plotSteps,
           averageTrialsWinSize,
           plotsFilenamePattern,
           width, height,
           xlim, zlim,
           phaseERPImageFilenamePattern,
           preProcessedPhaseERPIFilenamePattern) {
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", 
                     subjectName, component))
        if(plotSteps) {
            condPlotsFilenamePattern <- sprintf(plotsFilenamePattern, 
                                                 clusterID, 
                                                 condition, 
                                                 sortvar, 
                                                 modality, 
                                                 subjectName,
                                                 component)
        }
        phaseERPImageFilename <- sprintf(phaseERPImageFilenamePattern, 
                                          clusterID, 
                                          condition, 
                                          sortvar, 
                                          modality, 
                                          subjectName,
                                          component)
        loadPhaseERPImageRes <- get(load(phaseERPImageFilename))
        preProcessedERPI <- 
         preProcessPhaseERPI(phaseERPImage=loadPhaseERPImageRes$phaseERPImage, 
                              times=loadPhaseERPImageRes$times, 
                              sfpds=loadPhaseERPImageRes$sfpds, 
                              sfpdsOutliers=loadPhaseERPImageRes$sfpdsOutliers,
                              srate=srate, 
                              clipFrom=clipFrom, clipTo=clipTo, 
                              alignmentSignificance=alignmentSignificance,
                              plotSteps=plotSteps, 
                              averageTrialsWinSize=averageTrialsWinSize, 
                              plotsFilenamePattern=
                               condPlotsFilenamePattern, 
                              width=width, height=height, xlim=xlim, 
                              zlim=zlim)
        preProcessedERPI <- c(preProcessedERPI, 
                               list(sfpds=loadPhaseERPImageRes$sfpds,
                                     epochEventIDs=
                                      loadPhaseERPImageRes$epochEventIDs,
                                     peakITCFreq=
                                      loadPhaseERPImageRes$peakITCFreq,
                                     shuffleIndices=
                                      loadPhaseERPImageRes$shuffleIndices))
        preProcessedPhaseERPIFilename <- 
         sprintf(preProcessedPhaseERPIFilenamePattern, clusterID, 
                                                           condition, 
                                                           sortvar, 
                                                           modality, 
                                                           subjectName,
                                                           component)
        save(preProcessedERPI, file=preProcessedPhaseERPIFilename)
    }
}
