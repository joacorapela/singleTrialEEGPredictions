
computeITCsForModalities <- function(modalities, 
                                      sortvar,
                                      clustersIDs,
                                      conditions,
                                      noctave, nvoice, nCycles, 
                                      minTime, 
                                      maxTime, 
                                      significance,
                                      conf,
                                      peakFromTime, peakToTime, 
                                      peakFromFreq, peakToFreq,
                                      nResamples,
                                      erpimageFilenamePattern, 
                                      itcsFilenamePattern,
                                      scFilenamePattern) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        computeITCsForClusters(modality=modality, 
                                sortvar=sortvar,
                                clustersIDs=clustersIDs,
                                conditions=conditions,
                                noctave=noctave, 
                                nvoice=nvoice, 
                                nCycles=nCycles, 
                                minTime=minTime, 
                                maxTime=maxTime,
                                significance=significance,
                                conf=conf,
                                peakFromTime=peakFromTime, 
                                peakToTime=peakToTime,
                                peakFromFreq=peakFromFreq, 
                                peakToFreq=peakToFreq,
                                nResamples=nResamples, 
                                erpimageFilenamePattern=
                                 erpimageFilenamePattern,
                                itcsFilenamePattern=itcsFilenamePattern,
                                scFilenamePattern=scFilenamePattern)
    }
}

computeITCsForClusters <- function(modality,
                                    sortvar,
                                    clustersIDs, 
                                    conditions,
                                    noctave, nvoice, nCycles, 
                                    minTime, 
                                    maxTime, 
                                    significance,
                                    conf,
                                    peakFromTime, peakToTime, 
                                    peakFromFreq, peakToFreq,
                                    nResamples,
                                    erpimageFilenamePattern, 
                                    itcsFilenamePattern,
                                    scFilenamePattern) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %2d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID)
        subjectsAndComponentsInCluster <- 
         getSubjectsAndComponentsInCluster(clusterID, scFilename)
        computeITCsForConditions(modality=modality, 
                                  sortvar=sortvar, 
                                  clusterID=clusterID, 
                                  conditions=conditions,
                                  subjectsAndComponentsInCluster=
                                   subjectsAndComponentsInCluster,
                                  noctave=noctave, 
                                  nvoice=nvoice, 
                                  nCycles=nCycles, 
                                  minTime=minTime, 
                                  maxTime=maxTime,
                                  significance=significance,
                                  conf=conf,
                                  peakFromTime=peakFromTime, 
                                  peakToTime=peakToTime,
                                  peakFromFreq=peakFromFreq, 
                                  peakToFreq=peakToFreq,
                                  nResamples=nResamples, 
                                  erpimageFilenamePattern=
                                   erpimageFilenamePattern,
                                  itcsFilenamePattern=itcsFilenamePattern)
    }
}

computeITCsForConditions <- function(modality, 
                                      sortvar, 
                                      clusterID,
                                      conditions, 
                                      subjectsAndComponentsInCluster,
                                      noctave, nvoice, nCycles, 
                                      minTime, 
                                      maxTime, 
                                      significance,
                                      conf,
                                      peakFromTime, peakToTime, 
                                      peakFromFreq, peakToFreq,
                                      nResamples,
                                      erpimageFilenamePattern, 
                                      itcsFilenamePattern) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        computeITCForCondition(modality=modality, 
                                sortvar=sortvar,
                                clusterID=clusterID,
                                condition=condition,
                                subjectsAndComponentsInCluster=
                                 subjectsAndComponentsInCluster,
                                noctave=noctave, 
                                nvoice=nvoice, 
                                nCycles=nCycles, 
                                minTime=minTime, maxTime=maxTime,
                                significance=significance,
                                conf=conf,
                                peakFromTime=peakFromTime, 
                                peakToTime=peakToTime,
                                peakFromFreq=peakFromFreq, 
                                peakToFreq=peakToFreq,
                                nResamples=nResamples, 
                                erpimageFilenamePattern=erpimageFilenamePattern,
                                itcsFilenamePattern=itcsFilenamePattern)
    }
}

computeITCForCondition <- function(modality,
                                    sortvar,
                                    clusterID,
                                    condition, 
                                    subjectsAndComponentsInCluster,
                                    noctave, nvoice, nCycles, 
                                    minTime, 
                                    maxTime, 
                                    significance,
                                    conf,
                                    peakFromTime, peakToTime, 
                                    peakFromFreq, peakToFreq,
                                    nResamples,
                                    erpimageFilenamePattern,
                                    itcsFilenamePattern) {
    condERPImageFilenamePattern <- sprintf(erpimageFilenamePattern, clusterID,
                                                                    condition,
                                                                    sortvar,
                                                                    modality)
    itcsFilename <- sprintf(itcsFilenamePattern, clusterID, condition, sortvar, modality, noctave, nvoice, nCycles)
    resLoadData <- loadERPImageData(filenamesPattern=
                                     condERPImageFilenamePattern)
    trials <- resLoadData$data
    times <- resLoadData$times
    srate <- resLoadData$metaData$srate
    subjectsNames <- resLoadData$subjectsNames
    components <- resLoadData$components

    itcsRes <- list()
    for(subjectName in unique(subjectsNames)) {
        subjectComponents <- 
         getSubjectComponents(subjectName=subjectName, 
                               subjectsAndComponents=
                                subjectsAndComponentsInCluster)
        for(component in subjectComponents) {
            show(sprintf("Processing subject %s, component %02d",
                         subjectName, component))
            scCases <- getSubjectAndComponentCases(
                        subjectName=subjectName, 
                        component=component,
                        subjectsNames=subjectsNames,
                        components=components)
            if(length(scCases)==0) {
                stop(sprintf("subject %s and component %02d not found", 
                             subjectName, component))
            }
            scTrials <- trials[, scCases]
            selectedTimesIndices = which(minTime<=times & times<=maxTime)
            selectedTimes <- times[selectedTimesIndices]
            scTrialsWithSelectedTimes <- scTrials[selectedTimesIndices,]
            itcRes <- computeITCWithSignificanceMaskAndPeakCI(
                       times=selectedTimes,
                       trials=scTrialsWithSelectedTimes,
                       noctave=noctave, 
                       nvoice=nvoice, 
                       w0=pi/3*nCycles, 
                       srate=srate, 
                       peakFromTime=peakFromTime,
                       peakToTime=peakToTime,
                       peakFromFreq=peakFromFreq,
                       peakToFreq=peakToFreq,
                       nResamples=nResamples,
                       significance=significance,
                       conf=conf)
               itcsRes <- c(itcsRes, list(c(subjectName=subjectName, 
                                             component=component,
                                             itcRes)))
        }
    }
    save(itcsRes, file=itcsFilename)
}

