saveAmpPhaseERPImagesForModalities <- function(sortvar, 
                                                modalities, 
                                                clustersIDs, 
                                                conditions, 
                                                noctave, nvoice, nCycles, 
                                                minTime, 
                                                maxTime, 
                                                shuffleSFPDs,
                                                sfpdsInfo,
                                                itcsPeaksFilenamePattern, 
                                                erpimageFilenamePattern,
                                                apERPImagesFilenamePattern,
                                                scFilenamePattern) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        saveAmpPhaseERPImagesForClusters(sortvar=sortvar,
                                          modality=modality,
                                          clustersIDs=clustersIDs,
                                          conditions=conditions,
                                          noctave=noctave,
                                          nvoice=nvoice,
                                          nCycles=nCycles,
                                          minTime=minTime, 
                                          maxTime=maxTime, 
                                          shuffleSFPDs=shuffleSFPDs,
                                          sfpdsInfo=sfpdsInfo,
                                          itcsPeaksFilenamePattern=
                                           itcsPeaksFilenamePattern,
                                          erpimageFilenamePattern=
                                           erpimageFilenamePattern,
                                          apERPImagesFilenamePattern=
                                           apERPImagesFilenamePattern,
                                          scFilenamePattern=scFilenamePattern)
    }
}
saveAmpPhaseERPImagesForClusters <- function(sortvar, 
                                              modality, 
                                              clustersIDs, 
                                              conditions, 
                                              noctave, nvoice, nCycles, 
                                              minTime, 
                                              maxTime, 
                                              shuffleSFPDs,
                                              sfpdsInfo,
                                              itcsPeaksFilenamePattern, 
                                              erpimageFilenamePattern,
                                              apERPImagesFilenamePattern,
                                              scFilenamePattern) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %02d", clusterID))
        scFilename <- sprintf(scFilenamePattern, clusterID)
        subjectsAndComponents <-
         getSubjectsAndComponentsInCluster(clusterID, scFilename)
        saveAmpPhaseERPImagesForConditions(sortvar=sortvar, 
                                            modality=modality,
                                            clusterID=clusterID,
                                            conditions=conditions,
                                            subjectsAndComponents=
                                             subjectsAndComponents,
                                            noctave=noctave,
                                            nvoice=nvoice,
                                            nCycles=nCycles,
                                            minTime=minTime, 
                                            maxTime=maxTime, 
                                            shuffleSFPDs=shuffleSFPDs,
                                            sfpdsInfo=sfpdsInfo,
                                            itcsPeaksFilenamePattern=
                                             itcsPeaksFilenamePattern,
                                            erpimageFilenamePattern=
                                             erpimageFilenamePattern,
                                            apERPImagesFilenamePattern=
                                             apERPImagesFilenamePattern,
                                            scFilenamePattern=scFilenamePattern)
    }
}
saveAmpPhaseERPImagesForConditions <- function(sortvar, 
                                                modality, 
                                                clusterID, 
                                                conditions, 
                                                subjectsAndComponents,
                                                noctave, nvoice, nCycles, 
                                                minTime, 
                                                maxTime, 
                                                shuffleSFPDs,
                                                sfpdsInfo,
                                                itcsPeaksFilenamePattern, 
                                                erpimageFilenamePattern,
                                                apERPImagesFilenamePattern,
                                                scFilenamePattern) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        apERPImages <- 
         saveAmpPhaseERPImagesForCondition(sortvar=sortvar, 
                                            modality=modality,
                                            clusterID=clusterID,
                                            condition=condition,
                                            subjectsAndComponents=
                                             subjectsAndComponents,
                                            noctave=noctave,
                                            nvoice=nvoice,
                                            nCycles=nCycles,
                                            minTime=minTime, 
                                            maxTime=maxTime, 
                                            shuffleSFPDs=shuffleSFPDs,
                                            sfpdsInfo=sfpdsInfo,
                                            itcsPeaksFilenamePattern=
                                             itcsPeaksFilenamePattern,
                                            erpimageFilenamePattern=
                                             erpimageFilenamePattern,
                                            apERPImagesFilenamePattern=
                                             apERPImagesFilenamePattern)
    }
}
saveAmpPhaseERPImagesForCondition <- function(sortvar, 
                                                modality, 
                                                clusterID, 
                                                condition, 
                                                subjectsAndComponents,
                                                noctave, nvoice, nCycles, 
                                                minTime, 
                                                maxTime, 
                                                shuffleSFPDs,
                                                sfpdsInfo,
                                                itcsPeaksFilenamePattern, 
                                                erpimageFilenamePattern,
                                                apERPImagesFilenamePattern) {
    peaksInfoFilename <- sprintf(itcsPeaksFilenamePattern, clusterID, condition,
                                                           sortvar, modality, 
                                                           noctave, nvoice, 
                                                           nCycles)
    condERPImageFilenamePattern <- sprintf(erpimageFilenamePattern, clusterID, 
                                                                      condition,
                                                                      sortvar, 
                                                                      modality)
    
    resLoadData <- loadERPImageData(filenamesPattern=
                                     condERPImageFilenamePattern)
    subjectsNames <- resLoadData$subjectsNames
    components <- resLoadData$components
    epochEventIDs <- resLoadData$epochEventIDs
    trials <- resLoadData$data
    times <- resLoadData$times
    srate <- resLoadData$metaData$srate
    if(is.na(minTime)) {
        minTime <- min(resLoadData$times)
    }
    if(is.na(maxTime)) {
        maxTime <- max(resLoadData$times)
    }
    peaksInfo <- readPeaksInfo(filename=peaksInfoFilename)
    for(subjectName in unique(subjectsNames)) {
        subjectComponents <-
         getSubjectComponents(subjectName=subjectName,
                               subjectsAndComponents=subjectsAndComponents)
        for(component in subjectComponents) {
            show(sprintf("Processing subject %s and component %02d", 
                         subjectName, component))
            apERPImagesFilename <- sprintf(apERPImagesFilenamePattern,
                                            clusterID, condition, sortvar, 
                                            modality, subjectName, component)
            scPeakITCInfo <- 
             getSubjectAndComponentPeakTimeFreqInfo(subjectName=subjectName, 
                                                     component=component,
                                                     peaksInfo=peaksInfo)
            scCases <- getSubjectAndComponentCases(subjectName=subjectName, 
                                                    component=component,
                                                    subjectsNames=subjectsNames,
                                                    components=components)
            scEpochEventIDs <- epochEventIDs[scCases]
            sSFPDsInfo <- 
             getSubjectModalityAndConditionSFPDsInfo(sfpdsInfo=sfpdsInfo, 
                                              subjectName=subjectName,
                                              modality=modality,
                                              condition=condition)
            subjectSFPDs <- getSFPDsForNonTargetsUREvents(sfpds=sSFPDsInfo, 
                                                           nonTargetsUREvents=
                                                            scEpochEventIDs)
            scTrials <- trials[, scCases]
            outliersIDs <- outbox(subjectSFPDs, mbox=TRUE)$out.id
            outliersEpochEventIDs <- scEpochEventIDs[outliersIDs]
            if(length(outliersIDs[1])>0) {
                scTrials <- scTrials[, -outliersIDs]
                scEpochEventIDs <- scEpochEventIDs[-outliersIDs]
                subjectSFPDs <- subjectSFPDs[-outliersIDs]
            }
            saveAmpPhaseERPImagesForTrials(
             subjectName=subjectName,
             component=component,
             peakITCFreq=scPeakITCInfo$freq,
             nCycles=nCycles,
             trials=scTrials,
             epochEventIDs=scEpochEventIDs,
             sfpds=subjectSFPDs,
             sfpdsOutliers=outliersEpochEventIDs,
             times=times,
             srate=srate,
             minTime=minTime, 
             maxTime=maxTime,
             shuffleSFPDs=shuffleSFPDs,
             filename=apERPImagesFilename)
        }
    }
}
saveAmpPhaseERPImagesForTrials <- function(subjectName, 
                                            component,
                                            peakITCFreq, 
                                            nCycles,
                                            trials, 
                                            epochEventIDs,
                                            sfpds,
                                            sfpdsOutliers,
                                            times,
                                            srate, 
                                            minTime,
                                            maxTime, 
                                            shuffleSFPDs,
                                            filename) {
    apERPImages <- prepareDataAndComputeAmplitudeAndPhaseERPImages(
                    freq=peakITCFreq,
                    nCycles=nCycles,
                    trials=trials,
                    times=times,
                    srate=srate,
                    minTime=minTime, 
                    maxTime=maxTime)
    if(shuffleSFPDs) {
        shuffleIndices <- sample(length(sfpds))
        sfpds <- sfpds[shuffleIndices]
    } else {
        shuffleIndices <- NA
    }
    apERPImages <- c(list(epochEventIDs=epochEventIDs, 
                           sfpds=sfpds, 
                           shuffleIndices=shuffleIndices,
                           peakITCFreq=peakITCFreq,
                           sfpdsOutliers=sfpdsOutliers),
                      apERPImages)
    save(apERPImages, file=filename)
}
