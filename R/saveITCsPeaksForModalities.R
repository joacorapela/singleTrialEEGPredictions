
saveITCsPeaksForModalities <- function(sortvar, 
                                        modalities, 
                                        clustersIDs, 
                                        conditions,
                                        noctave, nvoice, nCycles,
                                        itcsFilenamePattern, 
                                        itcsPeaksFilenamePattern) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        saveITCsPeaksForClusters(sortvar=sortvar,
                                  modality=modality,
                                  clustersIDs=clustersIDs,
                                  conditions=conditions,
                                  noctave=noctave, nvoice=nvoice, nCycles=nCycles,
                                  itcsFilenamePattern=
                                   itcsFilenamePattern, 
                                  itcsPeaksFilenamePattern=
                                   itcsPeaksFilenamePattern)
    }
}

saveITCsPeaksForClusters <- function(sortvar, 
                                      modality, 
                                      clustersIDs, 
                                      conditions,
                                      noctave, nvoice, nCycles,
                                      itcsFilenamePattern, 
                                      itcsPeaksFilenamePattern) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %d", clusterID))
        saveITCsPeaksForConditions(sortvar=sortvar,
                                    modality=modality,
                                    clusterID=clusterID,
                                    conditions=conditions,
                                    noctave=noctave, nvoice=nvoice, nCycles=nCycles,
                                    itcsFilenamePattern=
                                     itcsFilenamePattern, 
                                    itcsPeaksFilenamePattern=
                                     itcsPeaksFilenamePattern)
    }
}

saveITCsPeaksForConditions <- function(sortvar, 
                                        modality, 
                                        clusterID, 
                                        conditions,
                                        noctave, nvoice, nCycles,
                                        itcsFilenamePattern, 
                                        itcsPeaksFilenamePattern) {
    for(condition in conditions) {
        show(sprintf("Processing %s", condition))
        saveITCsPeaksForCondition(sortvar=sortvar,
                                   modality=modality,
                                   clusterID=clusterID,
                                   condition=condition,
                                   noctave=noctave, nvoice=nvoice, nCycles=nCycles,
                                   itcsFilenamePattern=
                                    itcsFilenamePattern, 
                                   itcsPeaksFilenamePattern=
                                    itcsPeaksFilenamePattern)
    }
}

saveITCsPeaksForCondition <- function(sortvar, 
                                       modality, 
                                       clusterID, 
                                       condition, 
                                       noctave, nvoice, nCycles,
                                       itcsFilenamePattern, 
                                       itcsPeaksFilenamePattern) {
    itcsFilename <- sprintf(itcsFilenamePattern, 
                                     clusterID, 
                                     condition,
                                     sortvar,
                                     modality,
                                     noctave,
                                     nvoice,
                                     nCycles)
    itcsPeaksFilename <- sprintf(itcsPeaksFilenamePattern, 
                                  clusterID, 
                                  condition,
                                  sortvar,
                                  modality,
                                  noctave,
                                  nvoice,
                                  nCycles)
    itcsRes <- get(load(itcsFilename))
    subjectsNames <- c()
    components <- c()
    peaksValues <- c()
    peaksTimes <- c()
    peaksFreqs <- c()
    peaksSignificant <- c()
    for(i in 1:length(itcsRes)) {
        subjectsNames <- c(subjectsNames, itcsRes[[i]]$subjectName)
        components <- c(components, itcsRes[[i]]$component)
        peaksValues <- c(peaksValues,  itcsRes[[i]]$peakInfo$value)
        peaksTimes <- c(peaksTimes,  itcsRes[[i]]$peakInfo$time)
        peaksFreqs <- c(peaksFreqs,  itcsRes[[i]]$peakInfo$freq)
        peaksSignificant <- c(peaksSignificant, 
                               itcsRes[[i]]$significanceMask[itcsRes[[i]]$peakInfo$timeIndex, itcsRes[[i]]$peakInfo$freqIndex])
    }
    writePeaksInfo(filename=itcsPeaksFilename, 
                    subjectsNames=subjectsNames, components=components,
                    peaksValues=peaksValues, peaksTimes=peaksTimes, 
                    peaksFreqs=peaksFreqs,
                    peaksSignificant=peaksSignificant)
}
