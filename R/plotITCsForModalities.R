
plotITCsForModalities <- 
 function(sortvar,
           modalities, 
           clustersIDs, 
           conditions,
           noctave, nvoice, nCycles, 
           itcFilenamePattern,
           plotsFilenamePattern,
           xlim,
           ylim,
           zlim,
           cuts,
           nFreqTicks) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotITCsForClusters(sortvar=sortvar, 
                                     modality=modality,
                                     clustersIDs=clustersIDs, 
                                     conditions=conditions,
                                     noctave=noctave,
                                     nvoice=nvoice, 
                                     nCycles=nCycles,
                                     itcFilenamePattern=
                                      itcFilenamePattern,
                                     plotsFilenamePattern=
                                      plotsFilenamePattern,
                                     xlim=xlim, 
                                     ylim=ylim, 
                                     zlim=zlim,
                                     cuts=cuts, 
                                     nFreqTicks=nFreqTicks)
    }
}

plotITCsForClusters <- 
 function(sortvar,
           modality, 
           clustersIDs, 
           conditions,
           noctave, nvoice, nCycles, 
           itcFilenamePattern,
           plotsFilenamePattern,
           xlim,
           ylim,
           zlim,
           cuts,
           nFreqTicks) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %2d", clusterID))
        plotITCsForConditions(sortvar=sortvar, 
                                       modality=modality,
                                       clusterID=clusterID, 
                                       conditions=conditions, 
                                       noctave=noctave, 
                                       nvoice=nvoice, 
                                       nCycles=nCycles,
                                       itcFilenamePattern=
                                        itcFilenamePattern,
                                       plotsFilenamePattern=
                                        plotsFilenamePattern,
                                       xlim=xlim, 
                                       ylim=ylim, 
                                       zlim=zlim,
                                       cuts=cuts, 
                                       nFreqTicks=nFreqTicks)
    }
}

plotITCsForConditions <- 
 function(sortvar,
           modality, 
           clusterID, 
           conditions,
           noctave, nvoice, nCycles, 
           itcFilenamePattern,
           plotsFilenamePattern,
           xlim,
           ylim,
           zlim,
           cuts,
           nFreqTicks) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        itcFilename <- sprintf(itcFilenamePattern, clusterID, condition, 
                                                           sortvar, modality, 
                                                           noctave, nvoice, nCycles)
        itcRes <- get(load(itcFilename))
        plotITCForCondition(sortvar=sortvar,
                                    modality=modality,
                                    clusterID=clusterID,
                                    condition=condition,
                                    noctave=noctave, 
                                    nvoice=nvoice, 
                                    nCycles=nCycles,
                                    itcRes=itcRes,
                                    plotsFilenamePattern=
                                     plotsFilenamePattern,
                                    xlim=xlim,
                                    ylim=ylim,
                                    zlim=zlim,
                                    cuts=cuts,
                                    nFreqTicks=nFreqTicks)
    }
}

plotITCForCondition <- 
 function(sortvar, 
           modality,
           clusterID,
           condition,
           noctave, nvoice, nCycles,
           itcRes,
           plotsFilenamePattern, 
           width=6, height=6, 
           xlim=c(-200, 700),
           ylim=c(1, 35),
           zlim=c(0, 1), 
           cuts=40,
           nFreqTicks=10) {
    for(i in 1:length(itcRes)) {
        show(sprintf("Processing subject %s, component %02d", 
                     itcRes[[i]]$subjectName,
                     itcRes[[i]]$component))
        itcPeakTimeFreq <- c(itcRes[[i]]$peakInfo$time, 
                              itcRes[[i]]$peakInfo$freq) 
        plotFilename <- sprintf(plotsFilenamePattern, sortvar, modality, 
                                                      clusterID, condition, 
                                                      itcRes[[i]]$subjectName, 
                                                      itcRes[[i]]$component, 
                                                      noctave, nvoice, nCycles)
        maskedITC <- itcRes[[i]]$itc
        maskedITC[!itcRes[[i]]$significanceMask] <- 0

        trellis.device("postscript", width=width, height=height, onefile=FALSE,
                   horizontal=FALSE, file=plotFilename)
        plotTimeFreqRes(timeFreqRes=maskedITC,
                         times=itcRes[[i]]$times, 
                         freqs=itcRes[[i]]$freqs, 
                         peakTimeFreq=itcPeakTimeFreq,
                         xlab="Time (ms)", 
                         ylab="Frequency (Hz)",
                         xlim=xlim,
                         ylim=ylim,
                         zlim=zlim,
                         cuts=cuts,
                         region=TRUE,
                         labels=FALSE,
                         main="",
                         nFreqTicks=nFreqTicks)
        dev.off()
    } 
}
