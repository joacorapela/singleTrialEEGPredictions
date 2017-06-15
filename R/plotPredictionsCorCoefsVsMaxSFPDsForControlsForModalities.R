plotPredictionsCorCoefsVsMaxSFPDsForControlsForModalities <-
 function(sortvar, modalities, clustersIDs, conditions, minSFPD, maxSFPDs,
                   nResamples, modelSignificance, ciConf,
                   subjectsAndComponents, 
                   coefsFilenamePattern, 
                   plotsFilenamePattern,
                   xlab, ylab, main, 
                   ...) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotPredictionsCorCoefsVsMaxSFPDsForControlsForClusters(
         sortvar=sortvar, 
         modality=modality, 
         clustersIDs=clustersIDs, 
         conditions=conditions, 
         minSFPD=minSFPD,
         maxSFPDs=maxSFPDs,
         nResamples=nResamples,
         modelSignificance=modelSignificance,
         ciConf=ciConf,
         subjectsAndComponents=subjectsAndComponents,
         coefsFilenamePattern=coefsFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         xlab=xlab, 
         ylab=ylab, 
         main=main, 
         ...)
    }
}

plotPredictionsCorCoefsVsMaxSFPDsForControlsForClusters <-
 function(sortvar, modality, clustersIDs, conditions, minSFPD, maxSFPDs,
                   nResamples, modelSignificance, ciConf,
                   subjectsAndComponents, 
                   coefsFilenamePattern, 
                   plotsFilenamePattern,
                   xlab, ylab, main, 
                   ...) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %d", clusterID))
        plotPredictionsCorCoefsVsMaxSFPDsForControlsForConditions(
         sortvar=sortvar, 
         modality=modality, 
         clusterID=clusterID, 
         conditions=conditions, 
         minSFPD=minSFPD,
         maxSFPDs=maxSFPDs,
         nResamples=nResamples,
         modelSignificance=modelSignificance,
         ciConf=ciConf,
         subjectsAndComponents=subjectsAndComponents,
         coefsFilenamePattern=coefsFilenamePattern, 
         plotsFilenamePattern=plotsFilenamePattern,
         xlab=xlab, 
         ylab=ylab, 
         main=main, 
         ...)
    }
}

plotPredictionsCorCoefsVsMaxSFPDsForControlsForConditions <-
 function(sortvar, modality, clusterID, conditions, minSFPD, maxSFPDs,
                   nResamples, modelSignificance, ciConf,
                   subjectsAndComponents, 
                   coefsFilenamePattern, 
                   plotsFilenamePattern,
                   xlab, ylab, main, 
                   ...) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        plotPredictionsCorCoefsVsMaxSFPDsForControlsForSubjectsAndComponents(
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         minSFPD=minSFPD,
         maxSFPDs=maxSFPDs,
         nResamples=nResamples,
         modelSignificance=modelSignificance,
         ciConf=ciConf,
         subjectsAndComponents=subjectsAndComponents,
         coefsFilenamePattern=coefsFilenamePattern, 
         plotsFilenamePattern=plotsFilenamePattern,
         xlab=xlab,
         ylab=ylab,
         main=main,
         ...)
    }
}

plotPredictionsCorCoefsVsMaxSFPDsForControlsForSubjectsAndComponents <-
 function(sortvar, modality, clusterID, condition, minSFPD, maxSFPDs,
                   nResamples, modelSignificance, ciConf,
                   subjectsAndComponents, 
                   coefsFilenamePattern,
                   plotsFilenamePattern,
                   xlab, ylab, main, 
                   ...) {
    predSFPDurCorsCIs <- c()
    behavioralMeasuresCIs <- c()
    scNames <- c()
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
        plotPredictionsCorCoefsVsMaxSFPDsForControlsForSubjectAndComponent(
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         minSFPD=minSFPD,
         maxSFPDs=maxSFPDs,
         nResamples=nResamples,
         modelSignificance=modelSignificance,
         ciConf=ciConf,
         subjectName=subjectName,
         component=component,
         coefsFilenamePattern=coefsFilenamePattern, 
         plotsFilenamePattern=plotsFilenamePattern,
         xlab=xlab,
         ylab=ylab,
         main=main,
         ...)
    }
}
plotPredictionsCorCoefsVsMaxSFPDsForControlsForSubjectAndComponent <-
 function(sortvar, modality, clusterID, condition, minSFPD, maxSFPDs,
                   nResamples, modelSignificance, ciConf,
                   subjectName, 
                   component,
                   coefsFilenamePattern,
                   plotsFilenamePattern,
                   xlab,
                   ylab="Correlation Coefficient",
                   main="", 
                   ...) {
    i <- 1
    predSFPDurCorsCIs <- c()
    while(i<=length(maxSFPDs)) {
        maxSFPD <- maxSFPDs[i]
        coefsFilename <- sprintf(coefsFilenamePattern,
                                              clusterID,
                                              clusterID,
                                              condition,
                                              sortvar,
                                              modality,
                                              subjectName,
                                              component,
                                              minSFPD,
                                              maxSFPD)
        if(!file.exists(coefsFilename)) {
            break
        }
        show(sprintf("Processing maxSFPD %d", maxSFPDs[i]))
        coefs <- get(load(coefsFilename))
        bootRes <- bootstrapMedian(coefs, nResamples=nResamples)
        controlCI <- getBootstrapCIs(bootRes, conf=ciConf)
        predSFPDurCorsCIs <- rbind(predSFPDurCorsCIs, controlCI)
        i <- i+1
    }
    usedMaxSFPDs <- maxSFPDs[1:(i-1)]
    if(nrow(predSFPDurCorsCIs)>1) {
        plotCIsForCategories(categoriesNames=sprintf("%d", usedMaxSFPDs), 
                              cis=predSFPDurCorsCIs, 
                              xlab=xlab, ylab=ylab, main=main, ...)
    } else {
        print(getEmptyPlot())
    }
    plotFilename <- sprintf(plotsFilenamePattern, 
                             clusterID, 
                             clusterID, 
                             condition,
                             sortvar, 
                             modality, 
                             subjectName,
                             component)
    ggsave(filename=plotFilename)
}
