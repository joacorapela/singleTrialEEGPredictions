plotPredictionsCorCoefsVsMaxSFPDs2ForModalities <-
 function(sortvar, modalities, clustersIDs, conditions, maxSFPDs,
                   nResamples, modelSignificance, rConf,
                   subjectsAndComponents, 
                   scFilenamePattern,
                   analyzedConditionsFilenamePattern, 
                   plotsFilenamePattern,
                   xlab, ylab, main, 
                   ...) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotPredictionsCorCoefsVsMaxSFPDs2ForClusters(
         sortvar=sortvar, 
         modality=modality, 
         clustersIDs=clustersIDs, 
         conditions=conditions, 
         maxSFPDs=maxSFPDs,
         nResamples=nResamples,
         modelSignificance=modelSignificance,
         rConf=rConf,
         subjectsAndComponents=subjectsAndComponents,
         scFilenamePattern=scFilenamePattern,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         xlab=xlab, 
         ylab=ylab, 
         main=main, 
         ...)
    }
}

plotPredictionsCorCoefsVsMaxSFPDs2ForClusters <-
 function(sortvar, modality, clustersIDs, conditions, maxSFPDs,
                   nResamples, modelSignificance, rConf,
                   subjectsAndComponents, 
                   scFilenamePattern,
                   analyzedConditionsFilenamePattern, 
                   plotsFilenamePattern,
                   xlab, ylab, main, 
                   ...) {
    for(clusterID in clustersIDs) {
        if(is.null(subjectsAndComponents)) {
            scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
            if(is.null(scFilename)) {
                stop("arguments subjectsAndComponents or scFilenamePattern should be provided")
            }
            subjectsAndComponentsInCluster <-
             getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                                scFilename=scFilename)
        } else {
            subjectsAndComponentsInCluster <- subjectsAndComponents
        }
        show(sprintf("Processing cluster %d", clusterID))
        plotPredictionsCorCoefsVsMaxSFPDs2ForConditions(
         sortvar=sortvar, 
         modality=modality, 
         clusterID=clusterID, 
         conditions=conditions, 
         maxSFPDs=maxSFPDs,
         nResamples=nResamples,
         modelSignificance=modelSignificance,
         rConf=rConf,
         subjectsAndComponents=subjectsAndComponentsInCluster,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern, 
         plotsFilenamePattern=plotsFilenamePattern,
         xlab=xlab, 
         ylab=ylab, 
         main=main, 
         ...)
    }
}

plotPredictionsCorCoefsVsMaxSFPDs2ForConditions <-
 function(sortvar, modality, clusterID, conditions, maxSFPDs,
                   nResamples, modelSignificance, rConf,
                   subjectsAndComponents, 
                   analyzedConditionsFilenamePattern, 
                   plotsFilenamePattern,
                   xlab, ylab, main, 
                   ...) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        plotPredictionsCorCoefsVsMaxSFPDs2ForSubjectsAndComponents(
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         maxSFPDs=maxSFPDs,
         nResamples=nResamples,
         modelSignificance=modelSignificance,
         rConf=rConf,
         subjectsAndComponents=subjectsAndComponents,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern, 
         plotsFilenamePattern=plotsFilenamePattern,
         xlab=xlab,
         ylab=ylab,
         main=main,
         ...)
    }
}

plotPredictionsCorCoefsVsMaxSFPDs2ForSubjectsAndComponents <-
 function(sortvar, modality, clusterID, condition, maxSFPDs,
                   nResamples, modelSignificance, rConf,
                   subjectsAndComponents, 
                   analyzedConditionsFilenamePattern,
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
        plotPredictionsCorCoefsVsMaxSFPDs2ForSubjectAndComponent(
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         maxSFPDs=maxSFPDs,
         nResamples=nResamples,
         modelSignificance=modelSignificance,
         rConf=rConf,
         subjectName=subjectName,
         component=component,
         analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern, 
         plotsFilenamePattern=plotsFilenamePattern,
         xlab=xlab,
         ylab=ylab,
         main=main,
         ...)
    }
}
plotPredictionsCorCoefsVsMaxSFPDs2ForSubjectAndComponent <-
 function(sortvar, modality, clusterID, condition, maxSFPDs,
                   nResamples, modelSignificance, rConf,
                   subjectName, 
                   component,
                   analyzedConditionsFilenamePattern,
                   plotsFilenamePattern,
                   xlab,
                   ylab="Correlation Coefficient",
                   main="", 
                   ...) {
    i <- 1
    predSFPDurCorsCIs <- c()
    while(i<=length(maxSFPDs)) {
        maxSFPD <- maxSFPDs[i]
        analyzedConditionFilename <- sprintf(analyzedConditionsFilenamePattern,
                                              clusterID,
                                              clusterID,
                                              condition,
                                              sortvar,
                                              modality,
                                              subjectName,
                                              component,
                                              maxSFPD)
        if(!file.exists(analyzedConditionFilename)) {
            break
        }
        show(sprintf("Processing maxSFPD %d", maxSFPDs[i]))
        analyzedCondition <- get(load(analyzedConditionFilename))
        if(!is.null(analyzedCondition$predSFPDurCorCI)) {
            if(is.nan(modelSignificance)) {
                predSFPDurCorsCIs <- rbind(predSFPDurCorsCIs, 
                                            analyzedCondition$predSFPDurCorCI) 
            } else {
                if(analyzedCondition$lrtRes$pValue<modelSignificance) {
                    predSFPDurCorsCIs <- rbind(predSFPDurCorsCIs, 
                                                analyzedCondition$
                                                 predSFPDurCorCI) 
                } else {
                    predSFPDurCorsCIs <- rbind(predSFPDurCorsCIs, c(0, 0, 0))
                }
            }
        } else {
            predSFPDurCorsCIs <- rbind(predSFPDurCorsCIs, c(0, 0, 0))
        }
        i <- i+1
    }
    usedMaxSFPDs <- maxSFPDs[1:(i-1)]
    if(nrow(predSFPDurCorsCIs)>1) {
        plotCorCoefsCIsVsMaxSFPDs(maxSFPDs=usedMaxSFPDs/1000, 
                                   cis=predSFPDurCorsCIs, 
                                   xlab=xlab, ylab=ylab, main=main, ...)
    } else {
        print(getEmptyPlot())
    }
    plotFilename <- sprintf(plotsFilenamePattern, 
                             clusterID,
                             modality, 
                             sortvar, 
                             clusterID, 
                             condition,
                             subjectName,
                             component)
    ggsave(filename=plotFilename)
}
