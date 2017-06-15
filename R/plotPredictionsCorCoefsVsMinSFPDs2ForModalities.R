plotPredictionsCorCoefsVsMinSFPDs2ForModalities <-
 function(sortvar, modalities, clustersIDs, conditions, minSFPDs, maxSFPD,
                   nResamples, modelSignificance, rConf,
                   subjectsAndComponents, 
                   analyzedConditionsFilenamePattern, 
                   plotsFilenamePattern,
                   xlab, ylab, main, 
                   ...) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotPredictionsCorCoefsVsMinSFPDs2ForClusters(
         sortvar=sortvar, 
         modality=modality, 
         clustersIDs=clustersIDs, 
         conditions=conditions, 
         minSFPDs=minSFPDs,
         maxSFPD=maxSFPD,
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

plotPredictionsCorCoefsVsMinSFPDs2ForClusters <-
 function(sortvar, modality, clustersIDs, conditions, minSFPDs, maxSFPD,
                   nResamples, modelSignificance, rConf,
                   subjectsAndComponents, 
                   analyzedConditionsFilenamePattern, 
                   plotsFilenamePattern,
                   xlab, ylab, main, 
                   ...) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %d", clusterID))
        plotPredictionsCorCoefsVsMinSFPDs2ForConditions(
         sortvar=sortvar, 
         modality=modality, 
         clusterID=clusterID, 
         conditions=conditions, 
         minSFPDs=minSFPDs,
         maxSFPD=maxSFPD,
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

plotPredictionsCorCoefsVsMinSFPDs2ForConditions <-
 function(sortvar, modality, clusterID, conditions, minSFPDs, maxSFPD,
                   nResamples, modelSignificance, rConf,
                   subjectsAndComponents, 
                   analyzedConditionsFilenamePattern, 
                   plotsFilenamePattern,
                   xlab, ylab, main, 
                   ...) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        plotPredictionsCorCoefsVsMinSFPDs2ForSubjectsAndComponents(
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         minSFPDs=minSFPDs,
         maxSFPD=maxSFPD,
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

plotPredictionsCorCoefsVsMinSFPDs2ForSubjectsAndComponents <-
 function(sortvar, modality, clusterID, condition, minSFPDs, maxSFPD,
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
        plotPredictionsCorCoefsVsMinSFPDs2ForSubjectAndComponent(
         sortvar=sortvar,
         modality=modality,
         clusterID=clusterID,
         condition=condition,
         minSFPDs=minSFPDs,
         maxSFPD=maxSFPD,
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
plotPredictionsCorCoefsVsMinSFPDs2ForSubjectAndComponent <-
 function(sortvar, modality, clusterID, condition, minSFPDs, maxSFPD,
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
    for(minSFPD in minSFPDs) {
        analyzedConditionFilename <- sprintf(analyzedConditionsFilenamePattern,
                                              clusterID,
                                              condition,
                                              sortvar,
                                              modality,
                                              subjectName,
                                              component,
                                              minSFPD,
                                              maxSFPD)
        show(sprintf("Processing minSFPD %d", minSFPD))
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
    if(nrow(predSFPDurCorsCIs)>1) {
        plotCIsForCategories(categoriesNames=sprintf("%d", minSFPDs), 
                              cis=predSFPDurCorsCIs, 
                              xlab=xlab, ylab=ylab, main=main, ...)
    } else {
        print(getEmptyPlot())
    }
    plotFilename <- sprintf(plotsFilenamePattern, 
                             maxSFPD,
                             modality, 
                             sortvar, 
                             clusterID, 
                             condition,
                             subjectName,
                             component)
    ggsave(filename=plotFilename)
}
