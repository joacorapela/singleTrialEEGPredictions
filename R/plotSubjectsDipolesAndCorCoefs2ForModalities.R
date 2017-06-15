plotSubjectsDipolesAndCorCoefs2ForModalities <-
 function(sortvar, modalities, conditions, subjectsNames, clustersIDs, 
                   modelSignificance, scaleLimit, pointSize,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, 
                   dipfitInfoFilenamePattern,
                   plotsFilenamePattern,
                   width, height, ...) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        plotSubjectsDipolesAndCorCoefs2ForConditions(
         sortvar=sortvar, 
         modality=modality, 
         conditions=conditions, 
         subjectsNames=subjectsNames,
         clustersIDs=clustersIDs, 
         modelSignificance=modelSignificance,
         scaleLimit=scaleLimit,
         pointSize=pointSize,
         scFilenamePattern=scFilenamePattern,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         dipfitInfoFilenamePattern=dipfitInfoFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         width=width, 
         height=height, ...)
    }
}

plotSubjectsDipolesAndCorCoefs2ForConditions <-
 function(sortvar, modality, conditions, subjectsNames, clustersIDs,
                   modelSignificance, scaleLimit, pointSize,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, 
                   dipfitInfoFilenamePattern,
                   plotsFilenamePattern,
                   width, height, ...) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        plotSubjectsDipolesAndCorCoefs2ForSubjects(
         sortvar=sortvar, 
         modality=modality, 
         condition=condition, 
         subjectsNames=subjectsNames,
         clustersIDs=clustersIDs, 
         modelSignificance=modelSignificance,
         scaleLimit=scaleLimit,
         pointSize=pointSize,
         scFilenamePattern=scFilenamePattern,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         dipfitInfoFilenamePattern=dipfitInfoFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         width=width, 
         height=height, ...)
    }
}

plotSubjectsDipolesAndCorCoefs2ForSubjects <-
 function(sortvar, modality, condition, subjectsNames, clustersIDs,
                   modelSignificance, scaleLimit, pointSize,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, 
                   dipfitInfoFilenamePattern,
                   plotsFilenamePattern,
                   width, height, ...) {
    for(subjectName in subjectsNames) {
        show(sprintf("Processing subject %s", subjectName))
        plotSubjectsDipolesAndCorCoefs2ForSubject(
         sortvar=sortvar, 
         modality=modality, 
         condition=condition, 
         subjectName=subjectName,
         clustersIDs=clustersIDs, 
         modelSignificance=modelSignificance,
         scaleLimit=scaleLimit,
         pointSize=pointSize,
         scFilenamePattern=scFilenamePattern,
         minAndMaxSFPDOfBestPredictionsFilenamePattern=
          minAndMaxSFPDOfBestPredictionsFilenamePattern,
         analyzedDataFilenamePattern=analyzedDataFilenamePattern,
         dipfitInfoFilenamePattern=dipfitInfoFilenamePattern,
         plotsFilenamePattern=plotsFilenamePattern,
         width=width, 
         height=height, ...)
    }
}

plotSubjectsDipolesAndCorCoefs2ForSubject <-
 function(sortvar, modality, condition, subjectName, clustersIDs,
                   modelSignificance, scaleLimit, pointSize,
                   scFilenamePattern,
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, 
                   dipfitInfoFilenamePattern,
                   plotsFilenamePattern,
                   width, height, ...) {
    dipfitInfoFilename <- sprintf(dipfitInfoFilenamePattern, subjectName)
    dipfitInfo <- getSubjectDipfitInfo(dipfitInfoFilename=dipfitInfoFilename)

    clustersAndComponentsNames <- c()
    subjectCorCoefsCIs <- c()
    dipolesPositions <- list(x=c(), y=c(), z=c())
    doPlot <- FALSE
    for(clusterID in clustersIDs) {
        scFilename <- sprintf(scFilenamePattern, clusterID, sortvar)
        subjectsAndComponents <- 
         getSubjectsAndComponentsInCluster(clusterID=clusterID, 
                                            scFilename=scFilename)
        componentIndices <-
         which(subjectsAndComponents$subjectName==subjectName)
        for(i in componentIndices) {
            component <- subjectsAndComponents$component[i]
            minAndMaxSFPDOfBestPredictionsFilename <- 
             sprintf(minAndMaxSFPDOfBestPredictionsFilenamePattern, clusterID,
                      clusterID, condition, sortvar, modality, subjectName, 
                      component)
            res <- readLines(minAndMaxSFPDOfBestPredictionsFilename)
            minSFPD <- as.integer(res[1])
            maxSFPD <- as.integer(res[2])
            analyzedDataFilename <- sprintf(analyzedDataFilenamePattern,
                                             clusterID,
                                             clusterID,
                                             condition,
                                             sortvar,
                                             modality,
                                             subjectName,
                                             component,
                                             minSFPD,
                                             maxSFPD)
            analyzedData <- get(load(analyzedDataFilename))
            if(!is.null(analyzedData$predSFPDurCorCI) &&
               !is.null(analyzedData$lrtRes) && 
               analyzedData$lrtRes$pValue<modelSignificance) {
                clustersAndComponentsNames <- c(clustersAndComponentsNames, 
                                                 sprintf("%02d%02d", 
                                                         clusterID, component))
                subjectCorCoefsCIs <- rbind(subjectCorCoefsCIs,
                                             analyzedData$predSFPDurCorCI)
                # rotate coordinates by 90° clockwise
                # rx=-y, ry=x, rz=z
                rx <- -dipfitInfo[[component]]$posxyz[2]
                ry <- dipfitInfo[[component]]$posxyz[1]
                rz <- dipfitInfo[[component]]$posxyz[3]
                #
                rx <- 
                dipolesPositions$x <- c(dipolesPositions$x, rx)
                dipolesPositions$y <- c(dipolesPositions$y, ry)
                dipolesPositions$z <- c(dipolesPositions$z, rz)
                doPlot <- TRUE
            }
        }
    }
    plotFilename <- sprintf(plotsFilenamePattern, subjectName, sortvar, 
                                                  modality, condition)
    if(doPlot) {
        p <- getPlotDipoles(dipolesPositions=dipolesPositions,
                             dipolesColorScales=subjectCorCoefsCIs[,1],
                             pointSize=pointSize,
                             scaleLimit=scaleLimit,
                             guide="colourbar",
                             main=subjectName)
    } else {
        p <- getEmptyPlot()
    }
    trellis.device("postscript", width=width, height=height, onefile=FALSE,
                   horizontal=FALSE, file=plotFilename)
    print(p)
    dev.off()
}

