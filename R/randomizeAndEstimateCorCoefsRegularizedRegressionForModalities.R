
randomizeAndEstimateCorCoefsRegularizedRegressionForModalities <-
 function(sortvar, modalities, clustersIDs, conditions, 
                   subjectsAndComponents,
                   minSFPDs, maxSFPDs, lambda, order, scaleData, 
                   nGroups, 
                   a0, b0, c0, d0, maxIter, convergenceTol, nRandomizations,
                   clipFrom, clipTo, srate, 
                   stdsNextDsLatenciesInfo,
                   stdsNextCuesLatenciesInfo,
                   stdsNextEOBsLatenciesInfo,
                   phaseERPImageFilenamePattern, 
                   timesPPPhaseERPIFilenamePattern,
                   corCoefsFilenamePattern, 
                   plotSteps,
                   averageTrialsWinSize,
                   plotsFilenamePattern,
                   width, height) {
    for(modality in modalities) {
        show(sprintf("Processing modality %s", modality))
        randomizeAndEstimateCorCoefsRegularizedRegressionForClusters(
         sortvar=sortvar, 
         modality=modality, 
         clustersIDs=clustersIDs, 
         conditions=conditions, 
         subjectsAndComponents=subjectsAndComponents,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, 
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nRandomizations=nRandomizations,
         clipFrom=clipFrom, clipTo=clipTo, 
         srate=srate,
         stdsNextDsLatenciesInfo=stdsNextDsLatenciesInfo,
         stdsNextCuesLatenciesInfo=stdsNextCuesLatenciesInfo,
         stdsNextEOBsLatenciesInfo=stdsNextEOBsLatenciesInfo,
         phaseERPImageFilenamePattern=phaseERPImageFilenamePattern,
         timesPPPhaseERPIFilenamePattern=timesPPPhaseERPIFilenamePattern,
         corCoefsFilenamePattern=corCoefsFilenamePattern,
         plotSteps=plotSteps,            
         averageTrialsWinSize=averageTrialsWinSize,
         plotsFilenamePattern=plotsFilenamePattern, 
         width=width, height=height)
    }
}

randomizeAndEstimateCorCoefsRegularizedRegressionForClusters <-
 function(sortvar, modality, clustersIDs, conditions, 
                   subjectsAndComponents,
                   minSFPDs, maxSFPDs, lambda, order, scaleData, 
                   nGroups, 
                   a0, b0, c0, d0, maxIter, convergenceTol, nRandomizations,
                   clipFrom, clipTo, srate, 
                   stdsNextDsLatenciesInfo,
                   stdsNextCuesLatenciesInfo,
                   stdsNextEOBsLatenciesInfo,
                   phaseERPImageFilenamePattern, 
                   timesPPPhaseERPIFilenamePattern,
                   corCoefsFilenamePattern, 
                   plotSteps,
                   averageTrialsWinSize,
                   plotsFilenamePattern,
                   width, height) {
    for(clusterID in clustersIDs) {
        show(sprintf("Processing cluster %2d", clusterID))
        randomizeAndEstimateCorCoefsRegularizedRegressionForConditions(
         sortvar=sortvar, 
         modality=modality, 
         clusterID=clusterID, 
         conditions=conditions, 
         subjectsAndComponents=subjectsAndComponents,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, 
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nRandomizations=nRandomizations,
         clipFrom=clipFrom, clipTo=clipTo, 
         srate=srate,
         stdsNextDsLatenciesInfo=stdsNextDsLatenciesInfo,
         stdsNextCuesLatenciesInfo=stdsNextCuesLatenciesInfo,
         stdsNextEOBsLatenciesInfo=stdsNextEOBsLatenciesInfo,
         phaseERPImageFilenamePattern=phaseERPImageFilenamePattern,
         timesPPPhaseERPIFilenamePattern=timesPPPhaseERPIFilenamePattern,
         corCoefsFilenamePattern=corCoefsFilenamePattern,
         plotSteps=plotSteps,            
         averageTrialsWinSize=averageTrialsWinSize,
         plotsFilenamePattern=plotsFilenamePattern, 
         width=width, height=height)
    }
}

randomizeAndEstimateCorCoefsRegularizedRegressionForConditions <-
 function(sortvar, modality, clusterID, conditions, 
                   subjectsAndComponents,
                   minSFPDs, maxSFPDs, lambda, order, scaleData, 
                   nGroups, 
                   a0, b0, c0, d0, maxIter, convergenceTol, nRandomizations,
                   clipFrom, clipTo, srate, 
                   stdsNextDsLatenciesInfo,
                   stdsNextCuesLatenciesInfo,
                   stdsNextEOBsLatenciesInfo,
                   phaseERPImageFilenamePattern, 
                   timesPPPhaseERPIFilenamePattern,
                   corCoefsFilenamePattern, 
                   plotSteps,
                   averageTrialsWinSize,
                   plotsFilenamePattern,
                   width, height) {
    for(condition in conditions) {
        show(sprintf("Processing condition %s", condition))
        randomizeAndEstimateCorCoefsRegularizedRegressionForSubjectsAndComponents(
         sortvar=sortvar, 
         modality=modality, 
         clusterID=clusterID, 
         condition=condition, 
         subjectsAndComponents=subjectsAndComponents,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, 
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nRandomizations=nRandomizations,
         clipFrom=clipFrom, clipTo=clipTo, 
         srate=srate,
         stdsNextDsLatenciesInfo=stdsNextDsLatenciesInfo,
         stdsNextCuesLatenciesInfo=stdsNextCuesLatenciesInfo,
         stdsNextEOBsLatenciesInfo=stdsNextEOBsLatenciesInfo,
         phaseERPImageFilenamePattern=phaseERPImageFilenamePattern,
         timesPPPhaseERPIFilenamePattern=timesPPPhaseERPIFilenamePattern,
         corCoefsFilenamePattern=corCoefsFilenamePattern,
         plotSteps=plotSteps,            
         averageTrialsWinSize=averageTrialsWinSize,
         plotsFilenamePattern=plotsFilenamePattern, 
         width=width, height=height)
    }
}

randomizeAndEstimateCorCoefsRegularizedRegressionForSubjectsAndComponents <-
 function(sortvar, modality, clusterID, condition, 
                   subjectsAndComponents,
                   minSFPDs, maxSFPDs, lambda, order, scaleData, 
                   nGroups, 
                   a0, b0, c0, d0, maxIter, convergenceTol, nRandomizations,
                   clipFrom, clipTo, srate, 
                   stdsNextDsLatenciesInfo,
                   stdsNextCuesLatenciesInfo,
                   stdsNextEOBsLatenciesInfo,
                   phaseERPImageFilenamePattern, 
                   timesPPPhaseERPIFilenamePattern,
                   corCoefsFilenamePattern, 
                   plotSteps,
                   averageTrialsWinSize,
                   plotsFilenamePattern,
                   width, height) {
    for(i in 1:nrow(subjectsAndComponents)) {
        subjectName <- subjectsAndComponents[i, "subjectName"]
        component <- subjectsAndComponents[i, "component"]
        show(sprintf("Processing subject %s and component %02d", subjectName,
                     component))
        randomizeAndEstimateCorCoefsRegularizedRegressionForSubjectAndComponent(
         sortvar=sortvar, 
         modality=modality, 
         clusterID=clusterID, 
         condition=condition, 
         subjectName=subjectName,
         component=component,
         minSFPDs=minSFPDs,
         maxSFPDs=maxSFPDs,
         lambda=lambda,
         order=order, 
         scaleData=scaleData,
         nGroups=nGroups, 
         a0=a0, b0=b0, c0=c0, d0=d0, 
         maxIter=maxIter,
         convergenceTol=convergenceTol,
         nRandomizations=nRandomizations,
         clipFrom=clipFrom, clipTo=clipTo, 
         srate=srate,
         stdsNextDsLatenciesInfo=stdsNextDsLatenciesInfo,
         stdsNextCuesLatenciesInfo=stdsNextCuesLatenciesInfo,
         stdsNextEOBsLatenciesInfo=stdsNextEOBsLatenciesInfo,
         phaseERPImageFilenamePattern=phaseERPImageFilenamePattern,
         timesPPPhaseERPIFilenamePattern=timesPPPhaseERPIFilenamePattern,
         corCoefsFilenamePattern=corCoefsFilenamePattern,
         plotSteps=plotSteps,            
         averageTrialsWinSize=averageTrialsWinSize,
         plotsFilenamePattern=plotsFilenamePattern, 
         width=width, height=height)
    }
}
randomizeAndEstimateCorCoefsRegularizedRegressionForSubjectAndComponent <-
 function(sortvar, modality, clusterID, condition, 
                   subjectName,
                   component,
                   minSFPDs, maxSFPDs, lambda, order, scaleData, 
                   nGroups, 
                   a0, b0, c0, d0, maxIter, convergenceTol, nRandomizations,
                   clipFrom, clipTo, srate, 
                   stdsNextDsLatenciesInfo,
                   stdsNextCuesLatenciesInfo,
                   stdsNextEOBsLatenciesInfo,
                   phaseERPImageFilenamePattern, 
                   timesPPPhaseERPIFilenamePattern,
                   corCoefsFilenamePattern, 
                   plotSteps,
                   averageTrialsWinSize,
                   plotsFilenamePattern,
                   width, height) {
    preProcessedPhaseERPIFilename <- 
     sprintf(phaseERPImageFilenamePattern, 
              clusterID,
              condition,
              sortvar,
              modality,
              subjectName,
              component)
    preProcessedPhaseERPI <- get(load(preProcessedPhaseERPIFilename))
    for(minSFPD in minSFPDs) {
        i <- 1
        while(i<=length(maxSFPDs) && 
               maxSFPDs[i]<=max(preProcessedPhaseERPI$sfpds)) {
            if(maxSFPDs[i]<minSFPD) {
                next
            }
            show(sprintf("Processing %s%02d: minSFPD %d, maxSFPD %d",
                         subjectName, component, minSFPD, maxSFPDs[i]))
            maxSFPD <- maxSFPDs[i]
            corCoefsFilename <- 
             sprintf(corCoefsFilenamePattern, clusterID, condition,
                                              sortvar, modality, subjectName, 
                                              component, minSFPD, maxSFPD)
            corCoefs <- randomizeAndEstimateCorCoefsRegularizedRegression(
                         sortvar=sortvar, 
                         modality=modality, 
                         clusterID=clusterID, 
                         condition=condition, 
                         subjectName=subjectName,
                         component=component,
                         minSFPD=minSFPD,
                         maxSFPD=maxSFPD,
                         lambda=lambda,
                         order=order, 
                         scaleData=scaleData,
                         nGroups=nGroups, 
                         a0=a0, b0=b0, c0=c0, d0=d0, 
                         maxIter=maxIter,
                         convergenceTol=convergenceTol,
                         nRandomizations=nRandomizations,
                         clipFrom=clipFrom, clipTo=clipTo, 
                         srate=srate,
                         stdsNextDsLatenciesInfo=stdsNextDsLatenciesInfo,
                         stdsNextCuesLatenciesInfo=stdsNextCuesLatenciesInfo,
                         stdsNextEOBsLatenciesInfo=stdsNextEOBsLatenciesInfo,
                         phaseERPImageFilenamePattern=
                          phaseERPImageFilenamePattern,
                         timesPPPhaseERPIFilenamePattern=
                          timesPPPhaseERPIFilenamePattern,
                         corCoefsFilenamePattern=corCoefsFilenamePattern,
                         plotSteps=plotSteps,            
                         averageTrialsWinSize=averageTrialsWinSize,
                         plotsFilenamePattern=plotsFilenamePattern, 
                         width=width, height=height)
            save(corCoefs, file=corCoefsFilename)
            i <- i+1
        }
    }
}

randomizeAndEstimateCorCoefsRegularizedRegression <- 
 function(sortvar, modality, clusterID, condition, 
                   subjectName,
                   component,
                   minSFPD, maxSFPD, lambda, order, scaleData, 
                   nGroups, 
                   a0, b0, c0, d0, maxIter, convergenceTol, nRandomizations,
                   clipFrom, clipTo, srate, 
                   stdsNextDsLatenciesInfo,
                   stdsNextCuesLatenciesInfo,
                   stdsNextEOBsLatenciesInfo,
                   phaseERPImageFilenamePattern, 
                   timesPPPhaseERPIFilenamePattern,
                   corCoefsFilenamePattern, 
                   plotSteps,
                   averageTrialsWinSize,
                   plotsFilenamePattern,
                   width, height) {
    phaseERPImageFilename <- sprintf(phaseERPImageFilenamePattern, 
                                      clusterID, 
                                      condition, 
                                      sortvar, 
                                      modality, 
                                      subjectName,
                                      component)
    phaseERPImage <- get(load(phaseERPImageFilename))
    timesPPPhaseERPIFilename <- sprintf(timesPPPhaseERPIFilenamePattern,
                                         clusterID, 
                                         condition, 
                                         sortvar, 
                                         modality, 
                                         subjectName,
                                         component)
    significantTimes <- get(load(timesPPPhaseERPIFilename))
    subjectSTDsNextDsLatencies <- 
     getItemInAVShiftList(listOfItems=stdsNextDsLatenciesInfo, 
                           listFieldName="stdsNextDsLatenciesInfo",
                           keyFieldNames=c("subjectName", "modality",
                                           "condition"),
                           keyFieldValues=c(subjectName, modality,
                                                         condition))
    if(condition%in%c("switchvision", "switchaudition")) {
        subjectSTDsNextCuesLatencies <- 
         getItemInAVShiftList(listOfItems=stdsNextCuesLatenciesInfo, 
                               listFieldName="stdsNextCuesLatenciesInfo",
                               keyFieldNames=c("subjectName", "modality",
                                               "condition"),
                               keyFieldValues=c(subjectName, modality,
                                                             condition))
    } else {
        subjectSTDsNextCuesLatencies <- NA
    }
    subjectSTDsNextEOBsLatencies <- 
     getItemInAVShiftList(listOfItems=stdsNextEOBsLatenciesInfo, 
                           listFieldName="stdsNextEOBsLatenciesInfo",
                           keyFieldNames=c("subjectName", "modality",
                                           "condition"),
                           keyFieldValues=c(subjectName, modality,
                                                         condition))
    corCoefs <- c()
    for(i in 1:nRandomizations) {
        preProcessedERPI <- 
         randomizeSTDsAndPreProcessERPI(phaseERPImage=phaseERPImage, 
                                         clipFrom=clipFrom, clipTo=clipTo, 
                                         srate=srate, 
                                         significantTimes=significantTimes, 
                                         stdsNextDsLatenciesInfo=
                                          subjectSTDsNextDsLatencies, 
                                         stdsNextCuesLatenciesInfo=
                                          subjectSTDsNextCuesLatencies, 
                                         stdsNextEOBsLatenciesInfo=
                                          subjectSTDsNextEOBsLatencies, 
                                         plotSteps=plotSteps, 
                                         averageTrialsWinSize=
                                          averageTrialsWinSize, 
                                         plotsFilenamePattern=
                                          plotsFilenamePattern, 
                                         width=width, height=height)
        corCoef <- estimateCorCoefRegularizedRegression(erpImage=
                                                          preProcessedERPI, 
                                                         minSFPD=minSFPD,
                                                         maxSFPD=maxSFPD,
                                                         lambda=lambda, 
                                                         order=order, 
                                                         scaleData=scaleData, 
                                                         nGroups=nGroups, 
                                                         a0=a0, 
                                                         b0=b0, 
                                                         c0=c0, 
                                                         d0=d0, 
                                                         maxIter=maxIter,
                                                         convergenceTol=
                                                          convergenceTol)
        show(sprintf("corCoef %d (out of %d): %f", i, nRandomizations, corCoef))
        corCoefs <- c(corCoefs, corCoef)
    }
    return(corCoefs)
}
