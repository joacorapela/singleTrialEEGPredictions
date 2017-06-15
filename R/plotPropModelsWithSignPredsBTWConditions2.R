# conditionX is greater/less than condition Y

plotPropModelsWithSignPredsBTWConditions2 <- 
 function(sortvar, modality, conditionY, conditionX, clustersIDs, modelSignificance,
                   nResamples, conf, annotationPattern, 
                   scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ylab, xlab, main, ...) {

    proportionsCondY <- 
     getPropModelsWithSignPredsForClusters2(sortvar=sortvar, 
                                    modality=modality,
                                    clustersIDs=clustersIDs,
                                    condition=conditionY,
                                    modelSignificance=modelSignificance,
                                    scFilenamePattern=scFilenamePattern,
                                    minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                     minAndMaxSFPDOfBestPredictionsFilenamePattern,
                                    analyzedDataFilenamePattern=
                                     analyzedDataFilenamePattern)
    proportionsCondX <- 
     getPropModelsWithSignPredsForClusters2(sortvar=sortvar, 
                                    modality=modality,
                                    clustersIDs=clustersIDs,
                                    condition=conditionX,
                                    modelSignificance=modelSignificance,
                                    scFilenamePattern=scFilenamePattern,
                                    minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                     minAndMaxSFPDOfBestPredictionsFilenamePattern,
                                    analyzedDataFilenamePattern=
                                     analyzedDataFilenamePattern)
#     medianBootRes <- bootstrapMeanPairedDifference(x=proportionsCondX, 
    medianBootRes <- bootstrapMedianPairedDifference(x=proportionsCondX, 
                                              y=proportionsCondY, 
                                              nResamples=nResamples)
    medianBootCIs <- getBootstrapCIs(bootRes=medianBootRes, conf=conf)
    annotation <- sprintf(annotationPattern, medianBootCIs[1], medianBootCIs[2], medianBootCIs[3])
    clustersLabels <- sprintf("c%02d", clustersIDs)
    plotPropSignModels(ysProps=proportionsCondY,
                        xsProps=proportionsCondX,
                        annotation=annotation,
                        clustersLabels=clustersLabels,
                        ylab=ylab, 
                        xlab=xlab, 
                        main=main,
                        ...)
}
