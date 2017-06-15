# conditionX is greater/less than condition Y

plotPropSignModelsBTWConditions2 <- 
 function(sortvar, modality, conditionY, conditionX, clustersIDs, modelSignificance,
                   nResamples, conf, annotationPattern, 
                   scFilenamePattern, 
                   minAndMaxSFPDOfBestPredictionsFilenamePattern,
                   analyzedDataFilenamePattern, ylab, xlab, main, ...) {

    proportionsCondY <- 
     getPropSignModelsForClusters2(sortvar=sortvar, 
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
     getPropSignModelsForClusters2(sortvar=sortvar, 
                                    modality=modality,
                                    clustersIDs=clustersIDs,
                                    condition=conditionX,
                                    modelSignificance=modelSignificance,
                                    scFilenamePattern=scFilenamePattern,
                                    minAndMaxSFPDOfBestPredictionsFilenamePattern=
                                     minAndMaxSFPDOfBestPredictionsFilenamePattern,
                                    analyzedDataFilenamePattern=
                                     analyzedDataFilenamePattern)
#     meanBootRes <- bootstrapMeanPairedDifference(x=proportionsCondX, 
#                                                   y=proportionsCondY, 
#                                                   nResamples=nResamples)
#     meanBootCIs <- getBootstrapCIs(bootRes=meanBootRes, conf=conf)
#     annotation <- sprintf(annotationPattern, meanBootCIs[1], meanBootCIs[2], meanBootCIs[3])
#     medianBootRes <- bootstrapMedianPairedDifference(x=proportionsCondX, 
#                                                   y=proportionsCondY, 
#                                                   nResamples=nResamples)
#     medianBootCIs <- getBootstrapCIs(bootRes=medianBootRes, conf=conf)
#     annotation <- sprintf(annotationPattern, medianBootCIs[1], medianBootCIs[2], medianBootCIs[3])
    res <- ydbt(x=proportionsCondX, y=proportionsCondY)
    annotation <- sprintf(annotationPattern, res$dif, res$ci[1], res$ci[2])
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
