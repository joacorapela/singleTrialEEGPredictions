
require(singleTrialEEGPredictions)

plotPredictionsCorCoefsVsAveragedBehavioralMeasuresToDistributeForModalities(
    sortvar="STDsBeforeDsDLT500", 
    modalities=c("Visual"),
    clustersIDs=c(04),
    conditions=c("switchvision"),
    modelSignificance=NaN,
    annotationPattern="r=%.2f, p=%.04f",
    scFilenamePattern=
     "data/subjectsAndComponents/subjectsAndComponentsClusterID%02d.tsv",
    analyzedConditionsFilenamePattern=
     "results/phaseAnalysis/c%02d/analyzedDataClusterID%02dCondition%sSortvars%s%sStim%sC%02dMinSFPD%05dMaxSFPD%05d.RData",
    minAndMaxSFPDOfBestPredictionsFilenamePattern=
     "results/phaseAnalysis/c%02d/minAndMaxSFPDsOfBestPredictionsClusterID%02dCondition%sSortvars%s%sStim%sC%02d.txt",
    behavioralMeasuresFilenamePattern=
     "data/behavioralAnalysis/errorRates/errorRates%s.tsv",
    plotsFilenamePattern=
     "figures/phaseAnalysis/c%02d/predictionsCorCoefsVsErrorRatesBestMinMaxSFPDs%s%sC%02d%s.eps", 
    xCorCoefAnnotation=Inf,
    yCorCoefAnnotation=Inf,
    hjust=1,
    vjust=1,
    sizePoints=3,
    sizeLabels=3.5, 
    sizeAnnotations=4.5, 
    xlab="Error Rate", 
    ylab="Correlation Coefficient", 
    main="", 
    width=6, 
    height=6)
