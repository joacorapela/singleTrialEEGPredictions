
require(singleTrialEEGPredictions)

processAll <- function(sortvar, modalities, clustersIDs, conditions, 
                                sfpdsInfo,
                                noctave, nvoice, nCycles, 
                                shuffled, 
                                minSFPDs, maxSFPDs, 
                                lambda, order, scaleData, nGroups,
                                a0, b0, c0, d0, 
                                computeCoefsCIs=TRUE,
                                maxIter, convergenceTol,
                                erpimageFilenamePattern, 
                                apERPImagesFilenamePattern, 
                                itcsFilenamePattern, 
                                itcsPeaksFilenamePattern, 
                                subjectsAndComponents,
                                scFilenamePattern,
                                preProcessedPhaseERPIFilenamePattern,
                                analyzedConditionsFilenamePattern) {

    computeITCsForModalities(
        sortvar=sortvar,
        modalities=modalities,
        clustersIDs=clustersIDs,
        conditions=conditions,
        noctave=noctave, nvoice=nvoice, nCycles=nCycles,
        minTime=-300,
        maxTime=1000,
        conf=.95,
        significance=.01,
        peakFromTime=100, peakToTime=500, 
        peakFromFreq=1, peakToFreq=14,
        nResamples=2000, 
        erpimageFilenamePattern=erpimageFilenamePattern,
        itcsFilenamePattern=itcsFilenamePattern,
        scFilenamePattern=scFilenamePattern)
    saveITCsPeaksForModalities(
        sortvar=sortvar,
        modalities=modalities,
        clustersIDs=clustersIDs,
        conditions=conditions,
        noctave=noctave, nvoice=nvoice, nCycles=nCycles,
        itcsFilenamePattern=itcsFilenamePattern,
        itcsPeaksFilenamePattern=itcsPeaksFilenamePattern)
    saveAmpPhaseERPImagesForModalities(
        sortvar=sortvar, 
        modalities=modalities,
        clustersIDs=clustersIDs,
        conditions=conditions,
        noctave=noctave, nvoice=nvoice, nCycles=nCycles, 
        shuffleSFPDs=FALSE,
        minTime=NA, 
        maxTime=NA,
        sfpdsInfo=sfpdsInfo,
        itcsPeaksFilenamePattern=itcsPeaksFilenamePattern,
        erpimageFilenamePattern=erpimageFilenamePattern,
        apERPImagesFilenamePattern=apERPImagesFilenamePattern,
        scFilenamePattern=scFilenamePattern)
    preProcessPhaseERPImagesForModalities(
        sortvar=sortvar,
        modalities=modalities,
        clustersIDs=clustersIDs,
        conditions=conditions,
        srate=250,                      
        clipFrom=0, clipTo=500,
        alignmentSignificance=0.01,     
        plotsFilenamePattern=NA,
        scFilenamePattern=scFilenamePattern,
        phaseERPImageFilenamePattern=apERPImagesFilenamePattern,
        preProcessedPhaseERPIFilenamePattern=
         preProcessedPhaseERPIFilenamePattern,
        plotSteps=FALSE,                 
        averageTrialsWinSize=averageTrialsWinSize,
        width=6,
        height=6,              
        xlim=c(-300, 2000),
        zlim=NA)
    analyzeDataRegularizedRegressionForModalities(
        modalities=modalities, 
        sortvar=sortvar, 
        clustersIDs=clustersIDs, 
        conditions=conditions,
        subjectsAndComponents=subjectsAndComponents,
        scFilenamePattern=scFilenamePattern,
        preProcessedPhaseERPIFilenamePattern=preProcessedPhaseERPIFilenamePattern,
        analyzedConditionsFilenamePattern=analyzedConditionsFilenamePattern,
        minSFPDs=minSFPDs,
        maxSFPDs=maxSFPDs,
        lambda=lambda,
        order=order,
        scaleData=scaleData,
        nGroups=nGroups,
        a0=a0,
        b0=b0,
        c0=c0,
        d0=d0,
        computeCoefsCIs=computeCoefsCIs,
        maxIter=maxIter,
        convergenceTol=convergenceTol,
        nResamplesCoefs=2000,
        nResamplesPredictions=2000,
        confCIs=.95)
}

processAll(
    sortvar="STDsBeforeDsDLT500", 
    modalities=c("Visual"),
    clustersIDs=c(04),
    conditions=c("switchvision"),
    sfpdsInfo=get(load("results/sfpds/sfpdsInfo.RData")),
    noctave=8, nvoice=12, nCycles=3,
    shuffled=FALSE,
    minSFPDs=c(0),
    maxSFPDs=seq(from=500, to=10000, by=100),
    lambda=0,
    order=1,
    scaleData=TRUE,
    nGroups=4,
    a0=1e-2, b0=1e-4, c0=1e-2, d0=1e-4, 
    computeCoefsCIs=TRUE,
    maxIter=1e6, convergenceTol=1e-6,
    erpimageFilenamePattern=
     "data/erpImages/erpImageClusterID%02dCondition%sSortvars%s%sStim_%%s.dat",
    itcsFilenamePattern=
     "results/averageSpectralAnalysis/itcSingleSubjectsClusterID%02dCondition%sSortvars%s%sStimNoctave%dNvoice%dNCycles%.2f.RData",
    itcsPeaksFilenamePattern=
     "results/averageSpectralAnalysis/itcsPeaksSingleSubjectsClusterID%02dCondition%sSortvars%s%sStimNoctave%dNvoice%dNCycles%.2f.tsv", 
    subjectsAndComponents=NULL,
    scFilenamePattern=
     "data/subjectsAndComponents/subjectsAndComponentsClusterID%02d.tsv",
    apERPImagesFilenamePattern=
     "results/rERPImagesGeneration/apERPImagesClusterID%02dCondition%sSortvars%s%sStim%sC%02d.RData",
    preProcessedPhaseERPIFilenamePattern=
     "results/rERPImagesGeneration/ppPhaseERPImageClusterID%02dCondition%sSortvar%s%sStim%sC%02d.RData", 
    analyzedConditionsFilenamePattern=
     "results/phaseAnalysis/c%02d/analyzedDataClusterID%02dCondition%sSortvars%s%sStim%sC%02dMinSFPD%05dMaxSFPD%05d.RData")

rm(processAll)
