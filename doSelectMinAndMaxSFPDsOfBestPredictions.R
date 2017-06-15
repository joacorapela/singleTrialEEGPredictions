
source("doLoadSources.R")

selectMinAndMaxSFPDsOfBestPredictionsForModalities(
 modalities=c("Visual"), 
 sortvar="STDsBeforeDsDLT500", 
 clustersIDs=c(04),
 conditions=c("switchvision"),
 scFilenamePattern="data/subjectsAndComponents/subjectsAndComponentsClusterID%02d.tsv",
 minSFPDs=c(0), 
 maxSFPDs=seq(from=1000, to=10000, by=100),
 analyzedDataFilenamePattern=
  "results/phaseAnalysis/c%02d/analyzedDataClusterID%02dCondition%sSortvars%s%sStim%sC%02dMinSFPD%05dMaxSFPD%05d.RData",
 minAndMaxSFPDOfBestPredictionsFilenamePattern=
  "results/phaseAnalysis/c%02d/minAndMaxSFPDsOfBestPredictionsClusterID%02dCondition%sSortvars%s%sStim%sC%02d.txt")
