
source("doLoadSources.R")

processAll <- function(subjectsNames, modalities, conditions, 
                                      sfpdsInfoFilenamePattern, 
                                      firstDsAfterSTDsInfo,
                                      saveFilename) {
    sfpdsInfo <- buildSFPDsInfo(subjectsNames=subjectsNames, 
                                 modalities=modalities,
                                 conditions=conditions, 
                                 sfpdsInfoFilenamePattern=
                                  sfpdsInfoFilenamePattern)
    save(sfpdsInfo, file=saveFilename)
}

processAll(
 subjectsNames=c("av101a", "av107a", "av115a", "av122a", "av1039a", "av108a", "av116a", "av124a", "av1040a", "av109a", "av118a", "av128a", "av1042b", "av112a", "av120a", "av130a", "av105a", "av113a", "av121a"),
 modalities=c("Visual"),
 conditions=c("switchvision"),
 sfpdsInfoFilenamePattern="data/sfpds/sfpds%s%s%s.dat",
 saveFilename="results/sfpds/sfpdsInfo.RData")

rm(processAll)
