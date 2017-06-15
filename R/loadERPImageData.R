loadERPImageData <- function(filenamesPattern) {
    metaDataFilename <- sprintf(filenamesPattern, "metaData")
    dataFilename <- sprintf(filenamesPattern, "data")
    timesFilename <- sprintf(filenamesPattern, "times")
#     cueNTsDelaysFilename <- sprintf(filenamesPattern, "sortvar")
    subjectsNamesFilename <- sprintf(filenamesPattern, "subjectsNames")
    componentsFilename <- sprintf(filenamesPattern, "components")
#     targetsUREventsFilename <- sprintf(filenamesPattern, "targetsUREvents")
#     cueTsDelaysFilename <- sprintf(filenamesPattern, "targetsLatencies")
    epochEventIDsFilename <- sprintf(filenamesPattern, "epochEventIDs")

    metaData <- readMetaData(filename=metaDataFilename)
    data <- readVectorDouble(filename=dataFilename,
                                 n=metaData$nFrames*metaData$nTrials)
    data <- matrix(data, nrow=metaData$nFrames)
    times <- readVectorDouble(filename=timesFilename, n=metaData$nFrames)
#     cueNTsDelays <- readVectorDouble(filename=cueNTsDelaysFilename, 
#                                    n=metaData$nTrials)
    subjectsNames <- readStringsFromTextFile(filename=subjectsNamesFilename)
    components <- as.integer(readVectorDouble(filename=componentsFilename, 
                                               n=metaData$nTrials))
#     targetsUREvents <- readVectorDouble(filename=targetsUREventsFilename, 
#                                          n=metaData$nTrials)
#     cueTsDelays <- readVectorDouble(filename=cueTsDelaysFilename, 
#                                           n=metaData$nTrials)
    epochEventIDs <- readVectorDouble(filename=epochEventIDsFilename, 
                                       n=metaData$nTrials)
    return(list(metaData=metaData, data=data, times=times, 
#                                    cueNTsDelays=cueNTsDelays, 
                                   subjectsNames=subjectsNames,
                                   components=components,
#                                    targetsUREvents=targetsUREvents,
#                                    cueTsDelays=cueTsDelays,
                                   epochEventIDs=epochEventIDs))
}
