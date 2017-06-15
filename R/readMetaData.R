# meta data format: frames,trials,srate

readMetaData <- function(filename) {
    buffer <- readVectorDouble(filename=filename, n=3)
    return(list(nFrames=buffer[1], nTrials=buffer[2], srate=buffer[3]))
}
