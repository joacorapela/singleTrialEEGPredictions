readPeaksInfo <- function(filename) {
    con <- file(filename, open="rt")

    subjectsNames <- c()
    components <- c()
    peaksTimes <- c()
    peaksFreqs <- c()
    peaksValues <- c()

    # line=="<subject name>\t<peak time>\t<peak freq>\t<peak value>"
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        if(substr(line, 1, 1)!='#') {
            splits <- strsplit(line, "\t")
            subjectsNames <- c(subjectsNames, splits[[1]][1])
            components <- c(components, as.numeric(splits[[1]][2]))
            peaksTimes <- c(peaksTimes, as.numeric(splits[[1]][3]))
            peaksFreqs <- c(peaksFreqs, as.numeric(splits[[1]][4]))
            peaksValues <- c(peaksValues, as.numeric(splits[[1]][5]))
        }
    }
    close(con)
    peaksInfo <- data.frame(subjectsNames=subjectsNames, 
                             components=components,
                             peaksTimes=peaksTimes, 
                             peaksFreqs=peaksFreqs, 
                             peaksValues=peaksValues, 
                             stringsAsFactors=FALSE)
    return(peaksInfo)
}
