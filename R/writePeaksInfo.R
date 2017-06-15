writePeaksInfo <- function(filename, subjectsNames, components, peaksValues, 
                                     peaksTimes, peaksFreqs, peaksSignificant) {
    con <- file(filename, open="wt")
    for(i in 1:length(subjectsNames)) {
        writeLines(sprintf("%s\t%02d\t%.4f\t%.4f\t%.4f\t%d", subjectsNames[i],
                           components[i],
                           peaksTimes[i], peaksFreqs[i], peaksValues[i],
                           peaksSignificant[i]), 
                    con=con)
    }
    close(con)
}
