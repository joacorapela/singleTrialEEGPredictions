getSubjectAndComponentPeakTimeFreqInfo <- function(subjectName, component, 
                                                                peaksInfo) {
    subjectAndComponentCases <- 
     getSubjectAndComponentCases(subjectName=subjectName,
                                  component=component,
                                  subjectsNames=peaksInfo$subjectsNames,
                                  components=peaksInfo$components)
    if(length(subjectAndComponentCases)==0) {
        stop(sprintf("Subject %s and component %02d not found", 
                     subjectName, component))
    } else {
        if(length(subjectAndComponentCases)>1) {
            stop(sprintf("Multiple matches for subject %s and component %02d", 
                         subjectName, component))
        }
    }
    return(list(time=peaksInfo$peaksTimes[subjectAndComponentCases], 
                 freq=peaksInfo$peaksFreqs[subjectAndComponentCases], 
                 value=peaksInfo$peaksValues[subjectAndComponentCases]))
}
