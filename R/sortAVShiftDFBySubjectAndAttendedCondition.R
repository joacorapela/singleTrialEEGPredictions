sortAVShiftDFBySubjectAndAttendedCondition <- function(df) {
    subjectsNames <- sort(unique(df$subjectName))
    sortIndices <- c()
    for(subjectName in subjectsNames) {
        sortIndices <- c(sortIndices, getModalityAndConditionIndicesForSubject(
                                       subjectName=subjectName, 
                                       modality="Visual",
                                       condition="switchvision",
                                       df=df))
        sortIndices <- c(sortIndices, getModalityAndConditionIndicesForSubject(
                                       subjectName=subjectName, 
                                       modality="Visual",
                                       condition="switchaudition",
                                       df=df))
        sortIndices <- c(sortIndices, getModalityAndConditionIndicesForSubject(
                                       subjectName=subjectName, 
                                       modality="Auditory",
                                       condition="switchaudition",
                                       df=df))
        sortIndices <- c(sortIndices, getModalityAndConditionIndicesForSubject(
                                       subjectName=subjectName, 
                                       modality="Auditory",
                                       condition="switchvision",
                                       df=df))
    }
    return(df[sortIndices,])
}

getModalityAndConditionIndicesForSubject <- function(subjectName, modality,
                                                                  condition,
                                                                  df) {
    indices <- which(df$subjectName==subjectName & df$modality==modality & 
                                                   df$condition==condition)
    return(indices)
}

