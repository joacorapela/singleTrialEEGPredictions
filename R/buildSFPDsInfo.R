
buildSFPDsInfo <- function(subjectsNames, modalities, conditions, 
                                          sfpdsInfoFilenamePattern) {
    sfpdsInfo <- list()
    for(subjectName in subjectsNames) {
        show(sprintf('Processing subject %s', subjectName))
        subjectSFPDsInfo <- buildSubjectSFPDsInfo(
                             subjectName=subjectName,
                             modalities=modalities,
                             conditions=conditions, 
                             sfpdsInfoFilenamePattern=sfpdsInfoFilenamePattern)
        sfpdsInfo <- c(sfpdsInfo, list(list(subjectName=subjectName, 
                                             sfpdsInfo=subjectSFPDsInfo)))
    }
    return(sfpdsInfo)
}

buildSubjectSFPDsInfo <- function(subjectName, modalities, conditions, 
                                               sfpdsInfoFilenamePattern) {
    sfpdsInfo <- list()
    for(modality in modalities) {
        show(sprintf('Processing modality %s', modality))
        modalitySFPDsInfo <- buildModalitySFPDsInfo(
                              subjectName=subjectName, 
                              modality=modality,
                              conditions=conditions,
                              sfpdsInfoFilenamePattern=
                               sfpdsInfoFilenamePattern)
        sfpdsInfo <- c(sfpdsInfo, list(list(modality=modality, 
                                             sfpdsInfo=modalitySFPDsInfo)))
    }
    return(sfpdsInfo)
}

buildModalitySFPDsInfo <- function(subjectName, modality, conditions, 
                                                sfpdsInfoFilenamePattern) {
    sfpdsInfo <- list()
    for(condition in conditions) {
        show(sprintf('Processing condition %s', condition))
        conditionSFPDsInfo <- buildConditionSFPDsInfo(
                               subjectName=subjectName, 
                               modality=modality,
                               condition=condition,
                               sfpdsInfoFilenamePattern=
                                sfpdsInfoFilenamePattern)
        sfpdsInfo <- c(sfpdsInfo, list(list(condition=condition, 
                                             sfpdsInfo=conditionSFPDsInfo)))
    }
    return(sfpdsInfo)
}

buildConditionSFPDsInfo <- function(subjectName, modality, condition, 
                                                 sfpdsInfoFilenamePattern) {
    sfpdsInfoFilename <- sprintf(sfpdsInfoFilenamePattern, subjectName,
                                                           modality,
                                                           condition)
    sfpdsInfo <- readVectorDoubleWithLengthHeader(filename=sfpdsInfoFilename)
    sfpdsInfo <- matrix(sfpdsInfo, ncol=2)
    return(sfpdsInfo)
}
