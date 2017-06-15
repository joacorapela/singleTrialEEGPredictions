getSubjectComponents <- function(subjectName, subjectsAndComponents) {
    subjectIndices <- which(subjectsAndComponents[,1]==subjectName)
    subjectComponents <-  as.integer(subjectsAndComponents[subjectIndices, 2])
    return(subjectComponents)
}
