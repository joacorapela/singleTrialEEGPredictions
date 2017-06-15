getSubjectAndComponentCases <- function(subjectName, component,
                                                     subjectsNames,
                                                     components) {
    subjectAndComponentCases <- which(subjectsNames==subjectName & 
                                       components==component)
    return(subjectAndComponentCases)
}
