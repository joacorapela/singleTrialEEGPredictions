selectDFPStats <- function(subjectsDFPStats, subjectName, condition) {
    getSubjectDFPStats <- function(subjectsDFPStats, subjectName) {
        for(i in 1:length(subjectsDFPStats)) {
            if(subjectsDFPStats[[i]]$subjectName==subjectName) {
                return(subjectsDFPStats[[i]]$stats)
            }
        }
        return(NULL)
    }
    getConditionDFPStats <- function(conditionsDFPStats, condition) {
        for(i in 1:length(conditionsDFPStats)) {
            if(conditionsDFPStats[[i]]$condition==condition) {
                return(conditionsDFPStats[[i]]$stats)
            }
        }
        return(NULL)
    }

    conditionsDFPStats <- 
     getSubjectDFPStats(subjectsDFPStats=subjectsDFPStats, 
                         subjectName=subjectName)
    if(is.null(conditionsDFPStats)) {
        return(NULL)
    }
    conditionDFPStats <- getConditionDFPStats(conditionsDFPStats=
                                                conditionsDFPStats, 
                                               condition=condition)
    return(conditionDFPStats)
}
