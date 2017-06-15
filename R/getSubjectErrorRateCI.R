getSubjectErrorRateCI <- function(subjectName, condition, stats) {
    for(i in 1:length(stats)) {
        if(subjectName==stats[[i]]$subjectName) {
            return(getConditionErrorRateCI(condition=condition,
                                           stats=stats[[i]]$stats))
        }
    }
    stop(sprintf("Could not find subject %s", subjectName))
}
getConditionErrorRateCI <- function(condition, stats) {
    for(i in 1:length(stats)) {
        if(condition==stats[[i]]$condition) {
            return(stats[[i]]$stats$errorRateCI)
        }
    }
    stop(sprintf("Could not find condition %s", condition))
}
