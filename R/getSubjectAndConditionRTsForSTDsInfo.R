getSubjectAndConditionRTsForSTDsInfo <- function(subjectName, condition, 
                                                              rtsForSTDsInfo) {
    for(i in 1:length(rtsForSTDsInfo)) {
        if(rtsForSTDsInfo[[i]]$subjectName==subjectName) {
            rtsForSTDsInfoForConditions <- rtsForSTDsInfo[[i]]$rtsForSTDsInfo
            return(getConditionRTsForSTDs(condition=condition,
                                           rtsForSTDsInfoForConditions=
                                            rtsForSTDsInfoForConditions))
        }
    }
    stop(sprintf("Subject %s not found", subjectName))
}
getConditionRTsForSTDs <- function(condition, rtsForSTDsInfoForConditions) {
    for(i in 1:length(rtsForSTDsInfoForConditions)) {
        if(rtsForSTDsInfoForConditions[[i]]$condition==condition) {
            return(rtsForSTDsInfoForConditions[[i]]$rtsForSTDsInfo)
        }
    }
    stop(sprintf("Condition %s not found", condition))
}
