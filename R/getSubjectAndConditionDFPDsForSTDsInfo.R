getSubjectAndConditionDFPDsForSTDsInfo <- function(subjectName, 
                                                    modality,
                                                    condition, 
                                                    dfpdsForSTDsInfo) {
    for(i in 1:length(dfpdsForSTDsInfo)) {
        if(dfpdsForSTDsInfo[[i]]$subjectName==subjectName) {
            dfpdsForSTDsInfoForModalities <- 
             dfpdsForSTDsInfo[[i]]$dfpdsForSTDsInfo
            return(getModalityDFPDsForSTDs(modality=modality,
                                            condition=condition,
                                            dfpdsForSTDsInfoForModalities=
                                             dfpdsForSTDsInfoForModalities))
        }
    }
    stop(sprintf("Subject %s not found", subjectName))
}
getModalityDFPDsForSTDs <- function(modality, condition,
                                              dfpdsForSTDsInfoForModalities) {
    for(i in 1:length(dfpdsForSTDsInfoForModalities)) {
        if(dfpdsForSTDsInfoForModalities[[i]]$modality==modality) {
            dfpdsForSTDsInfoForConditions <- 
             dfpdsForSTDsInfoForModalities[[i]]$dfpdsForSTDsInfo
            return(getConditionDFPDsForSTDs(condition=condition,
                                             dfpdsForSTDsInfoForConditions=
                                              dfpdsForSTDsInfoForConditions))
        }
    }
    stop(sprintf("Modality %s not found", modality))
}
getConditionDFPDsForSTDs <- function(condition, dfpdsForSTDsInfoForConditions) {
    for(i in 1:length(dfpdsForSTDsInfoForConditions)) {
        if(dfpdsForSTDsInfoForConditions[[i]]$condition==condition) {
            return(dfpdsForSTDsInfoForConditions[[i]]$dfpdsForSTDsInfo)
        }
    }
    stop(sprintf("Condition %s not found", condition))
}
