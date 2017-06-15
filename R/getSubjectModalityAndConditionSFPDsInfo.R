getSubjectModalityAndConditionSFPDsInfo <- function(subjectName, 
                                                    modality, 
                                                    condition,
                                                    sfpdsInfoForSubjects) {
    for(i in 1:length(sfpdsInfoForSubjects)) {
        if(sfpdsInfoForSubjects[[i]]$subjectName==subjectName) {
            sfpdsInfoForModalities <- sfpdsInfoForSubjects[[i]]$sfpdsInfo
            return(getModalityAndConditionSFPDsInfo(modality=modality,
                                                    condition=condition,
                                                    sfpdsInfoForModalities=
                                                     sfpdsInfoForModalities))
        }
    }
    stop(sprintf("Subject %s not found", subjectName))
}
getModalityAndConditionSFPDsInfo <- function(modality, condition, 
                                                      sfpdsInfoForModalities) {
    for(i in 1:length(sfpdsInfoForModalities)) {
        if(sfpdsInfoForModalities[[i]]$modality==modality) {
            sfpdsInfoForConditions <- sfpdsInfoForModalities[[i]]$sfpdsInfo
            return(getConditionSFPDsInfo(condition=condition,
                                         sfpdsInfoForConditions=
                                          sfpdsInfoForConditions))
        }
    }
    stop(sprintf("Modality %s not found", modality))
}
getConditionSFPDsInfo <- function(condition, sfpdsInfoForConditions) {
    for(i in 1:length(sfpdsInfoForConditions)) {
        if(sfpdsInfoForConditions[[i]]$condition==condition) {
            return(sfpdsInfoForConditions[[i]]$sfpdsInfo)
        }
    }
    stop(sprintf("Condition %s not found", condition))
}
