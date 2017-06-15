# Call example
# listOfItems=stdsNextDsLatencies
# listFieldName="stdsNextDsLatencies"
# keyFieldNames=c("subjectName", "modality", "condition")
# keyFieldValues=c("av101a", "Visual", "switchvision")

getItemInAVShiftList <- function(listOfItems, listFieldName, 
                                              keyFieldNames, 
                                              keyFieldValues) {
    for(i in 1:length(listOfItems)) {
        if(keyFieldValues[1]==listOfItems[[i]][[keyFieldNames[1]]]) {
            if(length(keyFieldNames)==1) {
                return(listOfItems[[i]][[listFieldName]])
            } else {
                return(getItemInAVShiftList(listOfItems=
                                             listOfItems[[i]][[listFieldName]], 
                                             listFieldName=listFieldName, 
                                             keyFieldNames=keyFieldNames[-1], 
                                             keyFieldValues=keyFieldValues[-1]))
            }
        }
    }
    stop(sprintf("%s %s not found", keyFieldNames[[1]], keyFieldValues[[1]]))
}
