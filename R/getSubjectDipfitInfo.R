
getSubjectDipfitInfo <- function(dipfitInfoFilename) {
    dipfitInfoVector <- readVectorDoubleWithLengthHeader(filename=
                                                          dipfitInfoFilename)
    dipfitInfoMatrix <- matrix(dipfitInfoVector, nrow=7)
    componentsDipfitInfo <- c()
    for(i in 1:ncol(dipfitInfoMatrix)) {
        componentDipfitInfo <- list(component=i, 
                                     posxyz=c(dipfitInfoMatrix[1,i], 
                                               
                                               dipfitInfoMatrix[2,i], 
                                               dipfitInfoMatrix[3,i]),
                                     momxyz=c(dipfitInfoMatrix[3+1,i], 
                                               dipfitInfoMatrix[3+2,i], 
                                               dipfitInfoMatrix[3+3,i]),
                                     rv=dipfitInfoMatrix[6+1,i])
        componentsDipfitInfo <- c(componentsDipfitInfo,
                                   list(componentDipfitInfo))
    }
    return(componentsDipfitInfo)
}
