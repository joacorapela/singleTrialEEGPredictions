
prepareDataRegularizedRegression <- function(erpImage, minSFPD, maxSFPD, 
                                                       lambda, 
                                                       order, 
                                                       scaleData,
                                                       interactions, 
                                                       plotERPImages, 
                                                       averageTrialsWinSize, 
                                                       zlim=NA,
                                                       verbose=FALSE, ...) {
    if(is.null(erpImage$erpImage)) {
        return(erpImage)
    }
    sortedRegressors <- t(erpImage$erpImage)
    regressorsNames <- sprintf('%d', as.integer(erpImage$times))
    colnames(sortedRegressors) <- regressorsNames
# if(fieldExistsInList("sfpds", erpImage)) {
    sfpds <- erpImage$sfpds
# } else {
#     sfpds <- erpImage$cueNTsDelays
# }
    epochEventIDs <- erpImage$epochEventIDs

    # Remove extreme sfpds
    extremeSFPDsIndices <- which(sfpds<minSFPD | sfpds>maxSFPD)
    if(length(extremeSFPDsIndices>0)) {
        sortedRegressors <- sortedRegressors[-extremeSFPDsIndices,]
        sfpds <- sfpds[-extremeSFPDsIndices]
        extremeSFPDS <- epochEventIDs[extremeSFPDsIndices]
        epochEventIDs <- epochEventIDs[-extremeSFPDsIndices]
    } else {
        extremeSFPDS <- c()
    }

    removedCasesIndices <- c(erpImage$removedCasesIndices, 
                              list(extremeSFPDS=extremeSFPDS))
    # done removing extreme sfpds

    if(!is.nan(lambda)) {
        if(lambda==0) {
            sfpdsTransformed <- log(sfpds)
        } else {
            sfpdsTransformed <- 
             (sfpds^lambda-1)/lambda
        }
    } else {
        sfpdsTransformed <- sfpds
    }
    dataMatrix <- buildZeroMemoryDataMatrix(pxs=sortedRegressors,
                                             order=order,
                                             interactions=interactions)
    data <- cbind(sfpdsTransformed, dataMatrix)
    if(scaleData) {
        data <- scale(data)
    }
    return(list(data=data,
                 subjectName=erpImage$subjectName,
                 component=erpImage$component,
                 times=erpImage$times,
                 meanDirection=erpImage$meanDirection,
                 itc=erpImage$itc,
                 peakITCFreq=erpImage$peakITCFreq,
                 sfpds=sfpds,
                 epochEventIDs=epochEventIDs,
                 removedCasesIndices=removedCasesIndices))
}
