getOutliersFactor <- function(allIndices, bOutIndices, xOutIndices, 
                                          yOutIndices) {
    # 0: no outlier
    # 1: x oulier
    # 2: y oulier
    # 3: bivariate oulier
    # 4: x and y oulier
    # 5: x and bivariate oulier
    # 6: y and bivariate oulier
    # 7: x and y and bivariate oulier

    isXOut <- allIndices %in% xOutIndices
    isYOut <- allIndices %in% yOutIndices
    isBOut <- allIndices %in% bOutIndices

    values <- rep("noOut", length(allIndices))
    values[isXOut] <- "xOut"
    values[isYOut] <- "yOut"
    values[isBOut] <- "bOut"
    values[isXOut & isYOut] <- "xyOut"
    values[isXOut & isBOut] <- "xbOut"
    values[isYOut & isBOut] <- "ybOut"
    values[isXOut & isYOut & isBOut] <- "xybOut"

    return(factor(x=values))
}
