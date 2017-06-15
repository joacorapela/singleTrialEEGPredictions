getOutliersIndices <- function(x, y, STAND=TRUE, ...) {
    bOutIndices <- tryCatch({
        sink("/dev/null")
        # Fixing a problem in outpro when median(values)==0
        if(median(x)==0 || median(y)==0) {
            STAND=FALSE
        }
        #
        outpro(cbind(x, y), plotit=FALSE, STAND=STAND, ...)$out.id
    }, finally={
        sink()
    })
    xOutIndices <- adjboxout(x)$out.id
    yOutIndices <- adjboxout(y)$out.id
    allOutIndices <- unique(c(bOutIndices, xOutIndices, yOutIndices))
    return(list(bivariateOutliersIndices=bOutIndices, 
                 xOutliersIndices=xOutIndices,
                 yOutliersIndices=yOutIndices,
                 allOutliersIndices=allOutIndices))
}
