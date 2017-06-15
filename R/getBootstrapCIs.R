getBootstrapCIs <- function(bootRes, conf, type=c("perc"),
                                     typeListName="percent") {
    allEqual <- function(values, varThreshold=.Machine$double.eps) {
        return(var(values, na.rm=TRUE)<varThreshold)
    }
    cis <- c()
    for(i in 1:length(bootRes$t0)) {
        if((length(bootRes$t0)==1 && !allEqual(bootRes$t)) || 
           !allEqual(bootRes$t[,i])) {
            bootCIRes <- boot.ci(bootRes, type = type, conf=conf, index=i)
            cis <- rbind(cis, c(bootCIRes$t0, bootCIRes[[typeListName]][4:5]))
        } else {
            cis <- rbind(cis, c(bootRes$t0[i], c(NA, NA)))
        }
    }
    return(cis)
}
