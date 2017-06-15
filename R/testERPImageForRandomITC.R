testERPImageForRandomITC <- function(phaseERPImage) {
    pValues <- c()
    for(frame in 1:nrow(phaseERPImage)) {
        pValues[frame] <- 
         rayleighUniformityTest(phaseERPImage[frame,])
#          rayleigh.test(x=circular(phaseERPImage[frame,]))$p.value
    }
    return(pValues)
}
