
computeITCFromPhases <- function(phases) {
    itc <- Mod(sum(complex(modulus=1, argument=phases)))/length(phases)
    return(itc)
}
