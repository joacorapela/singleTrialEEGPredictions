getPeakCoefInfo <- function(analyzedData, srate, lowpassFilterOrder, margin,
                                          minPeakTime, onlySignificantPeaks,
                                          factorPeakITC=3, plot=FALSE) {
    findContingousTimeRegions <- function(times, srate) {
        continuousTimeRegions <- c()
        timeStep <- 1000/srate
        startTime = times[1]
        for(i in 2:length(times)) {
            if(times[i]!=times[i-1]+timeStep) {
                continuousTimeRegions <- rbind(continuousTimeRegions,
                                                c(startTime, times[i-1]))
                startTime = times[i]
            }
        }
        continuousTimeRegions <- rbind(continuousTimeRegions, 
                                        c(startTime, times[length(times)]))
        return(continuousTimeRegions)
    }
    getPeaksCoefsInfoForContigousTime <- function(times, 
                                                   coefs,
                                                   peakITCFreq, 
                                                   factorPeakITC,
                                                   srate,
                                                   filterOrderButter,
                                                   filterOrderFIR,
                                                   margin) {
        peakTimes <- c()
        peakValues <- c()
        n <- length(coefs)
        firDelay <- (filterOrderFIR-1)/2
        if(n>firDelay) {
            lowPassCutoff <- factorPeakITC*peakITCFreq
            lowPassCutoffSignal <- 2*lowPassCutoff/srate
            aFilter <- fir1(n=filterOrderFIR, w=lowPassCutoffSignal, type="low")
#             aFilter <- butter(n=filterOrderButter, W=lowPassCutoffSignal, type = 'low', plane='z')
            filteredCoefs <- signal:::filter(filt=aFilter, x=coefs)
            # Trick to avoid introduction of delay in filteredCoefs
            # http://blog.prosig.com/2001/06/06/removing-phase-delay-using-phaseless-filtering/
#             filteredCoefs <- signal:::filter(filt=aFilter, x=filteredCoefs[n:1])
#             filteredCoefs <- filteredCoefs[n:1]
            firPadValue <- filteredCoefs[length(filteredCoefs)]
            filteredCoefs <- c(filteredCoefs[-(1:firDelay)], 
                                rep(firPadValue, times=firDelay))
                                                        
            if(length(times)>2*margin) {
                i <- which(filteredCoefs[(1+margin):(n-2-margin)]<=
                            filteredCoefs[(2+margin):(n-1-margin)]&
                           filteredCoefs[(2+margin):(n-1-margin)]>=
                             filteredCoefs[(3+margin):(n-margin)])
                if(length(i)>0) {
                    peakTimes <- times[i+1+margin]
                    peakValues <- coefs[i+1+margin]
                }
                if(plot) {
                    X11()
                    plot(times, coefs, xlab="Time (ms)", 
                                ylab="Coefficient Value")
                    points(times, filteredCoefs, col="red")
                    abline(v=peakTimes, col="green")
                }
            }
        }
        return(list(peakTimes=peakTimes, peakValues=peakValues))
    }
    getPeaksAndValleysCoefsInfoForContigousTime <- 
     function(times, coefs, peakITCFreq, srate, margin, filterOrderButter=2,
                     filterOrderFIR=11) {
        peakTimes <- c()
        peakAmplitudes <- c()
        valleyTimes <- c()
        valleyAmplitudes <- c()

        res <- getPeaksCoefsInfoForContigousTime(times=times, 
                                                  coefs=coefs,
                                                  peakITCFreq=peakITCFreq,
                                                  factorPeakITC=factorPeakITC,
                                                  srate=srate,
                                                  filterOrderButter=
                                                   filterOrderButter,
                                                  filterOrderFIR=
                                                   filterOrderFIR,
                                                  margin=margin)
        if(length(res$peakTimes)>0) {
            peakTimes <- res$peakTimes
            peakAmplitudes <- res$peakValues
        }
        res <- getPeaksCoefsInfoForContigousTime(times=times, 
                                                  coefs=-coefs,
                                                  peakITCFreq=peakITCFreq,
                                                  factorPeakITC=factorPeakITC,
                                                  srate=srate,
                                                  filterOrderButter=
                                                   filterOrderButter,
                                                  filterOrderFIR=
                                                   filterOrderFIR,
                                                  margin=margin)
        if(length(res$peakTimes)>0) {
            valleyTimes <- res$peakTimes
            valleyAmplitudes <- -res$peakValues
        }
        return(list(peakTimes=peakTimes,
                     peakAmplitudes=peakAmplitudes,
                     valleyTimes=valleyTimes,
                     valleyAmplitudes=valleyAmplitudes))
    }
    getPositiveAndNegativePeaks <- function(times, nonConstantCoefs, 
                                                   peakITCFreq, srate, margin) {
        positivePeaksTimes <- c()
        negativePeaksTimes <- c()
        positivePeaksAmplitudes <- c()
        negativePeaksAmplitudes <- c()
        contiguousTimeRegions <-
         findContingousTimeRegions(times=times, srate=srate)
        for(i in 1:nrow(contiguousTimeRegions)) {
            contigousIndices <- 
             which(contiguousTimeRegions[i,1]<=times & 
                    times<=contiguousTimeRegions[i,2])
            contigousCoefs <- nonConstantCoefs[contigousIndices]
            contiguousTimes <- times[contigousIndices]
            res <- getPeaksAndValleysCoefsInfoForContigousTime(
                    times=contiguousTimes,
                    coefs=contigousCoefs,
                    peakITCFreq=peakITCFreq,
                    srate=srate,
                    margin=margin)
            positivePeaksIndices <- which(res$peakAmplitudes>0)
            if(length(positivePeaksIndices)>0) {
                positivePeaksTimes <- 
                 c(positivePeaksTimes, 
                   res$peakTimes[positivePeaksIndices])
                positivePeaksAmplitudes <- 
                 c(positivePeaksAmplitudes, 
                    res$peakAmplitudes[positivePeaksIndices])
            }
            negativePeaksIndices <- which(res$valleyAmplitudes<0)
            if(length(negativePeaksIndices)>0) {
                negativePeaksTimes <- 
                 c(negativePeaksTimes,
                    res$valleyTimes[negativePeaksIndices])
                negativePeaksAmplitudes <- 
                 c(negativePeaksAmplitudes,
                    res$valleyAmplitudes[negativePeaksIndices])
            }
        }
        return(list(positivePeaksTimes=positivePeaksTimes,
                     negativePeaksTimes=negativePeaksTimes,
                     positivePeaksAmplitudes=positivePeaksAmplitudes,
                     negativePeaksAmplitudes=negativePeaksAmplitudes))
    }
    removeEarlyPeaks <- function(positivePeaksTimes, positivePeaksAmplitudes,
                                                    negativePeaksTimes,
                                                    negativePeaksAmplitudes,
                                                    minPeakTime) {
        positivePeaksIndicesToRemove <- which(positivePeaksTimes<minPeakTime)
        if(length(positivePeaksIndicesToRemove)>0) {
            positivePeaksTimes <- 
             positivePeaksTimes[-positivePeaksIndicesToRemove]
            positivePeaksAmplitudes <- 
             positivePeaksAmplitudes[-positivePeaksIndicesToRemove]
        }
        negativePeaksIndicesToRemove <- which(negativePeaksTimes<minPeakTime)
        if(length(negativePeaksIndicesToRemove)>0) {
            negativePeaksTimes <- 
             negativePeaksTimes[-negativePeaksIndicesToRemove]
            negativePeaksAmplitudes <- 
             negativePeaksAmplitudes[-negativePeaksIndicesToRemove]
        }
        return(list(positivePeaksTimes=positivePeaksTimes,
                     positivePeaksAmplitudes=positivePeaksAmplitudes,
                     negativePeaksTimes=negativePeaksTimes,
                     negativePeaksAmplitudes=negativePeaksAmplitudes))
    }
    removeUnsiginficantPeaks <- function(times,
                                          nonConstantCoefsCIs, 
                                          positivePeaksTimes,
                                          positivePeaksAmplitudes,
                                          negativePeaksTimes,
                                          negativePeaksAmplitudes) {
        significantIndices <- which(nonConstantCoefsCIs[,2]*
                                     nonConstantCoefsCIs[,3]>0)
        significantTimes <- times[significantIndices]
        significantPositivePeaksIndices <-
         which(!is.na(match(positivePeaksTimes, significantTimes)))
        significantNegativePeaksIndices <-
         which(!is.na(match(negativePeaksTimes, significantTimes)))
        return(list(positivePeaksTimes=
                      positivePeaksTimes[significantPositivePeaksIndices],
                     positivePeaksAmplitudes=
                      positivePeaksAmplitudes[significantPositivePeaksIndices],
                     negativePeaksTimes=
                        negativePeaksTimes[significantNegativePeaksIndices],
                     negativePeaksAmplitudes=
                      negativePeaksAmplitudes[significantNegativePeaksIndices]))
    }
    getLargestPeaks <- function(positivePeaksTimes, 
                                 positivePeaksAmplitudes,
                                 negativePeaksTimes,
                                 negativePeaksAmplitudes) {
        largestPositivePeakTime <- NaN
        largestPositivePeakAmplitude <- NaN
        largestNegativePeakTime <- NaN
        largestNegativePeakAmplitude <- NaN
        largestAbsPeakAmplitude <- NaN
        largestAbsPeakTime <- NaN

        if(length(positivePeaksAmplitudes)>0) {
            index <- which.max(positivePeaksAmplitudes)
            largestPositivePeakTime <- positivePeaksTimes[index]
            largestPositivePeakAmplitude <- positivePeaksAmplitudes[index]
        }
        if(length(negativePeaksAmplitudes)>0) {
            index <- which.min(negativePeaksAmplitudes)
            largestNegativePeakTime <- negativePeaksTimes[index]
            largestNegativePeakAmplitude <- negativePeaksAmplitudes[index]
        }
        if(!is.nan(largestPositivePeakAmplitude)>0 &
           !is.nan(largestNegativePeakAmplitude)>0) {
            whichMaxRes <- which.max(c(abs(largestPositivePeakAmplitude),
                                        abs(largestNegativePeakAmplitude)))
            if(whichMaxRes==1) {
                largestAbsPeakAmplitude <- abs(largestPositivePeakAmplitude)
                largestAbsPeakTime <- largestPositivePeakTime
            } else {
                largestAbsPeakAmplitude <- abs(largestNegativePeakAmplitude)
                largestAbsPeakTime <- largestNegativePeakTime
            }
        } else {
            if(!is.nan(largestPositivePeakAmplitude)>0) {
                largestAbsPeakAmplitude <- abs(largestPositivePeakAmplitude)
                largestAbsPeakTime <- largestPositivePeakTime
            } else {
                if(!is.nan(largestNegativePeakAmplitude)>0) {
                    largestAbsPeakAmplitude <- abs(largestNegativePeakAmplitude)
                    largestAbsPeakTime <- largestNegativePeakTime
                }
            }
        }
        return(list(largestPositivePeakTime=largestPositivePeakTime,
                     largestPositivePeakAmplitude=largestPositivePeakAmplitude,
                     largestNegativePeakTime=largestNegativePeakTime,
                     largestNegativePeakAmplitude=largestNegativePeakAmplitude,
                     largestAbsPeakTime=largestAbsPeakTime,
                     largestAbsPeakAmplitude=largestAbsPeakAmplitude))

    }

    largestPositivePeakAmplitude <- NaN
    largestNegativePeakAmplitude <- NaN
    largestAbsPeakAmplitude <- NaN
    largestPositivePeakTime <- NaN
    largestNegativePeakTime <- NaN
    largestAbsPeakTime <- NaN
    if(!is.null(analyzedData$coefsCIs)) {
        res <- getPositiveAndNegativePeaks(times=analyzedData$times,
                                            nonConstantCoefs=
                                             analyzedData$coefsCIs[-1,1],
                                            peakITCFreq=analyzedData$peakITCFreq,
                                            srate=srate, margin=margin)
        res <- removeEarlyPeaks(positivePeaksTimes=res$positivePeaksTimes,
                                 positivePeaksAmplitudes=res$positivePeaksAmplitudes,
                                 negativePeaksTimes=res$negativePeaksTimes,
                                 negativePeaksAmplitudes=res$negativePeaksAmplitudes,
                                 minPeakTime=minPeakTime)
        if(onlySignificantPeaks) {
            res <-
             removeUnsiginficantPeaks(times=analyzedData$times,
                                       nonConstantCoefsCIs=
                                        analyzedData$coefsCIs[-1,],
                                       positivePeaksAmplitudes=
                                        res$positivePeaksAmplitudes,
                                       positivePeaksTimes=
                                        res$positivePeaksTimes,
                                       negativePeaksAmplitudes=
                                        res$negativePeaksAmplitudes,
                                       negativePeaksTimes=
                                        res$negativePeaksTimes)
        }
        res <- getLargestPeaks(positivePeaksTimes=res$positivePeaksTimes,
                                positivePeaksAmplitudes=res$positivePeaksAmplitudes,
                                negativePeaksTimes=res$negativePeaksTimes,
                                negativePeaksAmplitudes=res$negativePeaksAmplitudes)
        largestPositivePeakAmplitude <- res$largestPositivePeakAmplitude
        largestNegativePeakAmplitude <- res$largestNegativePeakAmplitude
        largestAbsPeakAmplitude <- res$largestAbsPeakAmplitude
        largestPositivePeakTime <- res$largestPositivePeakTime
        largestNegativePeakTime <- res$largestNegativePeakTime
        largestAbsPeakTime <- res$largestAbsPeakTime
    }
    return(list(positivePeakAmplitude=largestPositivePeakAmplitude,
                 negativePeakAmplitude=largestNegativePeakAmplitude,
                 absPeakAmplitude=largestAbsPeakAmplitude,
                 positivePeakTime=largestPositivePeakTime,
                 negativePeakTime=largestNegativePeakTime,
                 absPeakTime=largestAbsPeakTime))
}
