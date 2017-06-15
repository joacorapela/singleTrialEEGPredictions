plotRegLMCoefficients <- 
 function(coefsCIs, 
           times, meanDirection, itc,
           constantCoefTime=-20,
           epochStartTime=0,
           epochEndTime=500,
           srate=250,
           xlab="Time (ms)", 
           ylab="Regression Coefficient Value", 
           main="",
           hlinetype="solid",
           hlineCol="#666666",
           meanDirectionCol="black",
           meanDirectionLineType="dashed",
           itcCol="black",
           itcLineType="dotted",
           coefCol="black",
           coefLineType="solid",
           xlim=c(-50, 500),
           ylim=NA,
           timeStep=4,
           xBreaks=c(0, 200, 400),
           interceptErrorBarWidth=20,
           interceptErrorBarLineType="solid",
           interceptErrorBarColour="grey",
           fwpmLinetype="solid",
           fwpmColour="black") {
    scaleCos <- max(abs(coefsCIs[, 1]))
    scaledCosMeanDirection <- scaleCos*cos(meanDirection)
    scaledITC <- scaleCos*itc

    lowCoherenceTimeBlocks <- getLowCoherenceTimesBlocks(times=times,
                                                          epochStartTime=
                                                           epochStartTime, 
                                                          epochEndTime=
                                                           epochEndTime, 
                                                          srate=srate) 
    df <- data.frame(times=times, 
                      coef=coefsCIs[-1,1])
    dfIntercept <- data.frame(x=c(constantCoefTime), 
                               y=c(coefsCIs[1,1]),
                               ymin=c(coefsCIs[1,2]),
                               ymax=c(coefsCIs[1,3]))
    cisDF <- data.frame(times=times,
                         coefs95CIL=coefsCIs[-1,2],
                         coefs95CIU=coefsCIs[-1,3])
    if(length(lowCoherenceTimeBlocks)>1) {
        dfBlocks <- data.frame(xmin=lowCoherenceTimeBlocks[,1],
                                xmax=lowCoherenceTimeBlocks[,2])
    } else {
        dfBlocks <- NA
    }

    p <- ggplot() 
    p <- p + geom_line(data=df, aes(x=times, y=coef), 
                                linetype=coefLineType, col=coefCol) +
             geom_ribbon(data=cisDF, aes(x=times, ymin=coefs95CIL, 
                                                  ymax=coefs95CIU), 
                                     fill="orange", alpha=0.3) +
             geom_point(data=dfIntercept, aes(x=x, y=y)) +
             geom_errorbar(data=dfIntercept, aes(x=x, y=y, ymin=ymin, ymax=ymax), linetype=interceptErrorBarLineType, width=interceptErrorBarWidth, colour=interceptErrorBarColour)
    p <- p + geom_hline(yintercept=0, linetype=hlinetype, colour=hlineCol)
    if(length(dfBlocks)>1) {
        p <- p + geom_rect(data=dfBlocks, aes(xmin=xmin, xmax=xmax,  ymin=-Inf,
                                                         ymax=+Inf), 
                                          alpha=1.0, fill="gray")
    }
    p <- p + labs(fill="", colour="", linetype="") +
             xlab(xlab) + 
             ylab(ylab) + 
             ggtitle(main) +
             scale_x_continuous(limits=xlim, breaks=xBreaks)
    if(!is.na(ylim[1])) {
        p <- p + ylim(ylim)
    }


    print(p)
}
