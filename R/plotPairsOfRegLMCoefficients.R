plotPairsOfRegLMCoefficients <- 
 function(times1, coefsCIs1, times2, coefsCIs2,
           constantCoefTime=-20,
           epochStartTime=0,
           epochEndTime=500,
           srate=250,
           coefs1ScaleLabel="Attended",
           coefs2ScaleLabel="Unattended",
           xlab="Time (ms)", 
           ylab="Regression Coefficient Value", 
           main="",
           hlinetype="solid",
           hlineCol="#666666",
           coefs1Col="red",
           coefs2Col="blue",
           coefs1LineType="solid",
           coefs2LineType="solid",
           confidenceBand1Col="red",
           confidenceBand2Col="blue",
           alphaConfidenceBand1=0.3,
           alphaConfidenceBand2=0.3,
           xlim=c(-50, 500),
           timeStep=4,
           xBreaks=c(0, 200, 400)) {
    coefs1DF <- data.frame(times=times1, coefs1=coefsCIs1[-1,1])
    coefs2DF <- data.frame(times=times2, coefs2=coefsCIs2[-1,1])
    coefsDFM <- rbind(melt(coefs1DF, id="times"), melt(coefs2DF, id="times"))
    coefsCIs1DF <- data.frame(times=times1, coefs95CIL=coefsCIs1[-1,2],
                                            coefs95CIU=coefsCIs1[-1,3])
    coefsCIs2DF <- data.frame(times=times2, coefs95CIL=coefsCIs2[-1,2],
                                            coefs95CIU=coefsCIs2[-1,3])
    p <- ggplot() 
    p <- p + geom_line(data=coefsDFM, aes(x=times, y=value, colour=variable, linetype=variable))
    p <- p + geom_ribbon(data=coefsCIs1DF, aes(x=times, ymin=coefs95CIL, ymax=coefs95CIU), fill=confidenceBand1Col, alpha=alphaConfidenceBand1)
    p <- p + geom_ribbon(data=coefsCIs2DF, aes(x=times, ymin=coefs95CIL, ymax=coefs95CIU), fill=confidenceBand2Col, alpha=alphaConfidenceBand2)
    p <- p + scale_colour_manual(values=c(coefs1Col, coefs2Col), breaks=c("coefs1", "coefs2"), labels=c(coefs1ScaleLabel, coefs2ScaleLabel))
    p <- p + scale_linetype_manual(values=c(coefs1LineType, coefs2LineType), breaks=c("coefs1", "coefs2"), labels=c(coefs1ScaleLabel, coefs2ScaleLabel))
#     p <- p + scale_colour_manual(values=c(coefs1Col, coefs2Col), labels=c(coefs1ScaleLabel, coefs2ScaleLabel))
#              scale_linetype_manual(values=c(meanDirectionLineType, itcLineType, coefLineType), breaks=c("scaledCosMeanDirection", "scaledITC", "coef"), labels=c(expression(cos(bar(phi))), "ITC", "regression"))
    p <- p + geom_hline(yintercept=0, linetype=hlinetype, colour=hlineCol)
    p <- p + labs(fill="", colour="", linetype="") +
             xlab(xlab) + 
             ylab(ylab) + 
             ggtitle(main) +
             scale_x_continuous(limits=xlim, breaks=xBreaks)


    print(p)
}
