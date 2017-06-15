
getPlotDipoles <- function(dipolesPositions, dipolesColorScales,
                                          circleRadius=85,
                                          scaleLowColor="blue",
                                          scaleMidColor="white",
                                          scaleHighColor="red",
                                          scaleLimit=c(0.05, .35),
                                          scaleName="Cor.\nCoef.",
                                          pointSize=5,
                                          titleSize=14,
                                          guide=FALSE,
                                          main=NA) {
    getCircXYCoords <- function(xCenter, yCenter, radius, nPoints=200) {
        angles <- seq(from=0, to=2*pi, length=nPoints)
        xCoords <- xCenter + radius*cos(angles)
        yCoords <- yCenter + radius*sin(angles)
        return(list(xCoords=xCoords, yCoords=yCoords))
    }
    getNoseXYCoords <- function() {
        Nose.left = structure(list(x = c(-0.165709488227992, 
            -0.170777055719452, -0.160641920736532, -0.120101380804849, 
            -0.0542230034158648, 0), y = c(0.991010207442792, 
            1.00031714102429, 1.02885836681128, 1.06310783775568, 
            1.13731502480188, 1.16)), .Names = c("x", "y"), row.names = 2:7, 
            class = "data.frame")
        Nose.right = structure(list(x = c(0.165709488227992, 
            0.170777055719452, 0.160641920736532, 0.120101380804849, 
            0.0542230034158648, 0), y = c(0.991010207442792, 
            1.00031714102429, 1.02885836681128, 1.06310783775568, 
            1.13731502480188, 1.16)), .Names = c("x", "y"), row.names = 2:7, 
            class = "data.frame")
        return(list(noseLeft=Nose.left, noseRight=Nose.right))
    }

    circXYCoords <- getCircXYCoords(xCenter=0, yCenter=0, radius=1)
    noseXYCoords <- getNoseXYCoords()
    noseTriangleXYCoords <- data.frame(x=c(-.2, 0, .2), y=c(.95, 1.2, .95))
    polygonDF <- data.frame(x=circXYCoords$xCoords, y=circXYCoords$yCoords)
    rhoDF <- data.frame(x=dipolesPositions$x/circleRadius,
                         y=dipolesPositions$y/circleRadius,
                         cor.coef.=dipolesColorScales)
    p <- ggplot()
    p <- p + geom_polygon(data=polygonDF, 
                           mapping=aes(x=x, y=y), 
                           show_guide=FALSE,
                           colour="grey30", 
                           fill="grey30")
    p <- p + geom_polygon(data=noseTriangleXYCoords, mapping=aes(x=x, y=y),
                                                     show_guide=FALSE,
                                                     colour="grey30", 
                                                     fill="grey30")
#     p <- p + geom_line(data=noseXYCoords$noseLeft, mapping=aes(x=x, y=y), show_guide=FALSE, colour="black", fill="grey30")
#     p <- p + geom_line(data=noseXYCoords$noseRight, mapping=aes(x=x, y=y), show_guide=FALSE, colour="black", fill="grey30")
    p <- p + geom_point(data=rhoDF, mapping=aes(x=x, y=y, colour=cor.coef.),
                                    size=pointSize)
    p <- p + coord_equal()
#     p <- p + scale_colour_gradient2(low=scaleLowColor, mid=scaleMidColor, high=scaleHighColor, limit=scaleLimit, name=scaleName)
    p <- p + scale_colour_gradient2(low=scaleLowColor, 
                                     mid=scaleMidColor, 
                                     high=scaleHighColor, 
                                     midpoint=(scaleLimit[1]+scaleLimit[2])/2, 
                                     limit=scaleLimit, name=scaleName, 
                                     guide=guide)
#     p <- p + scale_colour_gradient(low=scaleLowColor, high=scaleHighColor) +
#     p <- p + scale_colour_gradient2(limit=scaleLimit, name=scaleName) +
#     p <- p + scale_colour_gradient2() +
    p <- p + theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
#                   legend.position="none",
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.background=element_blank(),
                  plot.title=element_text(size=titleSize))
    if(!is.na(main)) {
        p <- p + ggtitle(main)
    }
    return(p)
}
