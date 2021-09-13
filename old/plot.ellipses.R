
# panel.cluster.ellipse <- function( coords.df, rad, ...){

# 	if(!('plot.lab' %in% colnames(coords.df))){
# 		coords.df$plot.lab	 <- coords.df$lab
# 	}
# 	coords.df$plot.lab <- as.character(coords.df$plot.lab)
# 	if(!('circle' %in% colnames(coords.df))){
# 		coords.df$circle <- sapply(coords.df$plot.lab, function(x) return(nchar(x)<3))
# 	}
	
# 	if(any(coords.df$circle == TRUE)){
# 		grid.circle(x = unit(coords.df$x[which(coords.df$circle == TRUE)], "native"), y = unit(coords.df$y[which(coords.df$circle == TRUE)],"native"), r = unit(rad,"inches"), gp=gpar(fill=coords.df$c.col,col="transparent",lwd=2),default.units="native",...)
# 	# grid.rect(y=unit(coords.df$x[which(coords.df$lab==i)], "native"), x=unit(coords.df$y[which(coords.df$lab==i)], "native"), height=unit(2*rad,"inches"), width=unit(2*rad, "inches"),gp=gpar(col="black"))
	
# 	}
# 	if(any(coords.df$circle == FALSE)){
# 		grid.ellipse(x = unit(coords.df$x[which(coords.df$circle == FALSE)],"native"), y = unit(coords.df$y[which(coords.df$circle == FALSE)],"native"), size = rad*1.2, ar=3/5, gp=gpar(fill=coords.df$c.col, col=coords.df$c.col), angle=pi/2, position.units="native",...)
# 	}
# }
panel.cluster.ellipse <- function( coords.df, rad, ...){

	if(!('plot.lab' %in% colnames(coords.df))){
		coords.df$plot.lab	 <- coords.df$lab
	}
	coords.df$plot.lab <- as.character(coords.df$plot.lab)
	if(!('node' %in% colnames(coords.df))){
		coords.df$node <- sapply(coords.df$plot.lab, function(x) if(nchar(x)<3) return('circle') else return('ellipse'))
	}
	
	if(any(coords.df$node == 'circle')){
		grid.circle(x = unit(coords.df$x[which(coords.df$node == 'circle')], "native"), y = unit(coords.df$y[which(coords.df$node == 'circle')],"native"), r = unit(rad,"inches"), gp=gpar(fill=coords.df[which(coords.df$node == 'circle'),]$c.col,col="transparent",lwd=2),default.units="native",...)
	}
	if(any(coords.df$node == 'rect')){
		grid.rect(y=unit(coords.df$x[which(coords.df$node == 'rect')], "native"), x=unit(coords.df$y[which(coords.df$node == 'rect')], "native"), height=unit(2*rad,"inches"), width=unit(2*rad, "inches"),gp=gpar(col="black", fill='white',lwd=1.5, lty="31"))	
	}
	if(any(coords.df$node == 'ellipse')){
		grid.ellipse(x = unit(coords.df$x[which(coords.df$node == 'ellipse')],"native"), y = unit(coords.df$y[which(coords.df$node == 'ellipse')],"native"), size = rad*1.2, ar=3/5, gp=gpar(fill=coords.df[which(coords.df$node == 'ellipse'),]$c.col, col=coords.df[which(coords.df$node == 'ellipse'),]$c.col), angle=pi/2, position.units="native",...)
	}
}

panel.cluster.pie <- function(x=NULL, y=NULL, coords.df, cluster_list, rad, ...){
	if(!('plot.lab' %in% colnames(coords.df))){
		coords.df$plot.lab	 <- coords.df$lab
	}
	coords.df$plot.lab <- as.character(coords.df$plot.lab)

	coords.df$circle <- sapply(coords.df$lab, function(x) return(length(cluster_list[[paste0("N",x)]])<2))
	orig_par <- par()
	par(new=TRUE)
	for (node in coords.df$lab) {		
			pushViewport(viewport(y=unit(coords.df$x[coords.df$lab==node], "native"), x=unit(coords.df$y[coords.df$lab==node], "native"), height=unit(2*rad,"inches"), width=unit(2*rad, "inches")))
			par(new=TRUE)
			par(plt=gridPLT(),lwd=0.1)

			fill_col <- sapply(names(cluster_list[[paste0("N",node)]]), function(x) cluster_list[["col"]]$c.col[cluster_list[["col"]]$lab == x])
			if (coords.df$circle[coords.df$lab==node]==TRUE){
				grid.circle(x=0.5, y=0.5, r = unit(0.48,"npc"), gp=gpar(fill=fill_col,col="black", lwd=0.2),default.units="npc",...)
			# grid.text("x", x=0.5, y=0.5, default.units="npc")
			} else{
				# browser()				
				pie(cluster_list[[paste0("N",node)]],radius=1, labels=NA, col=fill_col, lwd=0.2, init.angle=45)
			}
			# grid.rect(gp=gpar(col="black"))

			popViewport()
		}	 
	par(orig_par)		

}

panel.cluster.pie2 <- function(x=NULL, y=NULL, coords.df, cluster_list, rad, ...){
	if(!('plot.lab' %in% colnames(coords.df))){
		coords.df$plot.lab	 <- coords.df$lab
	}
	coords.df$plot.lab <- as.character(coords.df$plot.lab)
	coords.df$circle <- sapply(coords.df$plot.lab, function(x) return(nchar(x)<3))
	print(coords.df)
	print(rad)
	for (i in names(cluster_list)) {
		pie_col <- sapply(names(cluster_list[[i]]), function(x) coords.df$c.col[coords.df$lab == x])
		gridPie(xpos=coords.df$y[coords.df$lab==i], ypos=coords.df$x[coords.df$lab==i],  x=cluster_list[[i]], radius=5,  col=pie_col)
	}
	
}

gridPie <- function (xpos = 0, ypos = 0, x, edges = 200, radius = 1, col = NULL, 
    startpos = 0, shadow = FALSE, shadow.col = c("#ffffff", "#cccccc"), 
    explode = 0, ...) 
{
    if (is.null(dev.list)) 
        plot(0, xlim = c(-1.5, 1.5) * radius + xpos, ylim = c(-1.5, 
            1.5) * radius + ypos, type = "n", axes = FALSE, xlab = "", 
            ylab = "")
    if (!is.numeric(x)) 
        stop("floating.pie: x values must be numeric.")
    validx <- which(!is.na(x) & x > 0)
    x <- c(0, cumsum(x[validx])/sum(x[validx]))
    dx <- diff(x)
    nx <- length(dx)
    if (is.null(col)) 
        col <- rainbow(nx)
    else if (length(col) < nx) 
        col <- rep(col, nx)
    xylim <- par("usr")
    plotdim <- par("pin")
    yradius <- radius * (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * 
        plotdim[1]/plotdim[2]
    bc <- 2 * pi * (x[1:nx] + dx/2) + startpos
 	browser()
    for (i in 1:nx) {
        n <- max(2, floor(edges * dx[i]))
        t2p <- 2 * pi * seq(x[i], x[i + 1], length = n) + startpos
        xc <- c(cos(t2p) * radius + xpos, xpos)
        yc <- c(sin(t2p) * yradius + ypos, ypos)
        
        grid.polygon(xc, yc, gp=gpar(fill = col[i]), ...)
        t2p <- 2 * pi * mean(x[i + 0:1]) + startpos
        xc <- cos(t2p) * radius
        yc <- sin(t2p) * radius
    }
    invisible(bc)
}
