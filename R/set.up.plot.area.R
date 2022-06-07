calculate_main_plot_size <- function(clone.out, scale1, wid, min.width, xaxis.space.left, xaxis.space.right, rad){
	ymax <- max(clone.out$v$len) + clone.out$v$y[1]
	height <- (ymax)*scale1
	
	if(is.null(min.width)){
	  xmax <- wid
	  width <- wid*scale1+xaxis.space.left + xaxis.space.right + 4*rad
	  xlims <- c(-xmax/2-(xaxis.space.left*1/scale1 +2*rad/scale1), xmax/2+(xaxis.space.right*(1/scale1) +2*rad/scale1))
	 } else{	  
	  xmin <- min(c(clone.out$v$x))
	  xmax <- max(c(clone.out$v$x))
	  xlims <- c(xmin-(xaxis.space.left*1/scale1),xmax+xaxis.space.right*(1/scale1))
	  width <- (max(xlims)-min(xlims))*scale1
	  diff <- min.width - width
	  if(diff > 0){
	    xmin <- xmin-0.5*diff*1/scale1
	    xmax <- xmax+0.5*diff*1/scale1
	    xlims <- c(xmin-(xaxis.space.left*1/scale1 +2*rad/scale1),xmax+xaxis.space.right*(1/scale1)+2*rad/scale1)
	    width <- (max(xlims)-min(xlims))*scale1
	  }
	}
	clone.out$height <- height
	clone.out$width <- width
	clone.out$xlims <- xlims
	clone.out$ymax <- ymax
	clone.out$vp <- make_plot_viewport(clone.out, clip= if(clone.out$no_ccf == TRUE) "off" else "on", just=c("centre","top"), y=0.9)
}

make_plot_viewport <- function(clone.out, clip="on", just=c('centre', 'centre'), y=0.5){
	vp <- viewport(y=y, height=unit(clone.out$height,"inches"), width=unit(clone.out$width, "inches"), name="plot.vp", xscale=clone.out$xlims,  yscale=c(clone.out$ymax,0), just=just, gp=gpar(fill='pink'), clip=clip)
	return(vp)
}

extend_axis <- function(axisGrob, limits, type){
	arg_list <- list(getGrob(axisGrob, "major"), limits)
	names(arg_list) <- c("grob",type)
	axisGrob <- setGrob(axisGrob, "major", do.call(editGrob, arg_list))
	return(axisGrob)
}

add_axis_label <- function(axisGrob, axis_label, axis_position, axis.label.cex, vp){	
	if(axis_position == "bottom"){
		d <- "y"
		just <- c("centre","top")
		rot <- 0
		x <- unit(0.5, "npc")
		y <- (getGrob(axisGrob, "labels")$y+getGrob(axisGrob, "ticks")$y1)*1.75
	} else {
		pushViewport(vp)
		tcklen  <- unit(diff(c(as.numeric(getGrob(axisGrob, "ticks")$x0), as.numeric(getGrob(axisGrob, "ticks")$x1))),"lines")
		y <-  convertY(unit(max(as.numeric(getGrob(axisGrob, "major")$y))*.5,"native"), "inches")			

		if(axis_position == "left"){
			d <- "x"
			just <- c("centre", "centre")
			rot <- 90
			
			x <- unit(convertX(grobWidth(getGrob(axisGrob, "labels")), "mm", valueOnly=TRUE)*axisGrob$gp$cex*-1 + convertX(unit(1,"lines")*axisGrob$gp$cex,"mm", valueOnly=TRUE)*-1+ convertX(getGrob(axisGrob, "labels")$x*axisGrob$gp$cex, "mm", valueOnly=TRUE),"mm")
		} else if(axis_position == "right"){
			d <- "x"
			just <- c("left","centre")
			x <- (getGrob(axisGrob, "labels")$x+tcklen)*1.5
			rot <- 270
		}
		popViewport()
	}

	axis_lab <- textGrob(name = 'axis.label', axis_label, gp=gpar(cex=axis.label.cex), vjust=0, x=x, rot=rot, y=y)
	axis_gtree <- gTree(name = paste0('axis.', axis_position), children=gList(axis_lab, axisGrob), vp=vp)
	return(axis_gtree)
}

add_axes <- function(clone.out,  scale1, scale2=NULL, yaxis_position="left", xaxis.label="CCF", yaxis1.label="PGA", yaxis2.label=NULL, yaxis1.interval=10, yaxis2.interval=NA, no_ccf=FALSE, axis.label.cex=list(x=1.55,y=1.55), axis.cex=list(x=1,y=1), ylabels1=NULL, ylabels2=NULL, ylimit=NULL){
	if(no_ccf == FALSE & 'ccf' %in% colnames(clone.out$v) & all(!is.na(clone.out$v$ccf))){		
		add_xaxis(clone.out, scale1=scale1, axis_label=xaxis.label, no_ccf=no_ccf, axis.label.cex=axis.label.cex[['x']], axis.cex=axis.cex[['x']])
	}
	if( yaxis_position == "both" ){
		if(is.null(yaxis2.label)){
			warning("Missing second y-axis label")
			yaxis2.label <- ""
		}
		conversion_factor <- scale1/scale2
		ymax1 <- add_yaxis(clone.out, yaxis_position="left", axis1_label=yaxis1.label, yaxis1.interval=yaxis1.interval, no_ccf=no_ccf, axis.label.cex=axis.label.cex[['y']], axis.cex=axis.cex[['y']],ylabels=ylabels1, ylimit=ylimit)
		add_yaxis(clone.out, yaxis_position="right", conversion_factor=conversion_factor, axis1_label=yaxis2.label, yaxis1.interval=yaxis2.interval, no_ccf=no_ccf, axis.label.cex=axis.label.cex[['y']], axis.cex=axis.cex[['y']],ylabels=ylabels2, ylimit=ymax1)
	}else{
		add_yaxis(clone.out, yaxis_position=yaxis_position, axis1_label=yaxis1.label, yaxis1.interval=yaxis1.interval, no_ccf=no_ccf, axis.label.cex=axis.label.cex[['y']], axis.cex=axis.cex[['y']],ylabels=ylabels1, ylimit=ylimit)
	}
}

add_yaxis <- function(clone.out, yaxis_position="left", conversion_factor=1, axis1_label="PGA", yaxis2.label=NULL, yaxis1.interval=NA, no_ccf=FALSE, ylimit=NULL, axis.label.cex=list(x=1.55,y=1.55), axis.cex=list(x=1,y=1), ylabels=NULL){
    #necessary to get the right positioning
    vp_unclipped <- make_plot_viewport(clone.out, clip="off") 

	if(!is.null(ylimit) && ylimit == 'node'| (is.null(ylimit) & no_ccf == TRUE)){
		ymax <- max(clone.out$v$y )
	}else if (!is.null(ylimit) && ylimit == 'plot_length'| (is.null(ylimit) & no_ccf == FALSE)){
		ymax <- clone.out$ymax
	}else if(!is.null(ylimit)){
		ymax <- ylimit
	}
	#set up tick labels
	if(is.null(ylabels)){
		if(!is.na(yaxis1.interval)){
			ylabels <- seq(0, ymax*conversion_factor, by=yaxis1.interval)
			if(no_ccf == TRUE & max(ylabels) < ymax){
				ylabels <- c(ylabels, max(ylabels) + yaxis1.interval)
			}
		} else{
			ylabels <- pretty(seq(0, ymax*conversion_factor))
		}
	}

	yat <- ylabels/conversion_factor
	yaxis1 <- yaxisGrob(name = 'axis.content', at=yat, label=ylabels, gp=gpar(cex=axis.cex), main=ifelse(yaxis_position=="left",TRUE,FALSE))
	
	if(max(yat)/conversion_factor != ymax & no_ccf == FALSE){ #extending the axis line beyond the last tick 
		yaxis1 <- extend_axis(yaxis1, limits=unit(c(0,ymax),"native"), type="y")
	}		

	yaxis_gtree <- add_axis_label(yaxis1, axis1_label, axis_position=yaxis_position, axis.label.cex, vp=vp_unclipped)
	clone.out$grobs <- c(clone.out$grobs, list(yaxis_gtree))
	return(ymax)
}

add_xaxis <- function(clone.out, scale1, axis_label="CCF", no_ccf=FALSE, axis.label.cex=1.55, axis.cex=1){
	vp_unclipped <- make_plot_viewport(clone.out, clip="off") #necessary to get the right positioning

	#set up tick labels
	clone_widths <- as.numeric(as.matrix(clone.out$v[,c("x1","x2")]))
	xat <- c(min(clone_widths), max(clone_widths))
	xlabels <- c(0,paste0(round(max(clone.out$v$ccf)*100,0),'%'))
	
	xaxis <- xaxisGrob(name = 'axis.content', at=xat, label=xlabels, gp=gpar(cex=axis.label.cex), main=TRUE)
	
	#move the labels up a little 
	xaxis.labels <- editGrob(getGrob(xaxis, "labels"), y= unit(-.09, "npc"), vjust=1)
	xaxis <- setGrob(xaxis, "labels", xaxis.labels)
	
	if(diff(xat)/scale1 != clone.out$width){ #extending the axis line beyond the clone limits
		xaxis <- extend_axis(xaxis, limits=unit(clone.out$xlims,"native"), type="x")#extendsetGrob(xaxis, "major", editGrob(getGrob(xaxis, "major"), x =unit(clone.out$xlims,"native")))
	}

	#add in the axis label
	xaxis_gtree <- add_axis_label(xaxis, axis_label, axis_position="bottom", axis.label.cex, vp=vp_unclipped)
	clone.out$grobs <- c(clone.out$grobs, list(xaxis_gtree))
}

add_title <- function(clone.out, title, title.cex, title.y=NULL, title.y.units="npc"){
		y_pos <- unit(1.08,"npc")
		if(!is.null(title.y)){
			pushViewport(clone.out$vp)
			plot_top <- convertY(unit(1,"npc"), title.y.units, valueOnly=TRUE)
			popViewport()
			y_pos <- plot_top + title.y
		}
		title_label <- textGrob(title, just="center",gp=gpar(col='black',cex=title.cex))
		title_grob <- gTree(children=gList(title_label),name="title.gtree", cl="title_label", vp=vpStack(make_plot_viewport(clone.out, clip="off", just=c("centre", "centre")), viewport(y=unit(y_pos, title.y.units), x=unit(0,"native"), height=grobHeight(title_label), width=grobWidth(title_label), just=c("centre","bottom"))))

		clone.out$grobs <- c(clone.out$grobs, list(title_grob))
}
