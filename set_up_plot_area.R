calculate_main_plot_size <- function(clone.out, scale1, wid, min_width, xaxis_space_left, xaxis_space_right){
	ymax <- max(clone.out$v$len) + clone.out$v$y[1]
	height <- (ymax+2)*scale1
	
	if(is.null(min_width)){
	  xmax <- wid
	  width <- wid*scale1+xaxis_space_left + xaxis_space_right
	  xlims <- c(-xmax/2-(xaxis_space_left*1/scale1), xmax/2+xaxis_space_right*(1/scale1))
	 } else{	  
	  xmin <- min(c(clone.out$v$x))
	  xmax <- max(c(clone.out$v$x))
	  xlims <- c(xmin-(xaxis_space_left*1/scale1),xmax+xaxis_space_right*(1/scale1))
	  width <- (max(xlims)-min(xlims))*scale1
	  diff <- min_width - width
	  if(diff > 0){
	    xmin <- xmin-0.5*diff*1/scale1
	    xmax <- xmax+0.5*diff*1/scale1
	    xlims <- c(xmin-(xaxis_space_left*1/scale1),xmax+xaxis_space_right*(1/scale1))
	    width <- (max(xlims)-min(xlims))*scale1
	  }
	}

	clone.out$height <- height
	clone.out$width <- width
	clone.out$xlims <- xlims
	clone.out$ymax <- ymax
	clone.out$vp <- make_plot_viewport(clone.out)
}


make_plot_viewport <- function(clone.out, clip="on"){
	vp <- viewport(height=unit(clone.out$height,"inches"), width=unit(clone.out$width, "inches"), name="plot.vp", xscale=clone.out$xlims,  yscale=c(clone.out$ymax,0),gp=gpar(fill='pink'), clip=clip)
	return(vp)
}


extend_axis <- function(axisGrob, limits, type){
	arg_list <- list(getGrob(axisGrob, "major"), limits)
	names(arg_list) <- c("grob",type)
	axisGrob <- setGrob(axisGrob, "major", do.call(editGrob, arg_list))
	return(axisGrob)
}

add_axis_label <- function(axisGrob, axis_label, axis_position, axis_label_cex, vp){	
	if(axis_position == "bottom"){
		d <- "y"
		just <- c("centre","top")
		rot <- 0
		x <- unit(0.5, "npc")
		y <- (getGrob(axisGrob, "labels")$y+getGrob(axisGrob, "ticks")$y1)*1.5		
	} else {
		tcklen  <- unit(diff(c(as.numeric(getGrob(axisGrob, "ticks")$x0), as.numeric(getGrob(axisGrob, "ticks")$x1))),"lines")
		y <- unit(0.5,"npc")
		if(axis_position == "left"){
			d <- "x"
			just <- c("right", "centre")
			rot <- 90
			# browser()
			x <- (getGrob(axisGrob, "labels")$x+tcklen)*1.5
		} else if(axis_position == "right"){
			d <- "x"
			just <- c("left","centre")
			x <- (getGrob(axisGrob, "labels")$x+tcklen)*1.5
			rot <- 270
		}
	}

	axis_lab <- textGrob(axis_label, gp=gpar(cex=axis_label_cex),vjust=0,  x=x, rot=rot, y=y)
	axis_w_lab <- addGrob(axisGrob,axis_lab)
	axis_gtree <- gTree(children=gList(axis_w_lab), vp=vp)
	return(axis_gtree)
}

add_axes <- function(clone.out,  scale1, scale2=NULL, yaxis_position="left", xaxis_label="CCF", yaxis1_label="PGA", yaxis2_label=NULL, yaxis1_interval=10, yaxis2_interval=NA, no_ccf=FALSE, axis_label_cex=list(x=1.55,y=1.55), axis_cex=list(x=1,y=1), ylabels1=NULL, ylabels2=NULL){
	add_xaxis(clone.out, scale1=scale1, axis_label=xaxis_label, no_ccf=no_ccf, axis_label_cex=axis_label_cex[['x']], axis_cex=axis_cex[['x']])
	
	if( yaxis_position == "both" ){
		if(is.null(yaxis2_label)){
			warning("Missing second y-axis label")
			yaxis2_label <- ""
		}
		print(yaxis1_interval)
		add_yaxis(clone.out, yaxis_position="left", axis1_label=yaxis1_label, yaxis1_interval=yaxis1_interval, no_ccf=no_ccf, axis_label_cex=axis_label_cex[['y']], axis_cex=axis_cex[['y']],ylabels=ylabels1)
		# print("done axis1")
		add_yaxis(clone.out, yaxis_position="right", conversion_factor=scale1/scale2, axis1_label=yaxis2_label, yaxis1_interval=yaxis2_interval, no_ccf=no_ccf, axis_label_cex=axis_label_cex[['y']], axis_cex=axis_cex[['y']],ylabels=ylabels2)
		# print("done axis2")
		#debugging
		# clone.out$grobs <- c(clone.out$grobs, list(yaxisGrob(at=c(0,1,3954*scale2,2,3), label=c(0,1,"", 2,3), gp=gpar(cex=axis_label_cex[['y']]), main=FALSE, vp=viewport(height=unit(clone.out$height,"inches"), width=unit(clone.out$width+.35, "inches"), name="ruler.vp", xscale=clone.out$xlims,  yscale=c(clone.out$ymax*scale1,0),gp=gpar(fill='pink'), clip="off")  )))
	}else{
		add_yaxis(clone.out, yaxis_position=yaxis_position, axis1_label=yaxis1_label, yaxis1_interval=yaxis1_interval, no_ccf=no_ccf, axis_label_cex=axis_label_cex[['y']], axis_cex=axis_cex[['y']],ylabels=ylabels1)
	}
}

add_yaxis <- function(clone.out, yaxis_position="left", conversion_factor=1, axis1_label="PGA", yaxis2_label=NULL, yaxis1_interval=NA, no_ccf=FALSE, axis_label_cex=list(x=1.55,y=1.55), axis_cex=list(x=1,y=1), ylabels=NULL){
	vp_unclipped <- make_plot_viewport(clone.out, clip="off") #necessary to get the right positioning

	#set up tick labels
	if(is.null(ylabels)){
		if(!is.na(yaxis1_interval)){
			ylabels <- seq(0, clone.out$ymax*conversion_factor, by=yaxis1_interval)
		} else{
			ylabels <- pretty(seq(0, clone.out$ymax*conversion_factor))
		}
	}

	yat <- ylabels/conversion_factor
	yat <- yat[which(yat <= clone.out$ymax)]
	ylabels <- ylabels[which(yat <= clone.out$ymax)]
	
	yaxis1 <- yaxisGrob(at=yat, label=ylabels, gp=gpar(cex=axis_label_cex), main=ifelse(yaxis_position=="left",TRUE,FALSE))
	
	if(max(yat)/conversion_factor != clone.out$ymax){ #extending the axis line beyond the last tick 
		yaxis1 <- extend_axis(yaxis1, limits=unit(c(0,clone.out$ymax),"native"), type="y")
		print(getGrob(yaxis1, "major")$y)
	}		
# browser()
	yaxis_gtree <- add_axis_label(yaxis1, axis1_label, axis_position=yaxis_position, axis_label_cex, vp=vp_unclipped)
	clone.out$grobs <- c(clone.out$grobs, list(yaxis_gtree))
}

add_xaxis <- function(clone.out, scale1, axis_label="CCF", no_ccf=FALSE, axis_label_cex=1.55, axis_cex=1){
	vp_unclipped <- make_plot_viewport(clone.out, clip="off") #necessary to get the right positioning

	#set up tick labels
	clone_widths <- as.numeric(as.matrix(clone.out$v[,c("x1","x2")]))
	xat <- c(min(clone_widths), max(clone_widths))
	xlabels <- c(0,paste0(round(max(clone.out$v$ccf)*100,0),'%'))
	
	xaxis <- xaxisGrob(at=xat, label=xlabels, gp=gpar(cex=axis_label_cex), main=TRUE)
	
	#move the labels up a little 
	xaxis_labels <- editGrob(getGrob(xaxis, "labels"), y= unit(-.09, "npc"), vjust=1)
	xaxis <- setGrob(xaxis, "labels", xaxis_labels)
	
	if(diff(xat)/scale1 != clone.out$width){ #extending the axis line beyond the clone limits
		xaxis <- extend_axis(xaxis, limits=unit(clone.out$xlims,"native"), type="x")#extendsetGrob(xaxis, "major", editGrob(getGrob(xaxis, "major"), x =unit(clone.out$xlims,"native")))
	}

	#add in the axis label
	xaxis_gtree <- add_axis_label(xaxis, axis_label, axis_position="bottom", axis_label_cex, vp=vp_unclipped)
	# axis_lab <- textGrob(axis_label, gp=gpar(cex=axis_label_cex), vjust=1, y=(getGrob(xaxis, "labels")$y+getGrob(xaxis, "ticks")$y1)*1.5)
	# xaxis_w_lab <- addGrob(xaxis,axis_lab)
	# xaxis_gtree <- gTree(children=gList(xaxis_w_lab), vp=vp_unclipped, name="xaxis")

	clone.out$grobs <- c(clone.out$grobs, list(xaxis_gtree))
}
