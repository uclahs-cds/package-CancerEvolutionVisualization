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
}

make_plot_viewport <- function(clone.out, axis_type, scale1, axis1){
	vp <- viewport(height=unit(clone.out$height,"inches"), width=unit(clone.out$width, "inches"), name="plot.vp", xscale=clone.out$xlims, yscale=c(clone.out$ymax,0),gp=gpar(fill='pink'), clip=TRUE)
	return(vp)
}

# make_axes <- function(clone.out, axis_type, scale1, scale2){
# 	xaxisGrob(at=c(0,100), label = round(all_ccfs,1), gp=gpar(cex=0.65), main=TRUE)
# }
