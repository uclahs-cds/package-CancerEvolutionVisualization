add.node.ellipse <- function( clone.out, rad, label_nodes=NULL, label_cex=NA, ...){
	
	if(!('plot.lab' %in% colnames(clone.out$v))){
		clone.out$v$plot.lab	 <- clone.out$v$lab
	}
	clone.out$v$plot.lab <- as.character(clone.out$v$plot.lab)
	clone.out$v$circle <- sapply(clone.out$v$plot.lab, function(x) return(nchar(x)<3))
	if(any(clone.out$v$circle==TRUE)){
		nodes_grob <- circleGrob(x = unit(clone.out$v$x[clone.out$v$circle==TRUE], "native"), y = unit(clone.out$v$y[clone.out$v$circle==TRUE],"native"), r = unit(rad,"inches"), gp=gpar(fill=clone.out$v$c.col,col="transparent",lwd=2),default.units="native",...)
	# grid.rect(y=unit(clone.out$v$x[clone.out$v$lab==i], "native"), x=unit(clone.out$v$y[clone.out$v$lab==i], "native"), height=unit(2*rad,"inches"), width=unit(2*rad, "inches"),gp=gpar(col="black"))
	
	}
	if(any(clone.out$v$circle!=TRUE)){
		nodes_grob <- ellipseGrob(x = unit(clone.out$v$x[clone.out$v$circle!=TRUE],"native"), y = unit(clone.out$v$y[clone.out$v$circle!=TRUE],"native"), size = rad*1.2, ar=3/5, gp=gpar(fill=clone.out$v$c.col, col=clone.out$v$c.col), angle=pi/2, position.units="native",...)
	}
    clone.out$grobs <- c(clone.out$grobs, list(nodes_grob))
	if(!is.null(label_nodes) && label_nodes== TRUE){
		if(is.na(label_cex)){
			label_cex <- rad*2/(get.gpar("fontsize")$fontsize/72)
		}
		lab <- if("plot.lab" %in% colnames(clone.out$v)) clone.out$v$plot.lab else  clone.out$v$lab
  		node_label_grob <- textGrob(lab, x=unit(clone.out$v$x, "native"), y=unit(clone.out$v$y, "native"), just=c("center","center"), gp=gpar(col='#FFFFFF',cex=label_cex))
  		clone.out$grobs <- c(clone.out$grobs, list(node_label_grob))
	}
}

