add.node.ellipse <- function( clone.out, rad, ...){
	
	if(!('plot.lab' %in% colnames(clone.out$v))){
		clone.out$v$plot.lab	 <- clone.out$v$lab
	}
	clone.out$v$plot.lab <- as.character(clone.out$v$plot.lab)
	clone.out$v$circle <- sapply(clone.out$v$plot.lab, function(x) return(nchar(x)<3))
	if(any(clone.out$v$circle==TRUE)){
		nodes_grob <- grid.circle(x = unit(clone.out$v$x[clone.out$v$circle==TRUE], "native"), y = unit(clone.out$v$y[clone.out$v$circle==TRUE],"native"), r = unit(rad,"inches"), gp=gpar(fill=clone.out$v$c.col,col="transparent",lwd=2),default.units="native",...)
	# grid.rect(y=unit(clone.out$v$x[clone.out$v$lab==i], "native"), x=unit(clone.out$v$y[clone.out$v$lab==i], "native"), height=unit(2*rad,"inches"), width=unit(2*rad, "inches"),gp=gpar(col="black"))
	
	}
	if(any(clone.out$v$circle!=TRUE)){
		nodes_grob <- grid.ellipse(x = unit(clone.out$v$x[clone.out$v$circle!=TRUE],"native"), y = unit(clone.out$v$y[clone.out$v$circle!=TRUE],"native"), size = rad*1.2, ar=3/5, gp=gpar(fill=clone.out$v$c.col, col=clone.out$v$c.col), angle=pi/2, position.units="native",...)
	}

  clone.out$grobs <- c(clone.out$grobs, list(nodes_grob))
}
