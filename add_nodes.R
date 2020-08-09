add.node.ellipse <- function( clone.out, rad, label_nodes=NULL, label_cex=NA, add_normal=FALSE, scale1,...){
	
	if(!('plot.lab' %in% colnames(clone.out$v))){
		clone.out$v$plot.lab	 <- clone.out$v$lab
	}
	clone.out$v$plot.lab <- as.character(clone.out$v$plot.lab)
	clone.out$v$circle <- sapply(clone.out$v$plot.lab, function(x) return(nchar(x)<3))
	if(any(clone.out$v$circle==TRUE)){
		pushViewport(clone.out$vp)
		#more precise than circleGrob
		nodes_grob <- ellipseGrob(x = unit(clone.out$v$x[clone.out$v$circle==TRUE],"native"), y = unit(clone.out$v$y[clone.out$v$circle==TRUE],"native"), size = rad, ar=1, gp=gpar(fill=clone.out$v$c.col, col=clone.out$v$c.col), angle=pi/2, position.units="native",size.units="inches",...)
		popViewport()
		# nodes_grob <- circleGrob(x = unit(clone.out$v$x[clone.out$v$circle==TRUE], "native"), y = unit(clone.out$v$y[clone.out$v$circle==TRUE],"native"), r = unit(rad/scale1,"native"), gp=gpar(fill=clone.out$v$c.col,col="black",lwd=1),default.units="native",...)
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
	
	#debugging
	# clone.out$grobs <- c(clone.out$grobs, list(segmentsGrob(x0=unit(clone.out$xlims[1], "native"), x1=unit(clone.out$xlims[2], "native"), y0=unit(clone.out$v$y, "native"), y1=unit(clone.out$v$y, "native"), gp=gpar(col="red", cex=1))))
	# clone.out$grobs <- c(clone.out$grobs, list(segmentsGrob(x0=unit(clone.out$xlims[1], "native"), x1=unit(clone.out$xlims[2], "native"), y0=unit(clone.out$v$y-rad/scale1, "native"),y1=unit(clone.out$v$y-rad/scale1, "native"), gp=gpar(col="red", cex=1))))
	# clone.out$grobs <- c(clone.out$grobs, list(segmentsGrob(x0=unit(clone.out$xlims[1], "native"), x1=unit(clone.out$xlims[2], "native"), y0=unit(clone.out$v$y+rad/scale1, "native"),y1=unit(clone.out$v$y+rad/scale1, "native"), gp=gpar(col="red", cex=1))))
	# clone.out$grobs <- c(clone.out$grobs, list(segmentsGrob(x0=unit(clone.out$xlims[1], "native"), x1=unit(clone.out$xlims[2], "native"), y0=unit(30, "native"),y1=unit(30, "native"), gp=gpar(col="blue", cex=1))))
	# clone.out$grobs <- c(clone.out$grobs, list(pointsGrob(x=unit(clone.out$v$x, "native"), y=unit(clone.out$v$y, "native"), gp=gpar(col="red", cex=1))))
}

add_normal <- function(clone.out, rad, label_cex){
		normal_label <- textGrob("N",x=unit(0.5,"npc"),y=unit(0.5,"npc"),name="normal.label", just="center",gp=gpar(col='black',cex=label_cex))  
		normal_box <- rectGrob(x=unit(0.5,"npc"), y=unit(0.5,"npc"),name="normal.box", height=grobHeight(normal_label)*1.2, width=grobWidth(normal_label)*1.2, just=c("center","center"),gp=gpar(col="black",fill="transparent", lwd=1.5,lty="31"))
		# normal_box <- rectGrob(x=unit(0.5,"npc"), y=unit(0.5,"npc"),name="normal.box", width=unit(2*rad,"inches"), height=unit(2*rad,"inches"), just=c("center","center"),gp=gpar(col="black",fill="transparent", lwd=1.5,lty="31")) unit(convertY(grobHeight(normal_box),"inches", valueOnly=TRUE),"inches")
		normal_grob <- gTree(children=gList(normal_box, normal_label),name="normal.gtree", cl="normal_node", vp=vpStack(make_plot_viewport(clone.out, clip="off"), viewport(y=unit(1,"npc"), x=unit(0.5,"npc"), height=grobHeight(normal_box), width=grobWidth(normal_box), just=c("centre","bottom"))))
		# normal_grob <- gTree(children=gList(normal_box, normal_label),name="normal.gtree", cl="normal_node", vp=viewport(y=unit(0.5,"native"), x=unit(0,"native"), just=c("centre","bottom") ))
		clone.out$grobs <- c(clone.out$grobs, list(normal_grob))
	}

# makeContent.add_normal <- function(normal_gtree){
# 		normal_label <- textGrob("N",x=unit(0.5,"npc"),y=unit(0.5,"npc"),name="normal.label", just="center",gp=gpar(col='black',cex=label_cex))
# 		normal_box <- rectGrob(x=unit(0.5,"npc"), y=unit(0.5,"npc"),name="normal.box", width=1.1*stringWidth(normal_label), height=1.1*stringHeight(normal_label), just=c("center","center"),gp=gpar(col="black",lwd=1.5,lty="31"))
# 		setChildren(normal_gtree, gList(normal_box, normal_label))
# }