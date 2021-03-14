library(plyr)
library(lattice)
library(latticeExtra)
library(grid)
library(gridExtra)
library(BoutrosLab.plotting.general)
library(tidyr)
library(gtable)

multiregion_ccfs <- function(tree_in,  tumour_name, tree_gTree, ccf_names=NULL, width=1.5, nrow=2, ncol=2, widths=c(4.5,3), height=5, title.cex=1.5){
	in.df <- tree_in[,c(2,3,8)]

	colnames(in.df)[2] <- "tip"
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/tree_tiers.R')

	in.df <- get_num_tiers(in.df)

	samps <- strsplit(in.df$ccf[1],split=',')[[1]]

	if(is.null(ccf_names)){
		ccf_names <- paste("sample", seq_along(samps), sep=" ")
	} else if(is.na(ccf_names)){
		ccf_names <- rep("", length(samps))
	}

	in.df <- separate(col=ccf, sep=",",into=ccf_names, in.df, convert=TRUE)
	in.df$fill <- tree_in$fill

	print(in.df)
	print(samps)

	samp_list <- list()
	for (i in seq_along(samps)){
		print(ccf_names[i])
		col <- match(ccf_names[i], colnames(in.df))
		print(col)
		position.df  <- data.frame(clone = in.df$tip, diameter=numeric(length=length(in.df$tip)), position=numeric(length=length(in.df$tip)))
		glist_list <- list()
		if(nrow(in.df[which(in.df[,col] > 0.01),]) == 0){
			test_tree <- gTree(children=gList(rectGrob(gp=gpar(fill="#FFFFFF",col="transparent")),
			 textGrob(ccf_names[i],x=unit(0.5,"npc"),y=unit(1.3,"npc"),gp=gpar(cex=0.8)), 
			 textGrob("All zero CCFs", gp=gpar(col="black",cex=0.8), y=0.4)),
			vp=viewport(name="base",width=unit(width,"in"),height=unit(width/2,"in"),clip=FALSE))			
				samp_list[[i]] <- test_tree
			next
		}
		test_tree <- gTree(children=gList(textGrob(ccf_names[i],x=unit(0.5,"npc"),y=unit(1.3,"npc"),gp=gpar(cex=0.73))), 
			# rectGrob(gp=gpar(col="red")),segmentsGrob(ccf_names[i],x0=unit(0,"npc"),x1=unit(1,"npc"),y0=unit(0.5,"npc"),y1=unit(0.5,"npc"),gp=gpar())), 
			vp=viewport(name="base",width=unit(width,"in"),height=unit(width/2,"in"),clip=FALSE))
		# test_tree <- gTree( viewport(width=unit(1.5,"in"),height=unit(1.5,"in")))
		# test_tree
		ccfs <- 0
		shift <- 2
		# png(paste0("test_circle2_",ccf_names[i],".png"),height=7, width=7, unit="in", res=400)
		cairo_pdf(paste0("test_circle2_",ccf_names[i],".pdf"), onefile=FALSE,fallback_resolution=400 )	
		grid.newpage()		
		for (tier in unique(in.df$tier)){

			nodes <- in.df[which(in.df$tier == tier & in.df[,col]>0.01), c(1,2,col,ncol(in.df))]
			tier_grobs <- gList()
			tier_ccf <- 0
			# browser()
			for (j in seq_along(nodes$parent)){
				node <- nodes[j,]
				
				if(node$parent != 0){
					parent <- position.df[position.df$clone == node$parent[1],]
					parent_diameter <- parent$diameter
					parent_position <- parent$position - 0.5*parent_diameter
				}else{
					parent_position <- 0
					parent_diameter <- 0
				}
				sample_diameter <- node[,3]
				siblings <- position.df[which(position.df$clone %in% nodes[1:(j-1),2] & position.df$clone %in% nodes$tip[nodes$parent==node$parent]),] # all siblings before
				if(nrow(siblings)> 0){
					siblings_right_bound <- max(siblings$position) + 0.5*siblings$diameter[which.max(siblings$position)]
				} else{
					siblings_right_bound <- parent_position
				}
				position <- 0.5*sample_diameter + siblings_right_bound
				position.df$diameter[position.df$clone == node$tip] <- sample_diameter
				position.df$position[position.df$clone == node$tip] <- position
				position=position*width
				sample_diameter=sample_diameter*width
						
				vp1 <- viewport(yscale=c(0,1),xscale=c(0,1),clip="on",x=unit(0.5,"npc"),y=unit(0,"npc"), width=unit(width,"inches"), height=unit(width*0.6,"inches"),just="bottom")
				vp2 <- viewport(yscale=c(0,1),xscale=c(0,1),clip="off",x=unit(0.5,"npc"),y=unit(0,"npc"), width=unit(width,"inches"), height=unit(width*0.6,"inches"),just="bottom")
				samp_circ <- grid.circle(draw=TRUE,name=paste("test",node$tip,sep=""),y=unit(0,"npc"), x = unit(position,"inches"), r=unit(0.5*sample_diameter,"inches"), gp=gpar(alpha=0.8,col=node$fill, lwd=2.5, fill=node$fill),default.units="inches",vp=vp1)
				test_tree <- addGrob(test_tree,samp_circ)
				
				child <- in.df[in.df$parent == node$tip & in.df[,col] > 0.01,]
				print("child")
				print(child)
				print(nrow(child))
				if(nrow(child) == 1 && (node[,3] - child[1,col])< 0.1){
					print("child overlaps")
					angle <- pi/2-pi/8*shift
					ypos <- sin(angle)*(0.6*sample_diameter)
					xpos <- cos(angle)*(0.6*sample_diameter)
					print(xpos)
					print(ypos)
					samp_labels <- textGrob(node$tip,y=unit(ypos,"inches"), x=unit(xpos+position,"inches"), check.overlap=TRUE,  gp=gpar(col='black', cex=0.6,lwd=2), default.units="native", vp=vp2, just=c("left","bottom"))
					test_tree <- addGrob(test_tree,samp_labels)
					line_seg <- segmentsGrob(y0=unit(ypos,"inches"), x0=unit(xpos+position,"inches"), y1=unit(sin(angle)*(0.5*sample_diameter),"inches"), x1=unit(cos(angle)*(0.5*sample_diameter)+position,"inches"),vp=vp2)
					test_tree <- addGrob(test_tree,line_seg)
					shift <- shift+1
				}else{				
					samp_labels <- textGrob(node$tip,y=unit(((0.5*sample_diameter))-0.5*strheight(node$tip,unit="inches",cex=0.6),"inches"), x=unit(position,"inches"), check.overlap=TRUE,  gp=gpar(col='black', cex=0.6),default.units="native",vp=vp2)
					test_tree <- addGrob(test_tree,samp_labels)
				}


				if(node[,3]>0){
					ccfs <- c(ccfs,node[,3]+tier_ccf)
				}
				tier_ccf <- tier_ccf+node[,3]
			}

		}
		print(position.df)
		print(ccfs)
		all_ccfs <- c(0,position.df$position+0.5*position.df$diameter)
		all_ccfs <- sort(unique(round(all_ccfs, 1)))
		print(all_ccfs)
		axis_grob <- xaxisGrob(at=all_ccfs, label=round(all_ccfs,1), gp=gpar(cex=0.65), main=TRUE, vp=viewport(y=unit(0.49,"npc"), width=unit(width,"in"),height=unit(width/2,"in"),clip=FALSE))
		xaxis_labels <- editGrob(getGrob(axis_grob, "labels"), y=unit(-.6, "lines"), vjust=1)
		axis_grob <- setGrob(axis_grob, "labels", xaxis_labels)
		
		axis_lab <- textGrob("CCF",gp=gpar(cex=0.7), vp=viewport(y=unit(-0.2,"npc"), width=unit(width,"in"),height=unit(width/2*0.25,"in"), clip="off", just="top"))
		test_tree <- addGrob(test_tree, axis_grob)
		test_tree <- arrangeGrob(test_tree, axis_lab, nrow=2, ncol=1, heights=unit(c(width/2, width/2*0.25), "in"))

		out.df <-  in.df[in.df[,col] > 0.01,c(2,col)]
		out.df[,2] <- round(out.df[,2],2)

		# ccf_grob <- tableGrob(d=out.df, rows=c(), cols=c("clone","CCF"), theme=ttheme_minimal(base_size=6, padding=unit(c(0.5,1),"mm")), vp=viewport(width=unit(width,"in"),height=unit(width/2,"in"),y=unit(-0.4,"npc"),just=c("left","top"))
		# test_tree <- addGrob(test_tree,ccf_grob)
		samp_list[[i]] <- test_tree
		dev.off()
	}

	heights=unit(c(1.5,1.5), "inches")
	test <- arrangeGrob(grobs=samp_list, nrow=nrow, ncol=ncol, vp=viewport(height=unit(height-height*0.2,"inches"),y=0.5,x=0.5,clip="off"), clip="off")

	test <- gtable_add_grob(test, grobs = rectGrob(gp = gpar(fill = 'white', col="white", lwd = 0),vp=viewport(height=unit(height-height*0.2,"inches"),y=0.5,x=0.5,clip="on") ), 1, 1, nrow(test), ncol(test), 0);
	cairo_pdf("test_circle2.pdf", onefile=FALSE,fallback_resolution=400)
	plot(test)
	dev.off()

	titleGrob <- textGrob(tumour_name,gp=gpar(cex=title.cex,fontface=2),vp=viewport(y=unit(-0.2,"npc"))) 
	# tree_gTree$vp$height

	test2 <- arrangeGrob(titleGrob, tree_gTree, test, ncol=2, nrow=2, widths=unit(widths,"inches"),heights=unit(c(0.02,0.98),"npc"),layout_matrix=rbind(c(1,1),c(2,3)),vp=viewport(height=unit(height,"inches")))
	test2$layout$z <- rev(test2$layout$z)
	test2 <- gtable_add_grob(test2, grobs = rectGrob(gp = gpar(fill = 'white', lwd = 0,col="white")), 1, 1, nrow(test2), ncol(test2), 0);

	cairo_pdf(paste0("multisample",tumour_name,".pdf"), onefile=FALSE,width=sum(widths))
	plot(test2)
	dev.off()
	return(test2)
}



# pdf("test_circle.pdf", onefile=FALSE)
# pushViewport(viewport(width=unit(2,"in"), height=unit(2,"in"),yscale=c(1,0),clip=FALSE))
# # pushViewport(viewport(width=unit(2,"in"), height=unit(2,"in"),yscale=c(1,0),clip=TRUE))
# # for (grob in glist_list){
# 	grid.draw(test)
# 	# grid.draw(grob)
# # }
# # grid.draw(tier_grobs[[1]])
# dev.off()
