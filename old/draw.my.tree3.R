draw.my.tree3 <- function( in.df=NULL, genes.list = NULL, samp_name=NULL, rad=1.2,filename = "myplot.pdf", scale.x.real = 0.05443424, 
                          scale2=0.5/362, wid=60,  extra.len=10, len=55,sig.shape=3, offset=0.3, dev.width=3.342857, gene.cex=1.17,quant_lengths=TRUE,
                          seg1.col="black",seg2.col="green", add.normal=FALSE, add.bells = TRUE, add.circle=TRUE, add.genes=TRUE, length_from_edge=TRUE,add.labels=TRUE,branching=TRUE,
                          axis.type='PGA',w.padding=0.3,h.padding=0.3,lab.cex =1.55,lin.width, line.dist=0.1,circle.col="grey29",line.lwd=3,title.cex=1.7,curve=3,
                          axis.space.left=0, axis.space.right=0,alternate.genes=FALSE, axis.cex =1.45, snv.interval=400,pga.cap=TRUE, title.y=0.3){
  
  

  	panel.draw_clones <- function(..., subscripts){ 
		in.df <- in.df[subscripts, ]
		in.tree.df <- in.df[-1,c(1:7)]
		v <- in.tree.df[!in.tree.df$excluded,]
		v <- v[order(v$lab),]
		v$vaf[!v$excluded] <- v$ccf[!v$excluded]/max(v$ccf[!v$excluded])

		if(all(is.null(in.tree.df$c.col))){
		v$c.col <- circle.col
		}
		v$x <- numeric(length(nrow(v)))
		v$y <- numeric(length(nrow(v)))
		v$len <- numeric(length(nrow(v)))
		v$y.shift <- numeric(length(nrow(v)))
		v$y.mid <- numeric(length(nrow(v)))
		print(v)	  
  	}

  	clone.plot <- xyplot(lab~parent | ID,
  	                    data=in.df,
  						panel = panel.draw_clones)
      # lims <- current.panel.limits() 
      # panel.xyplot(...) 
      # grid.remove(trellis.grobname(name="ticks.right","panel",column=1,row=1))
      # grid.remove(trellis.grobname(name="ticks.top","panel",column=1,row=1))
      # if (!is.null(gene_grobs)){
      #   add_genes(gene_grobs)
      # }
      # grid.lines(x=lims$xlim[1],y=lims$ylim,default.units="native",gp=gpar(col="black",alpha=1,lwd=2.5))
      # grid.lines(x=lims$xlim,y=lims$ylim[1],default.units="native",gp=gpar(col="black",alpha=1,lwd=2.5))
      
      # panel.clones(clones = clone.out$clones)
      # panel.cluster.ellipse(coords.df = v, rad=rad)

      # if(nrow(tree_segs)>0){
      #   grid.segments(x0=tree_segs$basey,y0=tree_segs$basex,x1=tree_segs$tipy, y1=tree_segs$tipx, default.units="native", gp=gpar(col=seg1.col,lwd=line.lwd))
     
      # }
      # #make lines for SNVs
      # if(!is.null(tree_segs2) ){
      #   grid.segments(x0=tree_segs2$basey,y0=tree_segs2$basex,x1=tree_segs2$tipy, y1=tree_segs2$tipx, default.units="native", gp=gpar(col=seg1.col,lwd=line.lwd))
     
      # }
      # if(add.labels ==TRUE ){
      #   grid.text(lab, x=unit(y,"native"), y=unit(x,"native"),just=c("center","center"), gp=gpar(col='#FFFFFF',cex=1.4))
      # }
      # if(add.normal ==TRUE){
      #   upViewport()
      #   downViewport(trellis.vpname("panel",1,1,clip.off=TRUE))
      #   grid.rect(x=unit(0,"native"), y=unit(-0.3-rad*1/scale.x.real,"native"),width=unit(2*rad,"inches"), height=unit(2*rad,"inches"), just=c("center","center"),gp=gpar(col="black",lwd=2,lty="31"))
      #   grid.text("N",x=unit(0,"native"),y=unit(-0.3-rad*1/scale.x.real,"native"),just="center",gp=gpar(col='black',cex=1.4))
      #   upViewport()
      #   downViewport(trellis.vpname("panel",1,1))

      }