draw.my.tree2 <- function( in.tree.df=NULL, tree = NULL, genes.df = NULL, cluster_list=NULL, samp_name=NULL, rad=0.1,filename = "myplot.pdf", scale.x.real = 0.05443424, 
                          scale2=0.5/362, wid=60,  extra.len=10, len=55,sig.shape=3, offset=0.3, dev.width=3.342857, gene.cex=1.17,fixed_angle=NULL,
                          seg1.col="black",seg2.col="green", add.normal=FALSE, add.bells = TRUE, add.circle=TRUE, add.genes=TRUE, length_from_edge=TRUE,add.labels=TRUE,
                          branching=TRUE, axis.type='PGA',w.padding=0.3,h.padding=0.3,lab.cex =1.55,lin.width, line.dist=0.1,circle.col="grey29",line.lwd=3,title.cex=1.7,
                          curve=3,axis.space.left=0, min_width=NULL, axis.space.right=0,alternate.genes=FALSE, axis.cex =1.45, snv.interval=400,pga.cap=TRUE, title.y=0.3,
                          label_nodes=FALSE, pga.interval=10, spread=1,xlab="",node.cex=1.4, poly_tumour=FALSE){
  
  
  
  #initializing dataframe for subclones
  no_ccf <- FALSE
  if(all(is.null(in.tree.df$ccf)) | add.bells == FALSE){
    v <- in.tree.df[!in.tree.df$excluded,]
    v <- v[order(v$lab),]
    v$vaf <- NULL
    v$vaf[v$parent == -1] <- 1
    no_ccf <- TRUE
  } else{
    v <- in.tree.df[!in.tree.df$excluded,]
    v <- v[order(v$lab),]
    v$vaf[!v$excluded] <- v$ccf[!v$excluded]/max(v$ccf[!v$excluded])
  }
  if(all(is.null(in.tree.df$c.col))){
    v$c.col <- circle.col
  }
  
  beta_in <- curve
  print(v)
  # rad <- rad*1/scale.x.real
  #initializing line segment dataframe and adjusting lengths to accomodate the node circles
  tree$angle <- numeric(length=nrow(tree))
  if('length2' %in% colnames(tree)){
    print("here")
    tree$length2.c <- tree$length2/scale.x.real*scale2
    tree$length <- apply(tree, 1,function(x) max(x[c(3,6)]))
  } else{
    tree$length <- tree$length1
  }

  raw.tree <- tree
  if(add.circle==TRUE && length_from_edge==TRUE){
    tree <- adjust_tree(v,tree,rad, scale.x.real)  
  }

  print(raw.tree)
  print(tree)

  #remove any nodes that shouldn't be plotted
  # x0 <- 0
  x0 <- x1 <- 0
  y1 <- tree$length[tree$parent==-1]
  v$x[1] <- x1
  v$x1 <- x1
  tree$angle[tree$parent==-1] <- 0
  
  
  # if(len < tree$length[tree$parent==-1]){ #test if this is necessary
  #   len <- len+ (tree$length[tree$parent==-1] - len)+extra.len
  # }
  # # if(no_ccf ==TRUE){
  #   len <- tree$length[tree$parent==-1] + extra.len
  # }
  clone.out <- NULL
  wid <- wid*1/scale.x.real
  
  print("wid")
  print(wid)
  #make subclone polygons and place nodes
  clone.out <- draw.sample.clones(v=v,wid=wid,len=len,tree=tree,extra.len=extra.len,x=x1,y=y1, fixed_angle=fixed_angle, sig.shape=sig.shape,beta_in=beta_in,branching=branching, no_ccf=no_ccf, spread=spread)
  tree <- clone.out$tree
  print(tree)
  offset <- line.lwd*1/96
  #make lines for PGA
  tree.out <- add_segs3(tree, clone.out$v,offset=offset,scale.x.real=scale.x.real)
  tree_segs <- tree.out$tree_segs 

print("trees")
  print(tree.out)
  # print(tree_segs2) 

  tree_segs <- tree_segs[which(!(tree_segs$basey == tree_segs$tipy &  tree_segs$basex == tree_segs$tipx)),]
  
  #get lines for SNVs
  tree_segs2 = NULL

  if (!is.null(tree.out[[2]])){
    tree_segs2 <-  tree.out$tree_segs2
    tree_segs2 <- tree_segs2[which(!(tree_segs2$basey ==tree_segs2$tipy &  tree_segs2$basex == tree_segs2$tipx)),]
  }

print("trees")
  print(tree_segs)
  print(tree_segs2) 

  
  if( add.circle==TRUE && poly_tumour == TRUE){
    if(!('plot.lab' %in% colnames(clone.out$v))){
      clone.out$v$plot.lab   <- clone.out$v$lab
    }
    clone.out$v$plot.lab <- as.character(clone.out$v$plot.lab)
    
    # clone.out$v$circle <- sapply(clone.out$v$plot.lab, function(x) return(nchar(x)<3))
    # clone.out$v$circle[clone.out$v$lab == 1] <- NA
    clone.out$v$node <- sapply(clone.out$v$plot.lab, function(x) if(nchar(x)<3) return('circle') else return('ellipse'))
    clone.out$v$node[clone.out$v$lab == 1] <- 'rect'
    clone.out$v$plot.lab[clone.out$v$lab == 1] <- 'N' 
    clone.out$v$plot.lab[clone.out$v$lab != 1] <- clone.out$v$lab[clone.out$v$lab != 1]-1
    add.normal <- FALSE
  }

  #make labels for nodes
  if(add.labels){
  
    if("plot.lab" %in% colnames(clone.out$v)){ 
      lab <- clone.out$v$plot.lab
    } else{
        lab <- clone.out$v$lab
     }
    x <- clone.out$v$x
    y <- clone.out$v$y
    print("v")
    print(clone.out$v)
  }  

  #get the dimensions of the tree
  ymax <- max(clone.out$v$len) + clone.out$v$y[1]

  print("ymax")
  print(ymax)
  if(is.null(min_width)){
  # if(no_ccf==FALSE){
    xmax <- wid
    width <- wid*scale.x.real+axis.space.left + axis.space.right
    xlims <- c(-xmax/2-(axis.space.left*1/scale.x.real), xmax/2+axis.space.right*(1/scale.x.real))
   } else{
    
    xmin <- min(c(clone.out$v$x))
    xmax <- max(c(clone.out$v$x))
    xlims <- c(xmin-(axis.space.left*1/scale.x.real),xmax+axis.space.right*(1/scale.x.real))
    width <- (max(xlims)-min(xlims))*scale.x.real
    diff <- min_width - width
  
    if(diff>0){
      xmin <- xmin-0.5*diff*1/scale.x.real
      xmax <- xmax+0.5*diff*1/scale.x.real
      xlims <- c(xmin-(axis.space.left*1/scale.x.real),xmax+axis.space.right*(1/scale.x.real))
      width <- (max(xlims)-min(xlims))*scale.x.real

    }
  }

  height = (ymax+2)*scale.x.real 
  
  if(pga.cap==TRUE){
    y.axis.max <- 100
  }else{
    y.axis.max <- ymax
  }
  alternating <- 1
  ylab.right <- NULL
  ylab <- NULL

  # yat <- list(seq(0,y.axis.max,by=10))
  
  ylab.cex <- 0
  ylab.cex.right <- 0
  tck.right <- 0
  tck.left <- 0
  
  gene_grobs <- NULL


  
  print("clones")
  print(clone.out$v)

  print("trees")
  print(tree_segs)
  print(tree_segs2)

 

    panel.draw_tree <- function(...){ 
      lims <- current.panel.limits() 
      panel.xyplot(...) 

      if(no_ccf == FALSE){
        panel.clones(clones = clone.out$clones)
      }
      if(nrow(tree_segs)>0){
        grid.segments(x0=tree_segs$basex,y0=tree_segs$basey,x1=tree_segs$tipx, y1=tree_segs$tipy, default.units="native", gp=gpar(col=seg1.col,lwd=line.lwd))
     
      }
      #make lines for SNVs
      if(!is.null(tree_segs2) & nrow(tree_segs2) > 0 ){
        # grid.segments(y0=tree_segs2$basex,x0=tree_segs2$basey,y1=tree_segs2$tipx, x1=tree_segs2$tipy, default.units="native", gp=gpar(col=seg2.col,lwd=line.lwd))
        grid.segments(x0=tree_segs2$basex,y0=tree_segs2$basey,x1=tree_segs2$tipx, y1=tree_segs2$tipy, default.units="native", gp=gpar(col=seg2.col,lwd=line.lwd))
     
      }
      
       if(add.circle == TRUE ){
         upViewport()
         downViewport(trellis.vpname("panel",1,1,clip.off=TRUE))
         panel.cluster.ellipse(coords.df = clone.out$v, rad=rad)
         upViewport()
         downViewport(trellis.vpname("panel",1,1))
       # panel.cluster.pie(coords.df = clone.out$v, cluster_list=cluster_list, rad=rad)
      }


      if(add.labels == TRUE ){
        grid.text(lab, x=unit(x,"native"), y=unit(y,"native"),just=c("center","center"), gp=gpar(col='#FFFFFF',cex=node.cex))
        if(poly_tumour == TRUE){
          grid.text(lab[which(lab=='N')], x=unit(x[which(lab=='N')],"native"), y=unit(y[which(lab=='N')],"native"),just=c("center","center"), gp=gpar(col='black',cex=node.cex))
        }
      }

      if(add.normal ==TRUE){
        upViewport()
        downViewport(trellis.vpname("panel",1,1,clip.off=TRUE))
        grid.rect(x=unit(0,"native"), y=unit(-0.3-rad*1/scale.x.real,"native"),width=unit(2*rad,"inches"), height=unit(2*rad,"inches"), just=c("center","center"),gp=gpar(col="black",lwd=1.5,lty="31"))
        grid.text("N",x=unit(0,"native"),y=unit(-0.3-rad*1/scale.x.real,"native"),just="center",gp=gpar(col='black',cex=node.cex))
        upViewport()
        downViewport(trellis.vpname("panel",1,1))

      }

      #  add title
        upViewport()
        downViewport(trellis.vpname("panel",1,1,clip.off=TRUE))
        grid.text(samp_name,x=unit(0,"native"),y=unit(-1*title.y*1/scale.x.real,"native"),just=c("center","bottom"),gp=gpar(col='black',cex=title.cex))
        title.y.abs <<- convertY(unit(-1*title.y*1/scale.x.real,"native"),"inches",valueOnly=TRUE)
        print(convertY(unit(-1*title.y*1/scale.x.real,"native"),"inches"))
        upViewport()
        downViewport(trellis.vpname("panel",1,1))
        
        #bottom axis
        # if(no_ccf == FALSE){
          grid.lines(x=lims$xlim,y=lims$ylim[1],default.units="native",gp=gpar(col="black",alpha=1,lwd=1))
        # }
         if (!is.null(gene_grobs)){
          print("adding genes")
            add_genes(gene_grobs)
         }
        

    }

  #adjust axes to show PGA, SNVs, none, or both axes
 
  if (axis.type=="PGA"){
    panel.func <- function(...){ 
      lims <- current.panel.limits() 
      panel.draw_tree(...)
      grid.remove(trellis.grobname(name="ticks.right","panel",column=1,row=1))
      grid.remove(trellis.grobname(name="ticks.top","panel",column=1,row=1))
      #left axis
      grid.lines(x=lims$xlim[1],y=lims$ylim,default.units="native",gp=gpar(col="black",alpha=1,lwd=2.5))
      
      # panel.abline(v=lims$xlim[1],col="black",alpha=1,lwd=2.5)
      
    }
    ylab <- "PGA"
    yat <- seq(0,y.axis.max,by=pga.interval)
    y.tck.lab <- yat
    #	ylab <- "Percent Genome Aberrated"
    ylab.cex <- lab.cex
    tck.left <- 1
    # title.pos <-  unit(w.padding+width/2,"inches") 
    
  }
  
  if (axis.type=="SNV"){
    panel.func <- function(...){ 
      lims <- current.panel.limits() 
      panel.draw_tree(...) 
      grid.remove(trellis.grobname(name="ticks.left","panel",column=1,row=1))
      grid.remove(trellis.grobname(name="ticks.top","panel",column=1,row=1))
      #right axis
      grid.lines(x=lims$xlim[2],y=lims$ylim,default.units="native",gp=gpar(col="black",alpha=1,lwd=2.5))
      # panel.abline(v=lims$xlim[2],col="black",alpha=1,lwd=2.5)
      
    }

    ylab <- NULL
    alternating <- 2
    ylab.right <- "SNVs"
    ylab.cex.right <- lab.cex
    tck.right <- 1
    y.tck.snv <- seq(0,(ymax*scale.x.real/scale2),by=snv.interval)
    # y.tck.lab <- list(y.tck.snv)
    y.tck.lab <- y.tck.snv
    print(y.tck.lab)
    
    # yat <- list(y.tck.snv*scale2/scale.x.real)
    yat <- y.tck.snv*scale2/scale.x.real
    print(yat)
    
  }

  if (axis.type == "SNV single"){
    panel.func <- function(...){ 
      lims <- current.panel.limits() 
      panel.draw_tree(...)
      grid.remove(trellis.grobname(name="ticks.right","panel",column=1,row=1))
      # grid.remove(trellis.grobname(name="ticks.top","panel",column=1,row=1))
      #left axis
      grid.lines(x=lims$xlim[1],y=lims$ylim,default.units="native",gp=gpar(col="black",alpha=1,lwd=2.5))    
      # panel.abline(v=lims$xlim[1],col="black",alpha=1,lwd=2.5)      
    }
    ylab <- "SNVs"
    ylab.cex <- lab.cex
    y.tck.snv <- seq(0,(ymax*scale.x.real/scale2),by=snv.interval)
    yat <- y.tck.snv*scale2/scale.x.real
    y.tck.lab <- y.tck.snv
    print(y.tck.lab)
    
  }


  if (axis.type == "single"){
    panel.func <- function(...){ 
      lims <- current.panel.limits() 
      panel.xyplot(...) 
      grid.remove(trellis.grobname(name="ticks.right","panel",column=1,row=1))
      grid.remove(trellis.grobname(name="ticks.top","panel",column=1,row=1))
      if (!is.null(gene_grobs)){
        add_genes(gene_grobs)
      }
      grid.lines(x=lims$xlim[1],y=lims$ylim,default.units="native",gp=gpar(col="black",alpha=1,lwd=2.5))
      grid.lines(x=lims$xlim[1],y=lims$ylim,default.units="native",gp=gpar(col="black",alpha=1,lwd=2.5))
      grid.lines(x=lims$xlim,y=lims$ylim[1],default.units="native",gp=gpar(col="black",alpha=1,lwd=2.5))
      
      panel.clones(clones = clone.out$clones)
      panel.cluster.ellipse(coords.df = clone.out$v, rad=rad)

      if(nrow(tree_segs)>0){
        grid.segments(x0=tree_segs$basey,y0=tree_segs$basex,x1=tree_segs$tipy, y1=tree_segs$tipx, default.units="native", gp=gpar(col=seg1.col,lwd=line.lwd))
     
      }
      #make lines for SNVs
      if(!is.null(tree_segs2) ){
        grid.segments(x0=tree_segs2$basey,y0=tree_segs2$basex,x1=tree_segs2$tipy, y1=tree_segs2$tipx, default.units="native", gp=gpar(col=seg1.col,lwd=line.lwd))
     
      }
      if(add.labels ==TRUE ){
        grid.text(lab, x=unit(y,"native"), y=unit(x,"native"),just=c("center","center"), gp=gpar(col='#FFFFFF',cex=1.4))
      }
      if(add.normal ==TRUE){
        upViewport()
        downViewport(trellis.vpname("panel",1,1,clip.off=TRUE))
        grid.rect(x=unit(0,"native"), y=unit(-0.3-rad*1/scale.x.real,"native"),width=unit(2*rad,"inches"), height=unit(2*rad,"inches"), just=c("center","center"),gp=gpar(col="black",lwd=2,lty="31"))
        grid.text("N",x=unit(0,"native"),y=unit(-0.3-rad*1/scale.x.real,"native"),just="center",gp=gpar(col='black',cex=1.4))
        upViewport()
        downViewport(trellis.vpname("panel",1,1))

      }

    }
   # ylab <- NULL
    alternating <- 1
    ylab <- "SNV"
    ylab.cex <- lab.cex
    # tck.right <- 1
    y.tck.snv <- seq(0,ymax,by=snv.interval)
    # y.tck.lab <- list(y.tck.snv)
    y.tck.lab <- y.tck.snv

    # yat <- list(y.tck.snv*scale2/scale.x.real)
    yat <- y.tck.snv
   
  }
  
  # browser()
  if (axis.type=="both"){
    panel.func <- function(...){ 
      lims <- current.panel.limits() 
      panel.draw_tree(...) 
      grid.lines(x=lims$xlim[1],y=lims$ylim,default.units="native",gp=gpar(col="black",alpha=1,lwd=1))
      grid.lines(x=lims$xlim[2],y=lims$ylim,default.units="native",gp=gpar(col="black",alpha=1,lwd=1))
      
      if(no_ccf == FALSE){
        grid.remove(trellis.grobname(name="ticks.top","panel",column=1,row=1))
      }else{
        # grid.remove(trellis.grobname(name="ticks.top","panel",column=1,row=1))
        # grid.remove(trellis.grobname(name="ticks.bottom","panel",column=1,row=1))
      }
      if (!is.null(gene_grobs)){
        add_genes(gene_grobs)
      }

             
      
    }
    ylab <- "PGA"
    alternating <- 3
    ylab.right <- "SNVs"
    ylab.cex <- lab.cex
    ylab.cex.right <- lab.cex
    tck.right <- 1
    yat <- seq(0,y.axis.max,by=pga.interval)
    y.tck.lab <- yat
    y.tck.snv <- seq(0,(ymax*scale.x.real/scale2),by=snv.interval)
    print(y.tck.lab)
    print(yat)
    w.padding <- w.padding+w.padding*.265
    yscale.components.CF <-
      function(...)
      {
        ans <- yscale.components.default(...)
        print("ans")
        print(ans)
        print("end")
        ans$right <- ans$left
        ans$right$labels$labels <- seq(0,(ymax*scale.x.real/scale2),by=snv.interval)
        ans$right$ticks$at <- y.tck.snv*scale2/scale.x.real
        ans$right$labels$at <- y.tck.snv*scale2/scale.x.real
        return(ans)
      }
   
  }
  
  
  if (axis.type=="none"){
    if(no_ccf ==FALSE){
      panel.func <- function(...){ 
        lims <- current.panel.limits() 
        panel.draw_tree(...) 
        grid.remove(trellis.grobname(name="ticks.top","panel",column=1,row=1))
      }
    } else{
      panel.func <- function(...){ 
        lims <- current.panel.limits() 
        panel.draw_tree(...) 
        # browser() 
        # grid.remove(trellis.grobname(name="abline.h","panel",column=1,row=1))

        if (!is.null(gene_grobs)){
          add_genes(gene_grobs)
        }
     
      }
    }
    yat <- "c()"
    y.tck.lab <-   FALSE
    alternating <- 0
  }


  print(c("ylab ", ylab))
  print("yat")
  print(yat)
  print("xmax")
  print(xmax)
  print("xlims")
  print(xlims) 
  print("width")
  print(width)  
  print("height")
  print(height)
  print("ccf")
  print(no_ccf)
  
  #base lattice call
    
  print(title.y)
  print(samp_name)
  title.y.abs <- numeric()
  # browser()
  clone.plot <- xyplot(x~y,
                       data=data.frame(x=1,y=1) ,
                       ylim=c(ymax,-2),
                       xlim=xlims,
                       type="n", 
                       xlab= if(xlab != "") list(label=xlab,cex=lab.cex,y=0.8) else NULL, 
                       ylab.right=list(label=ylab.right, cex= ylab.cex.right),
                       ylab=list(label=ylab,cex=ylab.cex),
                       scales=list(cex=axis.cex,col=1,
                       x= if(no_ccf == FALSE) {list(
                               # at=seq(-ymax/2-axis.space.left,ymax/2+axis.space.right,by=3),
                               # rot=90
                              at=c(-xmax/2,xmax/2),
                              labels=c(0,paste0(round(max(clone.out$v$ccf)*100,0),'%'))
                              )} else{
                        # list(at=seq(-xmax-axis.space.left,xmax+axis.space.right,by=5),rot=90)},
                        list( at=c(), labels=FALSE, alternating=0)},
                       # y=list(at=seq(ymax,-2,-5)),alternating=alternating),
                       y=list(alternating= alternating,labels=y.tck.lab, at = yat)),
                       yscale.components = ifelse(axis.type=="both", yscale.components.CF,yscale.components.default),
                       par.settings = list(axis.line = list(col = 'transparent')),
                        cex.lab = 3 , 
                       panel = panel.func) 
# 
  
  if(grepl(".pdf",filename)){
    trellis.device(device="pdf",file=filename,height=height+2*h.padding ,width=width+2*w.padding,onefile=FALSE)
  } else if (grepl(".png", filename)){
    trellis.device(device="png",file=filename,height=height+2*h.padding ,width=width+2*w.padding,unit="in", res=200 )
  }

  #annotate genes
  if(add.genes==TRUE){
    gene_grobs <- add_text2(tree,genes.df,label_nodes=label_nodes,line.dist=line.dist,title.y = title.y, panel_height=height, panel_width=width, xlims=xlims,ymax=ymax,cex=gene.cex,v=clone.out$v,axis.type=axis.type,rad=rad, scale=scale.x.real,alternating=alternate.genes)
    gene_grobs <- gene_grobs[[1]]
    print("gene_grobs")
    print(gene_grobs)
  }

  hjust <- ifelse(grepl("TITAN",samp_name),"right","left")
  xloc <- ifelse(hjust=="right",1,0)

  par(mar=c(1,1,1,1))
  plot.new()
  print(clone.plot,panel.width=list(width,"inches"),panel.height=list(height,"inches"))
  tree_gTree<- grid.grab()
  tree.gTree <- gTree(children=gList(tree_gTree),
  vp=viewport(height=unit(height+2*h.padding,"inches") ,width=unit(width+2*w.padding,"inches"),
    name="myVP",just=c(hjust,"top"),x=unit(xloc,"npc"),y=unit(1,"npc"),gp=gpar(col="red")))
  
  dev.off()

  # test_ag <- arrangeGrob(tree.gTree)
  # pdf("test.pdf",onefile=FALSE)
  # plot(test_ag)
  # dev.off()


    return(list(clone.out,tree.out,clone.plot, tree.gTree))
}


add_genes <- function(gene_grobs){
  upViewport()
  downViewport(trellis.vpname("panel",1,1,clip.off=TRUE))
  grid.draw(gene_grobs)
  upViewport()
  downViewport(trellis.vpname("panel",1,1))
}
