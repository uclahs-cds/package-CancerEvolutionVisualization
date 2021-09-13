draw.my.tree <- function( in.tree.df=NULL, tree = NULL, genes.df = NULL, samp_name=NULL, rad=1.2,filename = "myplot.pdf", scale.x.real = 0.05443424, 
                          scale2=0.5/362, wid=60, extra.len=10, len=55,sig.shape=3, offset=0.3, dev.width=3.342857, gene.cex=1.17,
                          seg1.col="black",seg2.col="green", add.circle=TRUE, add.genes=TRUE, length_from_edge=TRUE,add.labels=TRUE,branching=TRUE,
                          axis.type='PGA',w.padding=0.3,h.padding=0.3,lab.cex =1.55,lin.width, line.dist=0.1,circle.col="grey29",line.lwd=3,title.cex=1.7,curve=3,
                          axis.space.left=0, axis.space.right=0,alternate.genes=FALSE, axis.cex =1.45, snv.interval=400,pga.cap=TRUE, 
                          title.y=0.3,vp=NULL,position=c(0,0,1,1),add.normal=TRUE,in.title.pos=NULL){
  

 
  #initializing dataframe for subclones
  v <- in.tree.df[!in.tree.df$excluded,]
  v <- v[order(v$lab),]
  # v$vaf[!v$excluded] <- v$ccf[!v$excluded]*0.5/max(v$ccf[!v$excluded])
  v$vaf[!v$excluded] <- v$ccf[!v$excluded]/max(v$ccf[!v$excluded])
  v$x <- numeric(length(nrow(v)))
  v$y <- numeric(length(nrow(v)))
  v$len <- numeric(length(nrow(v)))
  v$y.shift <- numeric(length(nrow(v)))
  v$y.mid <- numeric(length(nrow(v)))
  
  beta_in <- curve
  
  #initializing line segment dataframe and adjusting lengths to accomodate the node circles
  tree$angle <- numeric(length=nrow(tree))
  tree$length2.c <- tree$length2/scale.x.real*scale2
  tree$length <- apply(tree, 1,function(x) max(x[c(3,6)]))
  raw.tree <- tree
  if(add.circle==TRUE && length_from_edge==TRUE){
    tree <- adjust_tree(in.tree.df,tree,rad)  
  }

  #remove any nodes that shouldn't be plotted
  if(in.tree.df$excluded[in.tree.df$lab==-1]==TRUE){
    x0<-0
    y0<-y1<-0
    x1<-tree$length[tree$parent==-1]
    v$x[1]<- x1
    v$y1 <- y1
    tree$angle[tree$parent==-1] <- 0
  }
  
  #if the tree is linear the width is fixed
  if(branching==FALSE){
    wid=lin.width*1/scale.x.real*2
  }
  
  assign("v",v,envir=.GlobalEnv)
  assign("tree",tree,envir=.GlobalEnv)
  
  if(len < tree$length[tree$parent==-1]){
    len <- len+ (tree$length[tree$parent==-1] - len)+extra.len
  }
    
  #make subclone polygons
  clone.out <- draw.sample.clones(v=v,wid=wid,len=len,extra.len=extra.len,tree=tree,x=x1,y=y1,sig.shape=sig.shape,beta_in=beta_in,branching=branching)
  tree <- get("tree",envir=.GlobalEnv)

  print(tree)
  #make lines for PGA
  tree.out <- add.segs(tree,offset=offset)
  tree_segs <- tree.out$tree_segs

  print(tree_segs)
  tree_segs <- tree_segs[which(tree_segs$basex !=tree_segs$tipx),]
  #add them to the plotting call
  if(nrow(tree_segs)>0){
    clone.out$ex <- c(clone.out$ex, paste0("layer( panel.segments(x0=tree_segs$basey,y0=tree_segs$basex,x1=tree_segs$tipy, y1=tree_segs$tipx, col=\"",seg1.col,"\",lwd=",line.lwd,"),data=tree_segs)"))
  }
  #make lines for SNVs
  if (length(tree.out)==2){
    tree_segs2 <-  tree.out$tree_segs2
    tree_segs2 <- tree_segs2[which(tree_segs2$basex !=tree_segs2$tipx),]
    if(nrow(tree_segs2)>0){
      clone.out$ex <- c(clone.out$ex, paste0("layer( panel.segments(x0=tree_segs2$basey,y0=tree_segs2$basex,x1=tree_segs2$tipy, y1=tree_segs2$tipx, col=\"",seg2.col,"\",lwd=",line.lwd,"),data=tree_segs2)"))
    }
    }
  #make circles for nodes
  if(add.circle==TRUE){
    print(str(clone.out))
    circle.coords <- add.circles(clone.out$coords.df,rad=rad)
    for (n in 1:length(circle.coords)){
      clone.out$ex <- c(clone.out$ex, paste0("layer(panel.polygon(x=circle.coords[[",n,"]]$y,y=circle.coords[[",n,"]]$x,col=\"",circle.col,"\",border=\"transparent\",font=2),data=circle.coords)"))
    }
  }

  clone.out$v <- get("v",envir=.GlobalEnv)
  #get the dimensions of the tree
  xmax <- max(clone.out$coords.df$len)
  print(xmax)
  ymax = wid 
  height = (xmax+2)*scale.x.real
  
  
  if(branching==TRUE){
    width = wid*scale.x.real+scale.x.real*axis.space.left + scale.x.real*axis.space.right
  } else{
    width=lin.width+scale.x.real*axis.space.left + scale.x.real*axis.space.right
    ymax = lin.width*1/scale.x.real
  }
  
  if(pga.cap==TRUE){
    y.axis.max=100
  }else{
    y.axis.max=xmax
  }
  alternating <- 1
  ylab.right <- NULL
  ylab <- NULL
  yat <- list(seq(0,y.axis.max,by=10))
  y.tck.lab <- yat
  ylab.cex <- 0
  ylab.cex.right <- 0
  tck.right <- 0
  tck.left <- 0
  
  #adjust axes to show PGA, SNVs, none, or both axes
  if (axis.type=="PGA"){
    panel.func <- function(...){ 
      lims <- current.panel.limits() 
      panel.xyplot(...) 
      panel.abline(v=lims$xlim[1],col="black",alpha=1,lwd=2.5)
    }
    ylab <- "PGA"
 #	ylab <- "Percent Genome Aberrated"
    ylab.cex <- lab.cex
    tck.left <- 1
    title.pos <- paste0(w.padding+width/2,",\"inches\"")
  }
  
  if (axis.type=="SNV"){
    panel.func <- function(...){ 
      lims <- current.panel.limits() 
      panel.xyplot(...) 
      
      panel.abline(v=lims$xlim[2],col="black",alpha=1,lwd=2.5) 
      }
    ylab <- NULL
    alternating <- 2
    ylab.right <- "SNVs"
    ylab.cex.right <- lab.cex
    title.pos <- paste0(0.3,",\"npc\"")
    # title.pos <- paste0(w.padding+width/2,",\"inches\"")
    tck.right <- 1
    y.tck.snv <- seq(0,(xmax*scale.x.real/scale2),by=snv.interval)
    y.tck.lab <- list(y.tck.snv)
    print(y.tck.lab)
    yat <- list(y.tck.snv*scale2/scale.x.real)
    print(yat)
    
  }
  
  
  if (axis.type=="both"){
    panel.func <- function(...){ 
      lims <- current.panel.limits() 
      panel.xyplot(...) 
      
      panel.abline(v=lims$xlim[2],col="black",alpha=1,lwd=2.5) 
      panel.abline(v=lims$xlim[1],col="black",alpha=1,lwd=2.5) 
      
    }
    ylab <- "PGA"
    alternating <- 3
    ylab.right <- "SNVs"
    ylab.cex <- lab.cex
    ylab.cex.right <- lab.cex
    title.pos <- paste0(0.5,",\"npc\"")
    tck.right <- 1
    y.tck.snv <- seq(0,(xmax*scale.x.real/scale2),by=snv.interval)
     print(y.tck.lab)
    print(yat)
 yscale.components.CF <-
    function(...)
	{
    ans <- yscale.components.default(...)
    print("ans")
    print(ans)
   print("end")
    ans$right <- ans$left
    ans$right$labels$labels <- seq(0,(xmax*scale.x.real/scale2),by=snv.interval)
    ans$right$ticks$at <- y.tck.snv*scale2/scale.x.real
	 ans$right$labels$at <- y.tck.snv*scale2/scale.x.real
  return(ans)
   }
   
  }
  
  
  if (axis.type=="none"){
     panel.func <- function(...){ 
       lims <- current.panel.limits() 
       panel.xyplot(...) 
     
     }
    yat <- "c()"
    y.tck.lab <-   FALSE
    title.pos <- paste0("0.5, \"npc\"")
    alternating <- 0
  }
 print(c("ylab ", ylab))

 
 if(!is.null(in.title.pos)){
   title.pos <- paste0(in.title.pos, ",\'npc\'")
 }
 #base lattice call
clone.plot = paste0("xyplot(x~y,data=data.frame(x=1,y=1) ,ylim=c(",xmax,",-2),xlim=c(",-ymax/2-axis.space.left,",",ymax/2+axis.space.right,"),type=\"n\", ",
                       "xlab=\'\', ylab.right=list(label=\'",ylab.right,"\',cex=",ylab.cex.right,"), ylab=list(label=\'",ylab,"\',cex=",ylab.cex,"),",
                       "scales=list(cex=",axis.cex,",col=1,x=list(at=seq(-ymax/2-axis.space.left,ymax/2+axis.space.right,3),rot=90),",
                       "y=list(alternating= ",alternating,", at=seq(-2,xmax,3))), " ,
                       "par.settings = list(axis.line = list(col = 0),par.main.text=list(font=1,just=\"center\",x=grid::unit(",title.pos,"),y=grid::unit(",title.y,",\"in\"),cex=",title.cex,")),",
                       "main = samp_name, cex.lab=3 ,",
                       "panel=panel.func)")
# clone.plot = paste0("xyplot(x~y,data=data.frame(x=1,y=1) ,ylim=c(",xmax,",-2),xlim=c(",-ymax/2-axis.space.left,",",ymax/2+axis.space.right,"),type=\"n\", ",
#                        "xlab=\'\', ylab.right=list(label=\'",ylab.right,"\',cex=",ylab.cex.right,"), ylab=list(label=\'",ylab,"\',cex=",ylab.cex,"),",
#                        "scales=list(cex=",axis.cex,",col=1,x=list(at=c(-ymax/2,ymax/2),labels=c(0,paste0(round(max(v$ccf)*100,0),'%'))),",
#                        "y=list(alternating= ",alternating,",labels=",y.tck.lab,", at=",yat,")), " ,
#                        "par.settings = list(axis.line = list(col = 0),par.main.text=list(font=1,just=\"center\",x=grid::unit(",title.pos,"),y=grid::unit(",title.y,",\"in\"),cex=",title.cex,")),",
#                        "main = samp_name, cex.lab=3 ,",
#                        "panel=panel.func)")
if(axis.type=="both"){ clone.plot = paste0("xyplot(x~y,data=data.frame(x=1,y=1) ,ylim=c(",xmax,",-2),xlim=c(",-ymax/2-axis.space.left,",",ymax/2+axis.space.right,"),type=\"n\", ",
                       "xlab=\'\', ylab.right=list(label=\'",ylab.right,"\',rot=270,cex=",ylab.cex.right,"), ylab=list(label=\'",ylab,"\',cex=",ylab.cex,"),",
                       "scales=list(cex=",axis.cex,",col=1,x=list(at=c(-ymax/2,ymax/2),labels=c(0,paste0(round(max(v$ccf)*100,0),'%'))),",
                       "y=list(alternating= ",alternating,",labels=",y.tck.lab,", at=",yat,")), " , "yscale.components = yscale.components.CF ,
                       par.settings = list(axis.line = list(col = 0),par.main.text=list(font=1,just=\"center\",x=grid::unit(",title.pos,"),y=grid::unit(",title.y,",\"in\"),cex=",title.cex,")),",
                       "main = samp_name, cex.lab=3 ,",
                       "panel=panel.func)")


}
  
  ex.all = clone.plot
  #add the polygon, segment, and circle panel calls
  for (ex in clone.out$ex){
    ex.all <- paste( ex.all,ex,sep='+')
  }
  
  #put back the bottom axis in case there is no left and right axis
  if(!(all(in.tree.df$bell==FALSE) & axis.type =="none") ){
  ex.all <- paste0(ex.all,"+layer(panel.abline(h=",xmax,",alpha=1,lwd=2.75))")
  }
  print(parse(text=clone.plot))
  assign("clones",clone.out$clones,envir=.GlobalEnv)
  assign("tree_segs",tree_segs,envir=.GlobalEnv)
  assign("tree_segs2",tree_segs2,envir=.GlobalEnv)
  if(add.circle==TRUE){
    assign("circle.coords",circle.coords,envir=.GlobalEnv)
    assign("circle.coords",circle.coords,envir=.GlobalEnv)
  }
  #execute the lattice call and get abacl a trellis object
  obj <- eval(parse(text=ex.all))

if(is.null(vp)){
  trellis.device(device="pdf",file=filename,height=height+2*h.padding ,width=width+2*w.padding,onefile=FALSE)
  # trellis.device(device="png",file=filename,height=height+2*h.padding ,width=width+2*w.padding, res=400,units="in")
  print(obj,panel.width=list(width,"inches"),panel.height=list(height,"inches"),position=position)
}
  # if(axis.type=="PGA"){
  #   grid.remove("plot_01.ticks.right.panel.1.1")
  # }title.pos <- paste0(w.padding+width/2,",\"inches\"")
  # if(axis.type=="SNV"){
  #  grid.remove("plot_01.ticks.left.panel.1.1")
  # }
  # if(all(in.tree.df$bell==FALSE)){
  #   grid.remove("plot_01.ticks.bottom.panel.1.1")
  #   grid.remove("plot_01.ticklabels.bottom.panel.1.1")
  #   grid.remove("plot_01.border.panel.1.1")
  # }
  # grid.remove("plot_01.ticks.top.panel.1.1")
else{

    print(obj,panel.width=list(width,"inches"),panel.height=list(height,"inches"),newpage=FALSE)

  }
  #clean up the axes
    if(axis.type=="PGA"){
    grid.remove(trellis.grobname(name="ticks.right","panel",column=1,row=1))
    }
  if(axis.type=="SNV" ){
    grid.remove(trellis.grobname(name="ticks.left","panel",column=1,row=1))
      }
  if(all(in.tree.df$bell==FALSE)){
    grid.remove(trellis.grobname(name="ticks.bottom","panel",column=1,row=1))
    grid.remove(trellis.grobname(name="tickslabels.bottom","panel",column=1,row=1))
    grid.remove(trellis.grobname(name="border","panel",column=1,row=1))
  }

    grid.remove(trellis.grobname(name="ticks.top","panel",column=1,row=1))

  #annotate genes
  if(add.genes==TRUE){
    add_text(tree,genes.df,line.dist=line.dist,cex=gene.cex,clone.out$v,axis.type=axis.type,rad=rad, scale=scale.x.real,alternating=alternate.genes)
  }
  #annotate nodes
  if(add.labels){
    trellis.focus("panel",1,1,clip.off=TRUE)
   

    x.real <- convertX(unit(wid/4,"native"),"inches",valueOnly=TRUE) - convertX(unit(-wid/4,"native"),"inches",valueOnly=TRUE)
    scale.x.real <- x.real/(wid/2)
    print(scale.x.real)
    y.real <- convertY(unit(0,"native"),"inches",valueOnly=TRUE) - convertY(unit(xmax,"native"),"inches",valueOnly=TRUE)
    print(y.real)
    scale.y.real<- y.real/xmax
    print(scale.y.real)
    d_ply(clone.out$v, .(lab), function(x) { grid.text(x$lab[1],x=unit(x$y[1],"native"),y=unit(x$x[1],"native"),just="center",gp=gpar(col='#FFFFFF',cex=1.4))})
    if(add.normal ==TRUE){
      grid.rect(x=unit(0,"native"), y=unit(-0.3,"native"),width=unit(2*rad,"native"), height=unit(2*rad,"native"), just=c("center","top"),gp=gpar(col="black",lwd=2,lty="31"))
      grid.text("N",x=unit(0,"native"),y=unit(-0.3-rad,"native"),just="center",gp=gpar(col='black',cex=1.4))
    }
    trellis.unfocus()
    }
  #
  if(is.null(vp)){
  dev.off()
  }
  return(list(clone.out,tree.out))
}

