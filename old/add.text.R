

add_text <- function(tree,genes,cex=1,line.dist=0.5,v=NULL, panel_height=NULL, panel_width=NULL,ymax=ymax, xmax=xmax,axis.type=NULL,rad=NULL,scale=NULL,alternating=TRUE, split=TRUE){
 # browser()
  gene.list <- alply(seq_len(nrow(tree)), 1,function(x) return(character()))
  gene.col <- gene.list

  a_ply(seq_len(nrow(genes)), 1, function(x){
    gene.row <- genes[x,]
    pos <- which(tree$tip==gene.row$node)
    gene <- gene.row$gene
    if(length(grep("_",gene))>0){
      gene.split <-  strsplit(gene,split="_")[[1]]
      gene.name <- gene.split[1]
      amp <- gene.split[2]
      call <- paste0(gene.name,"^\"A",amp,"\"")
      gene <- parse(text=call)
    }
    
    gene.list[[pos]] <<- c(gene.list[[pos]],gene)
    if(!is.na(gene.row$cn)){
       gene.col[[pos]] <<- c(gene.col[[pos]],ifelse(gene.row$cn=="loss" | gene.row$cn<2,"blue","red"))
    }else{
      gene.col[[pos]] <<- c(gene.col[[pos]],'black')
    }
  })
  
  
   tree.max <- adply(tree, 1,function(x){
      if(x$parent ==-1){
        basex=0;
        basey=0
      } else{
        basex <- v$x[v$lab== x$parent]; 
        basey <- v$y[v$lab== x$parent]; 
      }
      tipx  <- v$x[v$lab==x$tip]
    tipy  <- v$y[v$lab==x$tip]
    return(data.frame(basex,basey,tipx,tipy))
  })
 # browser() 
   trellis.focus("panel",1,1,clip.off=TRUE)
  # pushViewport(viewport(height=unit(panel_height,"inches"), width=unit(panel_width,"inches")))
  # pushViewport(viewport(height=unit(panel_height,"inches"), width=unit(panel_width,"inches"),xscale=c(-ymax/2-2,ymax/2+2),yscale=c(xmax,-2)))
  #  #  downViewport("plot_01.panel.1.1.vp")
  
  y.offset <- 
  x0 <- convertX(unit(tree.max$basey,"native"),"inches",valueOnly=TRUE)
  x1 <- convertX(unit(tree.max$tipy,"native"),"inches",valueOnly=TRUE)
  y0 <- convertY(unit(tree.max$basex,"native"),"inches",valueOnly=TRUE) 
  y1 <- convertY(unit(tree.max$tipx,"native"),"inches",valueOnly=TRUE)

  #upViewport()
 

#iterate over each node
  for (s in seq_along(gene.list)){
    if(length(gene.list[[s]])==0){
      next;
      } else{
        print(s)
      slope = (y1[s] - y0[s])/(x1[s]-x0[s])
      #where would the line intercept the x-axis
      intercept = y1[s]-slope*x1[s]
      y.height = y0[s] - y1[s]
      str.heights <- sapply(gene.list[[s]], function(x) strheight(x,unit="inches",cex=cex))
      str.heightsum <- sum(str.heights)+0.03*length(str.heights)-0.03
      label.bottom  <-y.height/2-str.heightsum/2 +y1[s]
      if(s==1 & str.heightsum > y.height & !is.null(rad) & !is.null(scale)){
        label.bottom <- y1[s] - rad*scale
      }
        for (g in rev(seq_along(gene.list[[s]]))){
          heights = ifelse((g-1)==0,0,sum(str.heights[c(1:(g-1))]))
          ypos = label.bottom + (g-1)*0.03+heights;
          #back computing the x position based on the intercept and the slope
          xpos = ifelse(is.infinite(slope),x0[s],(ypos-intercept)/slope)
        if( alternating==TRUE ){
          if (s%%2>0 ){
            grid.text(gene.list[[s]][g],x=unit(xpos-line.dist,"inches"),y=unit(ypos,"inches"),just=c("right","bottom"), gp=gpar(col=gene.col[[s]][g],cex=cex))
          } else{
            grid.text(gene.list[[s]][g],x=unit(xpos+line.dist,"inches"),y=unit(ypos,"inches"),just=c("left","bottom"), gp=gpar(col=gene.col[[s]][g],cex=cex))
          }}
          else if( split==TRUE &  s==1 & length(gene.list[[s]])>5 & nrow(tree)>2 & 'red' %in% gene.col[[s]] & 'blue' %in% gene.col[[s]]){
          if (gene.col[[s]][g] =='blue' ){
            # browser()
            offset_blue <- length(which(gene.col[[s]]=='blue'))
            heights = ifelse((g-1)==0,0,sum(str.heights[c(1:(g-offset_blue-1))]))
            ypos = label.bottom + (g-offset_blue-1)*0.03+heights;
            grid.text(gene.list[[s]][g],x=unit(xpos-line.dist,"inches"),y=unit(ypos,"inches"),just=c("right","bottom"), gp=gpar(col=gene.col[[s]][g],cex=cex))
          } else{
            grid.text(gene.list[[s]][g],x=unit(xpos+line.dist,"inches"),y=unit(ypos,"inches"),just=c("left","bottom"), gp=gpar(col=gene.col[[s]][g],cex=cex))
          }
          
        }else{
           if (slope > 0 | (is.infinite((slope)) & axis.type=="SNV" ) ){
            grid.text(gene.list[[s]][g],x=unit(xpos-line.dist,"inches"),y=unit(ypos,"inches"),just=c("right","bottom"), gp=gpar(col=gene.col[[s]][g],cex=cex))
            } else{
              grid.text(gene.list[[s]][g],x=unit(xpos+line.dist,"inches"),y=unit(ypos,"inches"),just=c("left","bottom"), gp=gpar(col=gene.col[[s]][g],cex=cex))
            }
            
        }  
      }
      }
  }
  trellis.unfocus()
#upViewport()
  }

