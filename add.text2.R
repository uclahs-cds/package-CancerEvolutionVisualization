
axis_overlap <- function(xpos,gene,line.dist, axis.type, cex,panel_width, return_cex=FALSE){
  # function checks if the gene label will cross over the axis
  # browser()

  gene.xrange <- sort(c(xpos+line.dist, xpos+line.dist+ strwidth(gene,unit="inches",cex=cex)*line.dist/abs(line.dist)))
  overlaps <- NULL
  if(axis.type == "PGA" | axis.type == "SNV single" | axis.type == "left"){
    if(any(gene.xrange < 0)){
      overlaps <- TRUE
    }    
  } else if(axis.type == "SNV"){
   if(any(gene.xrange) > panel_width){
     overlaps <- TRUE
   }  
 
  } else if(axis.type == "both"){
   if(any(gene.xrange > panel_width) | any(gene.xrange < 0)){
     overlaps <- TRUE
   } 
   
  } 
   
   if(return_cex & !is.null(overlaps)){ #find a text size that prevents the axis overlap
    new_cex <- cex 

    while(!is.null(overlaps)){
      new_cex <- new_cex - 0.05
      overlaps <- axis_overlap(xpos, gene, line.dist, axis.type, new_cex, panel_width)
     }
   return(new_cex)
   } 
  return(overlaps)
}

check_overlap <- function(xpos,ypos,gene,line.dist, tree.max.adjusted, cex,rad){
  # function checks if the gene label will cross over any of the branch lines

  gene.xrange <- sort(c(xpos, xpos+line.dist/abs(line.dist)*(strwidth(gene,unit="inches",cex=cex))))
  
  node_segs <- adply(tree.max.adjusted[,c('tip','parent','x','y')], 1, function(w){
                                                       data.frame(y0 = (w$y+rad), y1 =(w$y-rad), x0=(w$x-rad), x1=(w$x+rad))
                                            })


  line.intercept <- logical(length=nrow(tree.max.adjusted))
  node.intercept <- logical(length=nrow(tree.max.adjusted))

  for (i in seq_along(tree.max.adjusted[,1])){
    if(is.infinite(tree.max.adjusted$slope[i])){ #only overlaps with straight lines if the line's ypos is in the range
      line.intercept[i] <- ifelse(((ypos < tree.max.adjusted$y0[i]) & (ypos > tree.max.adjusted$y1[i]) & (gene.xrange[1]<tree.max.adjusted$x0[i]) &  (gene.xrange[2]>tree.max.adjusted$x0[i])),TRUE,FALSE)
    }else{
      line.intercept.x <- (ypos-tree.max.adjusted$intercept[i])/tree.max.adjusted$slope[i]
  
      if(line.intercept.x < max(c(tree.max.adjusted$x0[i], tree.max.adjusted$x1[i])) & line.intercept.x > min(c(tree.max.adjusted$x0[i], tree.max.adjusted$x1[i])) & (line.intercept.x > gene.xrange[1]) & (line.intercept.x < gene.xrange[2])){
        line.intercept[i]<- TRUE
      }
    }
    node.intercept[i] <- ifelse(((ypos<node_segs$y0[i]) & (ypos>node_segs$y1[i]) & (gene.xrange[1]<node_segs$x0[i]) &  (gene.xrange[2]>node_segs$x0[i])),TRUE,FALSE)
  }

 intercepts_lines <- tree.max.adjusted$tip[line.intercept]
 intercepts_nodes <- tree.max.adjusted$tip[node.intercept]
 return(list(lines=intercepts_lines, nodes=intercepts_nodes))

}


position_genes <- function(tree.max.adjusted=NULL, gene.list=NULL, gene.col=NULL, axis.type=axis.type, panel_height=NULL, panel_width=NULL, title.y=NULL, line.dist=line.dist, cex=NULL, rad=rad, alternating=FALSE, split=FALSE, label_nodes=FALSE, adjust_axis_overlap=TRUE){
  text_grob_list <- vector("list",length(unlist(gene.list)))
  orig_cex <- cex
  idx <- 1
  for (s in seq_along(gene.list)){
    split_genes <- FALSE
    print(idx)
    if(length(gene.list[[s]])==0){
      next;
    } else{
      slope <- tree.max.adjusted$slope[s]
      intercept <- tree.max.adjusted$intercept[s]
      y.height = tree.max.adjusted$y0[s] - tree.max.adjusted$y1[s]
      
      label.bottom <- 0
      str.heightsum <- 0
      cex <- orig_cex 


      #centre the height of all the text relative to the line
      while(str.heightsum == 0 | (label.bottom + str.heightsum) > (title.y+panel_height) | (label_nodes == FALSE & (label.bottom + str.heightsum) > (tree.max.adjusted$y0[s]+rad*0.5))){

        if((label.bottom + str.heightsum) > (tree.max.adjusted$y0[s]+rad*0.5) & length(gene.list[[s]]) > 1){
          split_genes <- TRUE        
        }
        # if(s == 2){
        #   browser()
        # }
        str.heights <- sapply(gene.list[[s]], function(x) strheight(x,unit="inches",cex=cex))
        spacing=0.33*mean(str.heights)
        str.heightsum <- sum(str.heights)+spacing*length(str.heights)-spacing
        
        if(split == TRUE & split_genes ){
          str.heights.left <- str.heights[1:ceiling(length(gene.list[[s]])/2)]
          str.heights.right <- str.heights[(ceiling(length(gene.list[[s]])/2)+1):length(gene.list[[s]])]
          str.heightsum_left <- sum(str.heights.left)+spacing*length(str.heights.left)-spacing
          str.heightsum_right <- sum(str.heights.right)+spacing*length(str.heights.right)-spacing
          str.heightsum <- max(c(str.heightsum_left, str.heightsum_right) )
        }

        if(!label_nodes){
          if(length(gene.list[[s]])==1){ #when there is just one gene row center it otherwise position relative to the bottom of the textGrob 
            label.bottom  <- tree.max.adjusted$y1[s] + y.height/2
            vjust = "center"
          }else{
            label.bottom  <- y.height/2-str.heightsum/2 +tree.max.adjusted$y1[s]
            vjust = "bottom"
          }

         if(s==1 & ((str.heightsum - y.height) > rad) & !is.null(rad) & !is.null(scale)){
           label.bottom <- tree.max.adjusted$y1[s] - rad
          }
        }
        else{
          label.bottom <- tree.max.adjusted$y[s]-0.5*str.heightsum

        }
        # browser()
        if(((label.bottom + str.heightsum) > (title.y+panel_height)  | (label_nodes == FALSE & (label.bottom + str.heightsum) > (tree.max.adjusted$y0[s]+rad*0.5)))) {
          cex <- cex - 0.05
          print( (label.bottom + str.heightsum))
          print( (tree.max.adjusted$y0[s]+rad*0.5))
          print((title.y+panel_height))
          print(paste("cex",cex))
        }
      }

      #iterate through the genes for a given node
      for (g in rev(seq_along(gene.list[[s]]))){
        heights <- ifelse((g-1) == 0 ,0,sum(str.heights[c(1:(g-1))]))
        if(label_nodes == TRUE){
          ypos <- tree.max.adjusted$y[s]
          xpos <- tree.max.adjusted$x[s] 
          xline.dist <- line.dist + rad
          vjust <- "center"
          # if(!(tree.max.adjusted$tip[s] %in% tree.max.adjusted$parent)){
          #   ypos <- ypos-0.25*rad
          # }
        } else{
          
          ypos = label.bottom + (g-1)*spacing+heights-spacing
          
          # print("ypos")
          # print(ypos)
          #back computing the x position based on the intercept and the slope
          xpos = ifelse(is.infinite(slope),tree.max.adjusted$x0[s],(ypos-intercept)/slope)
          xline.dist <- line.dist
        }

        gene_positions = data.frame(labels=character(length=length(gene.list[[s]])), x=numeric(length=length(gene.list[[s]])), y=numeric(length=length(gene.list[[s]]))) 
        if( split == TRUE & split_genes ){
            # browser()
          if ( g <= ceiling(length(gene.col[[s]])/2)){
            # offset_left <- ceiling(length(gene.col[[s]])/2)
            heights <- ifelse((g-1) == 0,0,sum(str.heights.left[c(1:(g-1))]))
            ypos <- label.bottom + (g-1)*spacing+heights-spacing
            text_grob_list[[idx]] <- textGrob(gene.list[[s]][g],x=unit(xpos-xline.dist,"inches"),y=unit(ypos,"inches"),just=c("right","bottom"), gp=gpar(col=gene.col[[s]][g],cex=cex))
          } else{
            
            offset_left <- ceiling(length(gene.col[[s]])/2)
            heights <- ifelse((g-offset_left-1) == 0,0,sum(str.heights.right[c(1:(g-offset_left-1))]))
            ypos <- label.bottom + (g-offset_left-1)*spacing+heights-spacing
            
            text_grob_list[[idx]] <- textGrob(gene.list[[s]][g],x=unit(xpos+xline.dist,"inches"),y=unit(ypos,"inches"),just=c("left","bottom"), gp=gpar(col=gene.col[[s]][g],cex=cex))
          }
          
        }else if( alternating == TRUE ){ #alternate between putting the genes to the left and to the right of the node
          if (s%%2>0 ){
            xline.dist.adj <- (-1)*xline.dist
            just <- c("right","bottom")
            # text_grob_list[[idx]] <- textGrob(gene.list[[s]][g],x=unit(xpos-xline.dist,"inches"),y=unit(ypos,"inches"),just=c("right","bottom"), gp=gpar(col=gene.col[[s]][g],cex=cex))
          } 
          else{
            just=c("left","bottom")
            xline.dist.adj <- xline.dist
            # text_grob_list[[idx]] <- textGrob(gene.list[[s]][g],x=unit(xpos+xline.dist,"inches"),y=unit(ypos,"inches"),just=c("left","bottom"), gp=gpar(col=gene.col[[s]][g],cex=cex))
          }
            text_grob_list[[idx]] <- textGrob(gene.list[[s]][g],x=unit(xpos+xline.dist.adj,"inches"),y=unit(ypos,"inches"),just=just, gp=gpar(col=gene.col[[s]][g],cex=cex))
          if(adjust_axis_overlap){
            overlaps_axis  <- axis_overlap(xpos, gene.list[[s]][g], xline.dist.adj, axis.type,cex, panel_width, return_cex=TRUE)

            if(!is.null(overlaps_axis)){ #if a gene overlaps the axis shrink the gene labels until it doesn't
              text_grob_list <- position_genes(tree.max.adjusted=tree.max.adjusted, gene.list=gene.list, gene.col=gene.col, axis.type=axis.type, panel_height=panel_height, panel_width = panel_width, title.y=title.y, line.dist=line.dist,cex=overlaps_axis, rad=rad, alternating=alternating, split=split, label_nodes=label_nodes)
              return(text_grob_list)
            }
          }
        }
        else{
          # not sure why this was here idx = ifelse(s==1,g, length(gene.list[[(s-1)]])+g)  
          if (slope > 0 | (is.infinite((slope)) & axis.type=="SNV" ) ){
            xline.dist <- -1*abs(xline.dist)
          }else{
            xline.dist <- abs(xline.dist)
          }

          if(label_nodes == TRUE){
            node <- tree.max.adjusted[which(tree.max.adjusted$tip == tree.max.adjusted$tip[s]),]
            parent <- tree.max.adjusted[which(tree.max.adjusted$tip == tree.max.adjusted$parent[s]),]
            children <- tree.max.adjusted[which(tree.max.adjusted$parent == tree.max.adjusted$tip[s]),]
            if(nrow(children) > 0){
              if(nrow(children[which(children$x > node$x),]) > nrow(children[which(children$x < node$x),])){
                xline.dist <- -1*abs(xline.dist)
              }else if(nrow(children[which(children$x > node$x),]) < nrow(children[which(children$x < node$x),])){
                xline.dist <- abs(xline.dist)
              }
              # browser()
              if( (max(children$y)+rad) > label.bottom){
                ypos <- ypos+rad
              }
            }else{
              # browser()
              leaves <- tree.max.adjusted[!(tree.max.adjusted$tip %in% tree.max.adjusted$parent),]
              leaves <- leaves[order(leaves$x),]
              if(all(diff(leaves$x) < 2.5*rad) & all(diff(leaves$y) < 2.5*rad)){
                if(node$tip %in% leaves[c(2:(nrow(leaves)-1)),]){
                  ypos <- node$y - (xline.dist + rad)*cos(node$angle)
                  xpos <- node$x - (xline.dist)*sin(node$angle)
                }
              }
              # browser()
            }
          }
          if(label_nodes == TRUE && tree.max.adjusted$parent[s] %in% tree.max.adjusted$tip &&  ((tree.max.adjusted[which(tree.max.adjusted$tip == tree.max.adjusted$parent[s]),]$y - tree.max.adjusted$y[s]) < rad)) {
            if(tree.max.adjusted$tip[s] %in% tree.max.adjusted$parent){
              print("one")
              ypos <- tree.max.adjusted$y[s] + 1*rad + abs(xline.dist)
              xline.dist <- 0
              hjust <- "centre"
              cex <- orig_cex
            }else{
              print("two")
              ypos <- tree.max.adjusted$y[s]
              cex <- orig_cex
              if(tree.max.adjusted$x[s]  > parent$x){
                xline.dist <- abs(xline.dist)+rad  
                hjust <- "left"   
              }else{
                # browser()
                hjust <- "right"
                xline.dist <- -1*(abs(xline.dist)+rad)
              }
            }
          } else{          
            overlap <- check_overlap(xpos+xline.dist,ypos,gene.list[[s]][g],xline.dist, tree.max.adjusted,cex, rad)        
            if(length(unlist(overlap)) > 0 ){
              print("overlapping")
              xline.dist <- xline.dist*-1
              
              overlap <- check_overlap(xpos+xline.dist,ypos,gene.list[[s]][g],xline.dist, tree.max.adjusted,cex, rad)
              if( length(unlist(overlap)) > 0 ){
                print("still overlapping")
                xline.dist <- xline.dist*-1
              
              }
            }          

            if(length(overlap$nodes) > 0 && label_nodes == TRUE && !(tree.max.adjusted[overlap$nodes] %in% tree.max.adjusted$parent)){  #TODO make this more general,
              ypos <- ypos - abs(xline.dist) - rad
              xline.dist <- 0
              hjust <- "center"
              # print(ypos)
            } else{
              hjust <- ifelse(xline.dist > 0,"left","right")              
            }

            if(adjust_axis_overlap){
              overlaps_axis  <- axis_overlap(xpos, gene.list[[s]][g], xline.dist, axis.type,cex, panel_width, return_cex=TRUE)

              if(!is.null(overlaps_axis)){ #if a gene overlaps the axis shrink the gene labels until it doesn't
                text_grob_list <- position_genes(tree.max.adjusted=tree.max.adjusted, gene.list=gene.list, gene.col=gene.col, axis.type=axis.type, panel_height=panel_height, panel_width = panel_width, title.y=title.y, line.dist=line.dist,cex=overlaps_axis, rad=rad, alternating=alternating, split=split, label_nodes=label_nodes)
                return(text_grob_list)
              }
            }
          }
            # print("hjust")
            # print(hjust)
            text_grob_list[[idx]] <- textGrob(gene.list[[s]][g],x=unit(xpos+xline.dist,"inches"),y=unit(ypos,"inches"),just=c(hjust,vjust), gp=gpar(col=gene.col[[s]][g],cex=cex))
          }
          
         
      idx <- idx +1
      }
    }
  }
  return(text_grob_list)
}


add_text2 <- function(tree,genes,label_nodes=FALSE, cex=1,line.dist=0.5,v=NULL, title.y=NULL, panel_height=NULL, panel_width=NULL, xlims=NULL, ymax=ymax,axis.type=NULL,scale=NULL,rad=NULL,alternating=TRUE, split=TRUE, clone.out=NULL ){
  radn <- rad*1/scale #radius in native units
  genes <- genes[genes$node %in% tree$tip,]
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
    if(is.null(genes$col)){
      if(!is.na(gene.row$cn)){
      gene.col[[pos]] <<- c(gene.col[[pos]],ifelse(gene.row$cn=="loss" | gene.row$cn<2,"blue","red"))
    }else{
      gene.col[[pos]] <<- c(gene.col[[pos]],'black')
     }
    } else{
      gene.col[[pos]] <<- c(gene.col[[pos]],gene.row$col)
    }
  })
  
  tree.max <- adply(tree, 1,function(x){
    if(x$parent ==-1){
      basex=0;
      basey=0
    } else{
      basex <- v$x[v$lab== x$parent]; 
      basey <- v$y[v$lab== x$parent] ; 
    }
    tipx  <- v$x[v$lab==x$tip] 
    tipy  <- v$y[v$lab==x$tip] 
    return(data.frame(basex,basey,tipx,tipy))
  })
  
  print("tree.max")
  print(tree.max)

  #the length of the visible line segments
  tree.max.adjusted <- adply(tree.max,1, function(x){
                                            if(x$tipx == x$basex){ #straight line
                                              basex <- x$basex
                                              tipx <- x$tipx
                                              basey <- x$basey + radn
                                              tipy <- x$tipy - radn
                                            } else if(x$tipx > x$basex){
                                              basey <- x$basey + radn*cos(x$angle)
                                              tipy <- x$tipy - radn*cos(x$angle)
                                              basex <- x$basex + radn*sin(x$angle)
                                              tipx <- x$tipx - radn*sin(x$angle)
                                            } else if(x$tipx < x$basex){
                                              basey <- x$basey + radn*cos(x$angle)
                                              tipy <- x$tipy - radn*cos(x$angle)
                                              basex <- x$basex + radn*sin(x$angle)
                                              tipx <- x$tipx - radn*sin(x$angle)
                                            }
                                            if(x$parent ==-1){
                                              basex <- 0
                                              basey <- 0
                                            }                                            
                                            return(data.frame(basex,basey,tipx,tipy))
                                            }
                                          )


  #push a viewport the same size as the final panel so we can do calculations based on absolute size units
  if(!is.null(clone.out)){
    pushViewport(clone.out$vp)
  }else{
    pushViewport(viewport(height=unit(panel_height,"inches"), name="ref",width=unit(panel_width,"inches"),xscale=xlims,yscale=c(ymax,-2)))
  }
  tree.max.adjusted$x0 <- convertX(unit(tree.max.adjusted$basex,"native"),"inches",valueOnly=TRUE)
  tree.max.adjusted$x1 <- convertX(unit(tree.max.adjusted$tipx,"native"),"inches",valueOnly=TRUE)
  tree.max.adjusted$y0 <- convertY(unit(tree.max.adjusted$basey,"native"),"inches",valueOnly=TRUE) 
  tree.max.adjusted$y1 <- convertY(unit(tree.max.adjusted$tipy,"native"),"inches",valueOnly=TRUE)
  
  tree.max.adjusted$y <- convertY(unit(tree.max$tipy,"native"),"inches",valueOnly=TRUE) #actual node positions
  tree.max.adjusted$x <- convertX(unit(tree.max$tipx,"native"),"inches",valueOnly=TRUE)

  tree.max.adjusted$slope <- (tree.max.adjusted$y1 - tree.max.adjusted$y0)/(tree.max.adjusted$x1-tree.max.adjusted$x0)            
  tree.max.adjusted$intercept <-  tree.max.adjusted$y1-tree.max.adjusted$slope*tree.max.adjusted$x1
  print(tree.max.adjusted)
  text_grob_list <- position_genes(tree.max.adjusted=tree.max.adjusted, gene.list=gene.list, gene.col=gene.col, axis.type=axis.type, panel_height=panel_height, panel_width = panel_width, title.y=title.y, line.dist=line.dist,cex=cex, rad=rad, alternating=alternating, split=split, label_nodes=label_nodes)
  text_grob_glist <- do.call(gList, text_grob_list)

  if(!is.null(clone.out)){
    popViewport()
    text_tree <- gTree(children=text_grob_glist, vp=make_plot_viewport(clone.out, clip="off"))
    return(text_tree)
  }
  text_tree <- gTree(children=text_grob_glist,childrenvp = viewport(height=unit(panel_height,"inches"), name="ref",width=unit(panel_width,"inches"),xscale=xlims,yscale=c(ymax,-2), clip='off'))
  return(list(text_tree, tree.max.adjusted))
}


