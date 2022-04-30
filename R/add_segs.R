
add_segs3 <- function(tree,v,offset=0,rad=0,scale.x.real=NULL){
  #calculate offset based on the line width
  offset <- offset / scale.x.real
  offset <- offset/2

  tree_segs_adjusted <- tree_segs <- adply(tree, 1, function(x) {
    if(x$parent ==-1){
      basey = 0;
      basex = 0
    } else{
      basey <- v$y[v$id == x$parent]; 
      basex <- v$x[v$id == x$parent]; 
    }
      
    tipy <- basey+x$length1*cos(x$angle)
    tipx <- basex+x$length1*sin(x$angle)
    return(data.frame(basex,basey,tipx,tipy))
    })
  
  tree.out <- list()

  second_tree_segs_adjusted <- NULL
  if (length(grep("length",colnames(tree)))== 4){  
  tree_segs_adjusted <- adply(tree_segs,1, function(r){
                                                        offset_x  <- offset*cos(r$angle)
                                                        offset_y  <- offset*sin(r$angle)
                                                        if (r$angle > 0){
                                                          basey <- r$basey + offset_y
                                                          tipy <- r$tipy + offset_y
                                                        }else{
                                                          basey <- r$basey + offset_y
                                                          tipy <- r$tipy + offset_y                                                        
                                                        }
                                                        basex <- r$basex - offset_x
                                                        tipx <- r$tipx - offset_x
                                                        return(data.frame(basex,basey,tipx,tipy))
                                                        })

    tree_segs_adjusted <- tree_segs_adjusted[which(!(tree_segs_adjusted$basey == tree_segs_adjusted$tipy & tree_segs_adjusted$basex == tree_segs_adjusted$tipx)),]
    
    second_tree_segs <- tree_segs
    second_tree_segs$tipy <- second_tree_segs$basey+second_tree_segs$length2.c*cos(second_tree_segs$angle)
    second_tree_segs$tipx <- second_tree_segs$basex+second_tree_segs$length2.c*sin(second_tree_segs$angle)

    second_tree_segs_adjusted <- adply(second_tree_segs,1, function(r){
                                                          offset_x  <- offset*cos(r$angle)
                                                          offset_y  <- offset*sin(r$angle)
                                                          if (r$angle > 0){
                                                            basey <- r$basey - offset_y
                                                            tipy <- r$tipy - offset_y
                                                          }else{
                                                            basey <- r$basey - offset_y
                                                            tipy <- r$tipy - offset_y
                                                          }
                                                          basex <- r$basex + offset_x
                                                          tipx <- r$tipx + offset_x
                                                          return(data.frame(basex,basey,tipx,tipy))
                                                          })

    second_tree_segs_adjusted <- second_tree_segs_adjusted[which(!(second_tree_segs_adjusted$basey == second_tree_segs_adjusted$tipy & second_tree_segs_adjusted$basex == second_tree_segs_adjusted$tipx)),]
  }
  tree.out <- list(tree_segs = tree_segs_adjusted,tree_segs2=second_tree_segs_adjusted)
  return(tree.out)    
}

get_seg_coords <- function(tree,v,offset=0,rad=0,scale1=NULL){
  #calculate offset based on the line width
  offset <- offset*1/scale1
  offset <- offset/2

  tree_segs <- adply(tree, 1, function(x) {
    if(x$parent ==-1){
      basey = 0;
      basex = 0;
    } else{
      basey <- v$y[v$id == x$parent]; 
      basex <- v$x[v$id == x$parent]; 
    }
      
    tipy <- basey+x$length1*cos(x$angle)
    tipx <- basex+x$length1*sin(x$angle)
    return(data.frame(basex,basey,tipx,tipy))
    })
  
  tree.out <- list()
  second_tree_segs_adjusted <- NULL

  tree_segs_adjusted <- adply(tree_segs,1, function(r){
                                                        offset_x  <- offset*cos(r$angle)
                                                        offset_y  <- offset*sin(r$angle)
                                                        if (r$angle > 0){
                                                          basey <- r$basey + offset_y
                                                          tipy <- r$tipy + offset_y
                                                        }else{
                                                          basey <- r$basey + offset_y
                                                          tipy <- r$tipy + offset_y                                                        
                                                        }
                                                        basex <- r$basex - offset_x
                                                        tipx <- r$tipx - offset_x
                                                        return(data.frame(basex,basey,tipx,tipy))
                                                        })

  tree_segs_adjusted <- tree_segs_adjusted[which(tree_segs_adjusted$basey !=tree_segs_adjusted$tipy),]
  
  if (length(grep("length",colnames(tree)))== 4){  
    second_tree_segs <- tree_segs
    second_tree_segs$tipy <- second_tree_segs$basey+second_tree_segs$length2.c*cos(second_tree_segs$angle)
    second_tree_segs$tipx <- second_tree_segs$basex+second_tree_segs$length2.c*sin(second_tree_segs$angle)


    second_tree_segs_adjusted <- adply(second_tree_segs,1, function(r){
                                                          offset_x  <- offset*cos(r$angle)
                                                          offset_y  <- offset*sin(r$angle)
                                                          if (r$angle > 0){
                                                            basey <- r$basey - offset_y
                                                            tipy <- r$tipy - offset_y
                                                          }else{
                                                            basey <- r$basey - offset_y
                                                            tipy <- r$tipy - offset_y
                                                          }
                                                          basex <- r$basex + offset_x
                                                          tipx <- r$tipx + offset_x
                                                          return(data.frame(basex,basey,tipx,tipy))
                                                          })

    second_tree_segs_adjusted <- second_tree_segs_adjusted[which(second_tree_segs_adjusted$basey !=second_tree_segs_adjusted$tipy),]
  }
  
  tree.out <- list(tree_segs = tree_segs_adjusted, tree_segs2=second_tree_segs_adjusted)
  return(tree.out)    
}


add_tree_segs <- function(clone.out,  rad, line.lwd, scale1,  seg1.col, seg2.col){
  offset <- line.lwd*1/96
  if(!('length2' %in% colnames(clone.out$tree))){
    offset <- 0
  }
  tree.out <- get_seg_coords(clone.out$tree, clone.out$v, offset, rad, scale1)
  tree_segs1 <- tree.out[[1]]
  tree_segs2 <- tree.out[[2]]
  out <- list()
  out$tree_segs1 <- segmentsGrob(name = 'tree.segs.1', x0=tree_segs1$basex,y0=tree_segs1$basey,x1=tree_segs1$tipx, y1=tree_segs1$tipy, default.units="native", gp=gpar(col=seg1.col,lwd=line.lwd))
  
  if(!is.null(tree_segs2)){
    tree_segs2 <- tree_segs2[which(tree_segs2$basey !=tree_segs2$tipy),]
    if( nrow(tree_segs2) > 0) {
    out$tree_segs2 <- segmentsGrob(name = 'tree.segs.2', x0=tree_segs2$basex,y0=tree_segs2$basey,x1=tree_segs2$tipx, y1=tree_segs2$tipy, default.units="native", gp=gpar(col=seg2.col,lwd=line.lwd))
    }
  }
  clone.out$grobs <- c(clone.out$grobs, out)
}