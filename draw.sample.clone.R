
draw.sample.clones <- function(v, x=1, y=0, wid, len=9, extra.len=1,tree=NULL, fixed_angle= NULL,
                                sig.shape=4,rad=0, beta_in=beta_in, branching=TRUE, no_ccf = FALSE, spread=1){
  
  
 print("v")
 print(v)
  #make sure the root is properly defined 
    root = v[!is.na(v$parent) & v$parent == -1,]
    v = v[is.na(v$parent) | v$parent != -1,]
    v = rbind(root, v)
    if(no_ccf ){
      # browser()
      v <- count_leaves_per_node(v)
      clone.out <-  position_nodes_radial(v, tree, extra.len,spread)
      return(clone.out)
    } else{
      v <- position_clones(v,tree,wid)
    }
    v$x = 0
    v$y = 0
    v$len = 0
    
   
    print("len")
    print(len)
    # len <- estimate_min_len(v)
    # tree <- get("tree",envir=parent.env(environment()))
    # assign('v',v,envir=.GlobalEnv)
    
    #debug
    print("v")
    print(v)
    
    coords.df <- data.frame(old.x=numeric(length=nrow(v)),new.x=numeric(length=nrow(v)),new.y=numeric(length=nrow(v)),len=numeric(length=nrow(v)),x1=numeric(length=(nrow(v))),x2=numeric(length=nrow(v)))
     
    
    clones <- list()
    
    ex.list <- list()

    # if(no_ccf == TRUE){
    #   print("no ccf")
    #   print("fixed_angle")
    #   print(fixed_angle)
    #   for (j in 1:(nrow(v))){  
    #     print(j)
    #     clones[[j]] <- my.draw.sample.clone(j,wid,clones,x=x,y=y,len=len,sig.shape=sig.shape,tree=tree,branching=branching,fixed_angle=fixed_angle, no_ccf=no_ccf)
    #     coords.df[j,] <- unlist(clones[[j]])
    #   }
    #   if(is.null(fixed_angle)){
 
    #      while (max(coords.df$new.x) > (coords.df$len[1]+x) | (min(coords.df$len) < extra.len )){
    #       print("while loop")
    #       if ( min(coords.df$len) < extra.len ) {
    #         len <- len+(extra.len-min(coords.df$len))+0.0001
    #         print(paste0("len2:",len, " coords ", coords.df$len[1]+x, " max ", max(coords.df$new.x)," extra ", extra.len,branching=branching))
    #         print((coords.df$len[1] - max(coords.df$new.x)) )
            
            
    #       }

    #       clones = list()

    #      for (j in 1:(nrow(v))){  
    #         print(j)
    #         clones[[j]] <- my.draw.sample.clone(j,wid,clones,x=x,y=y,len=len,sig.shape=sig.shape,tree=tree,branching=branching, fixed_angle=fixed_angle, no_ccf= no_ccf)
    #         coords.df[j,] <- unlist(clones[[j]])
    #       }
    #     }
    #   }
    #   assign("v",v,envir=parent.frame())
    #   assign("tree",tree,envir=parent.frame())
    #   return(list(coords.df=coords.df))
    
    # }

    print("first pass")
    for (j in 1:(nrow(v))){
      #cat('---', v[j,]$parent,'\n')
        print(j)
        clones[[j]] <- my.draw.sample.clone(j,wid,clones,x=x,y=y,len=len,sig.shape=sig.shape,tree=tree,beta_in=beta_in,branching=branching,no_ccf=no_ccf, fixed_angle=fixed_angle, spread=spread)
        if(no_ccf==FALSE){
          coords.df[j,] <- unlist(clones[[j]][c(4:9)])
          } else{
          coords.df[j,] <- unlist(clones[[j]])
        }
    }
   #if the end of the polygon is shorter than the last clone polygon or the desired length make the polygon longer and recompute
    print(coords.df)
    print(v)
     # coords.df[j,] <- ifelse(no_ccf==FALSE, unlist(clones[[j]][c(4:9)]), unlist(clones[[j]]))
     while (max(coords.df$new.x) > (coords.df$len[1]+x) | (min(coords.df$len) < extra.len )){
      print("while loop")
      len <- len +(extra.len-min(coords.df$len))+0.0001
      print(paste0("len2:",len, " coords ", coords.df$len[1]+x, " max ", max(coords.df$new.x), "min len", min(coords.df$len) , "extra ", extra.len,branching=branching))
      print((coords.df$len[1] - max(coords.df$new.x)) )

      coords.df <- data.frame(old.y=numeric(length=nrow(v)),new.x=numeric(length=nrow(v)),new.y=numeric(length=nrow(v)),len=numeric(length=nrow(v)),y1=numeric(length=(nrow(v))),y2=numeric(length=nrow(v)))
      clones <- list()    
      ex.list <- list()      
      
      for (j in 1:(nrow(v))){
   
        clones[[j]] <- my.draw.sample.clone(j,wid,clones,x=x,y=y,len=len,sig.shape=sig.shape,tree=tree,beta_in=beta_in,branching=branching,no_ccf=no_ccf, fixed_angle=fixed_angle, spread=spread)
        # coords.df[j,] <- 
        if(no_ccf==FALSE){
          coords.df[j,] <- unlist(clones[[j]][c(4:9)])
          } else{
           coords.df[j,] <- unlist(clones[[j]])}
        }
      }

    if(no_ccf == TRUE){
      assign("v",v,envir=parent.frame())
      assign("tree",tree,envir=parent.frame())
      return(list(v=v))
    }

    for (j in 1:(nrow(v))){
 
      clones[[j]] <- my.draw.sample.clone(j,wid,clones,x=x,y=y,len=len,sig.shape=sig.shape,tree=tree,beta_in=beta_in,branching=branching, no_ccf=no_ccf, spread=spread)
      coords.df[j,] <- unlist(clones[[j]][c(4:10)])
      beta.add <- 0.5
      
      #if the polygon gets cut off before it can occupy the full width adjust the beta value to make it curve more sharply
      while(all(clones[[j]]$y[which(abs(clones[[j]]$x) == max(abs(clones[[j]]$x)))]> (coords.df$len[1]+y))){
        clones[[j]] <- my.draw.sample.clone(j,wid,clones,x=x,y=y,len=len,sig.shape=sig.shape,tree=tree,beta_in=beta_in+beta.add,branching=branching, spread=spread)
        coords.df[j,] <- unlist(clones[[j]][c(4:10)])
        beta.add <- beta.add + 0.5
        print(c("new beta",beta.add,clones[[j]]$y[which.max(clones[[j]]$x[-which.max(clones[[j]]$x)])], coords.df$len[1]))
      }
      
      
    }

    assign("tree",tree,envir=parent.frame())
    return(list(v=v,tree=tree, clones=clones))
}

panel.clones <- function(clones=NULL, ...){
    for (j in 1:length(clones)){
      grid.polygon(x=clones[[j]]$x, y= clones[[j]]$y, default.units="native", gp=gpar(fill=clones[[j]]$col,col='transparent',alpha=clones[[j]]$alpha))
    }
  
}


circle <- function(r,x0,y0,res=0.01){
  seqx <- seq(r,-r,by=-res)
  outlen = length(seqx)
  y=numeric(length=outlen*2)
  x=numeric(length=outlen*2)
  for (i in seq_along(seqx)){
    xi=seqx[i]
    yi=sqrt(r^2-xi^2)
    y[i]=yi+y0
    y[outlen+i]=-yi+y0
    x[i]=xi+x0
    x[outlen+i]=-xi+x0
  }
  return(data.frame(x=x,y=y))
} 

test_circle <- circle(1,10,10)

  
add.circles <- function(coords.df,rad){
    circle.coords <- list()
   for (j in 1:nrow(coords.df)){
    circle.coords <- c(circle.coords, circle(rad,0,0))
    }
    circle.coords <- alply(coords.df,1, function(x){
    return(circle(rad,x$new.x,x$new.y))
  })
  return(circle.coords)
}


add.segs <- function(tree,offset=0,rad=0,scale.x.real=NULL){
  print(tree)
  offset <- offset*1/scale.x.real
  v <- get("v",envir=parent.frame())
  print(v)
  tree_segs <- adply(tree, 1, function(x) {
    if(x$parent ==-1){
      basex = 0;
      basey = 0
    } else{
      basex <- v$x[v$lab== x$parent]; 
      basey <- v$y[v$lab== x$parent]; 
    }
      
    tipx <- basex+x$length1*cos(x$angle)
    tipy <- basey+x$length1*sin(x$angle)
    return(data.frame(basex,basey,tipx,tipy))
    })
    
    orig.tree <- tree_segs
    tree.out <- list()

  if (length(grep("length",colnames(tree)))== 4){  
    tree_segs$basey[orig.tree$basey<=0] <- tree_segs$basey[orig.tree$basey<=0] - offset/2
    tree_segs$tipy[orig.tree$tipy<=0] <- tree_segs$tipy[orig.tree$tipy<=0] - offset/2
    tree_segs$basey[orig.tree$basey>0] <- tree_segs$basey[orig.tree$basey>0] - offset/2
    tree_segs$tipy[orig.tree$tipy>0] <- tree_segs$tipy[orig.tree$tipy>0] - offset/2
    
     tree.out <- list(tree_segs=tree_segs)
    
    if(!is.null(tree$length2.c)){
      mini.tree <- orig.tree
      #   mini.tree <- tree_segs
      #orig.tree <- tree_segs
      mini.tree$tipx <- mini.tree$basex+mini.tree$length2.c*cos(mini.tree$angle)
      mini.tree$tipy <- mini.tree$basey+mini.tree$length2.c*sin(mini.tree$angle)
      mini.tree$basey[orig.tree$basey<=0] <- mini.tree$basey[orig.tree$basey<=0] + offset/2
      mini.tree$tipy[orig.tree$tipy<=0] <- mini.tree$tipy[orig.tree$tipy<=0] + offset/2
      mini.tree$basey[orig.tree$basey>0] <- mini.tree$basey[orig.tree$basey>0] + offset/2
      mini.tree$tipy[orig.tree$tipy>0] <- mini.tree$tipy[orig.tree$tipy>0] + offset/2
      tree.out <- list(tree_segs=tree.out$tree_segs,tree_segs2=data.frame(mini.tree))
    }
  }else if (length(grep("length",colnames(tree)))== 2) {
    tree_segs$basey[orig.tree$basey<=0] <- tree_segs$basey[orig.tree$basey<=0]
    tree_segs$tipy[orig.tree$tipy<=0] <- tree_segs$tipy[orig.tree$tipy<=0] 
    tree_segs$basey[orig.tree$basey>0] <- tree_segs$basey[orig.tree$basey>0]
    tree_segs$tipy[orig.tree$tipy>0] <- tree_segs$tipy[orig.tree$tipy>0]
    
    tree.out <- list(tree_segs=tree_segs)
  } 
  return(tree.out)
}


add_segs2 <- function(tree,offset=0,rad=0,scale.x.real=NULL){
  print("add.segs2")
  print(tree)
  offset <- offset*1/scale.x.real
  offset <- offset/2
  v <- get("v",envir=parent.frame())
  print(v)
  tree_segs <- adply(tree, 1, function(x) {
    if(x$parent ==-1){
      basex = 0;
      basey = 0
    } else{
      basex <- v$x[v$lab== x$parent]; 
      basey <- v$y[v$lab== x$parent]; 
    }
      
    tipx <- basex+x$length1*cos(x$angle)
    tipy <- basey+x$length1*sin(x$angle)
    return(data.frame(basex,basey,tipx,tipy))
    })
  
  tree.out <- list()
  if (length(grep("length",colnames(tree)))== 4){  
  if(!is.null(tree$length2.c)){
      second_tree_segs <- tree_segs
      second_tree_segs$tipx <- second_tree_segs$basex+second_tree_segs$length2.c*cos(second_tree_segs$angle)
      second_tree_segs$tipy <- second_tree_segs$basey+second_tree_segs$length2.c*sin(second_tree_segs$angle)
    }
    # browser()
  tree_segs_adjusted <- adply(tree_segs,1, function(r){
                                                        offset_x  <- offset*cos(pi/2-r$angle)
                                                        offset_y  <- offset*sin(pi/2-r$angle)
                                                        if (r$angle > 0){
                                                          basex <- r$basex + offset_x
                                                          tipx <- r$tipx + offset_x
                                                        }else{
                                                          basex <- r$basex - offset_x
                                                          tipx <- r$tipx - offset_x
                                                        }
                                                        basey <- r$basey - offset_y
                                                        tipy <- r$tipy - offset_y
                                                        return(data.frame(basex,basey,tipx,tipy))
                                                        })

  second_tree_segs_adjusted <- adply(second_tree_segs,1, function(r){
                                                        offset_x  <- offset*cos(pi/2-r$angle)
                                                        offset_y  <- offset*sin(pi/2-r$angle)
                                                        if (r$angle > 0){
                                                          basex <- r$basex - offset_x
                                                          tipx <- r$tipx - offset_x
                                                        }else{
                                                          basex <- r$basex + offset_x
                                                          tipx <- r$tipx + offset_x
                                                        }
                                                        basey <- r$basey + offset_y
                                                        tipy <- r$tipy + offset_y
                                                        return(data.frame(basex,basey,tipx,tipy))
                                                        })
  return( tree.out <- list(tree_segs = tree_segs_adjusted,tree_segs2=second_tree_segs_adjusted))
  } else if(length(grep("length",colnames(tree)))== 2) {
      tree.out <- list(tree_segs=tree_segs)
    } 
  return(tree.out)    
}
