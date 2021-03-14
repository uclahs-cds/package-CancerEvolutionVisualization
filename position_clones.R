add_vaf <- function(v){
  v <- v[order(v$parent),]
  for (i in unique(v$lab)){
    print(i)
    if(v$parent[i] == -1){
      v$vaf[i] <- 1
    }else{
      parent <- v[which(v$lab == v$parent[i]),]    
      v$vaf[i] <- parent$vaf/nrow(v[v$parent == parent$lab,])
    }
  }
  return(v)
}
position_clones <- function(v,tree,wid){
  if(!('vaf' %in% colnames(v)) | all(is.na(v[v$parent != -1,]$vaf))){
    v <- add_vaf(v)
  }
  v$x.mid <- 0
  v$x1 <- 0
  v$x2 <- 0
  # browser()
  for (p in unique(v$parent)){
    children <- v[which(v$parent==p),]
    parent <- v[which(v$lab==p),]
    #if there is only one child center them with the parent

    if(nrow(children) == 1){
      if(p == -1){
        v$x.mid[v$lab == children$lab] <- 0
        v$x1[v$lab == children$lab] <- -wid/2
        v$x2[v$lab == children$lab] <- wid/2
      }else{
        v$x.mid[v$lab == children$lab] <- v$x.mid[v$lab==p]
        v$x1[v$lab == children$lab] <- v$x.mid[v$lab == children$lab]-v$vaf[v$lab == children$lab]*wid/2
        v$x2[v$lab == children$lab] <- v$x.mid[v$lab == children$lab]+v$vaf[v$lab == children$lab]*wid/2
      }
    } else{
      if(nrow(children)>2){
        children$length <- sapply(children$lab, function(x) tree$length[tree$tip == x])
        children_ascending <- children[order(children$length),]

        # browser()
        child_order <- unlist(lapply(c(1:ceiling(nrow(children)/2)), function(x) if(x < (nrow(children)-x+1)) return(c(x,(nrow(children)-x+1))) else return(x)))
        children <- children_ascending[child_order,-ncol(children_ascending)]

      }
      if(p == -1){
        parent <- data.frame(vaf=1,x.mid=0)
      }
      child_vaf_sum <- sum(children$vaf*wid)
      total_space <- parent$vaf*wid-child_vaf_sum # the gap between subclones
      per_gap <- total_space/(nrow(children)-1) #if there are more than two subclones split the gap
      position <- parent$x.mid-wid/2*parent$vaf #parent's lower bound
         
      for (c in seq_along(children$lab)){
        child <- children[c,]
        if(c==1){
          v$x.mid[v$lab==child$lab[1]] <- position+child$vaf[1]*wid/2
          position <- position+child$vaf[1]*wid
        }else{
          v$x.mid[v$lab==child$lab[1]] <- position+child$vaf[1]*wid/2+per_gap
          position <- position+child$vaf[1]*wid + per_gap
        }
        v$x1[v$lab==child$lab[1]] <- v$x.mid[v$lab==child$lab[1]]-child$vaf[1]*wid/2
        v$x2[v$lab==child$lab[1]] <- v$x.mid[v$lab==child$lab[1]]+child$vaf[1]*wid/2
      }
    }
  }
  return(v)
}

position_nodes_fixed <- function(v, tree, fixed_angle, len){
  for (i in seq_along(v$lab)){
    vi <- v[i,]
    if (!is.na(vi$parent) && vi$parent == -1){ #if root the clone extends the full width of the plot
      x0 <- 0
      y0 <- tree$length[tree$parent==-1]
      len0 <- len + y0

    }else{ #parent not root -- not trunk clone
      par <- v[v$lab == vi$parent,] #get parent clone

      siblings <- v[which(v$parent == par$lab),]
      if(nrow(siblings) == 1){
        parent_angle <- 0
      } else if(nrow(siblings)==2){           
        if (any(siblings$x > par$x)) {
          parent_angle <-  -fixed_angle      
        }else{
          parent_angle <- fixed_angle
        }
      }  else if(nrow(siblings) == 3){                
        if (any(siblings$x > par$x)) {
          parent_angle <-  -fixed_angle      
        }else if(any(siblings$x < par$x)){
          parent_angle <- fixed_angle
        }else{
          parent_angle <- 0
        }
      } 

      r <- tree$length[which(tree$parent==par$lab & tree$tip == vi$lab)]
      x.shift <- r*sin(parent_angle)
      x0 <- par$x + x.shift
      y.shift <- r*cos(parent_angle)
      y0 <- par$y + y.shift
      len0 <- par$len + y.shift
      tree$angle[which(tree$parent==par$lab & tree$tip == vi$lab)] <- parent_angle 
    }

      v[i,]$len <- len0
      v[i,]$y <- y0
      v[i,]$x <- x0
  }
  clone_env <-  new.env(parent = emptyenv())
  clone_env$v <- v
  clone_env$tree <- tree
  return(clone_env)
}

position_clones_no_vaf <- function(v, wid, spread=TRUE){

  v$y.mid <- 0
  v$y1 <- 0
  v$y2 <- 0
  for (p in sort(unique(v$parent))){
    children <- v[which(v$parent==p),]
    parent <- v[which(v$lab==p),]
   # if there is only one child center them with the parent
    if(nrow(children)==1){
      if(p==-1){ #trunk clone
        v$vaf[v$lab == children$lab] <- 1
        v$y.mid[v$lab == children$lab] <- 0
        v$y1[v$lab == children$lab] <- -wid/2
        v$y2[v$lab == children$lab] <- wid/2
      }else{ # only children are centered on their parent clones midline
        v$y.mid[v$lab == children$lab] <- v$y.mid[v$lab==p]
        if(spread==TRUE){
          v$vaf[v$lab == children$lab] <- v$vaf[v$lab == p]
          v$y1[v$lab == children$lab] <- -wid/2
          v$y2[v$lab == children$lab] <- wid/2
        }else{
          v$vaf[v$lab == children$lab] <- v$vaf[v$lab == p]
          v$y1[v$lab == children$lab] <- v$y.mid[v$lab == children$lab]-v$vaf[v$lab == children$lab]*wid/2
          v$y2[v$lab == children$lab] <- v$y.mid[v$lab == children$lab]+v$vaf[v$lab == children$lab]*wid/2
        }
      }
    } else if(nrow(children)>0){
      if(p==-1){
        parent <- data.frame(vaf=1,y.mid=0)
      }
      children <- v[which(v$parent==p),]
      if(spread==TRUE){
        dist_left <- abs(-wid/2 - parent$y.mid)
        dist_right <- wid/2 - parent$y.mid
        dist_min <- min(dist_left, dist_right)
        bounds <- c(parent$y.mid-dist_min, parent$y.mid+dist_min)
        child_width <- (bounds[2]-bounds[1])/nrow(children)
        last_bound <- 0
        for (i in seq_along(children$lab)){
          child <- children[i,]
          v$y1[v$lab==child$lab[1]] <- bounds[1]+child_width*last_bound
          v$y2[v$lab==child$lab[1]] <- v$y1[v$lab==child$lab[1]] + child_width 
          v$y.mid[v$lab==child$lab[1]] <- v$y1[v$lab==child$lab[1]] + 0.5*child_width
          last_bound <- last_bound + 1
        }
      } else{
        # browser()
        print("parent")
        print(parent)
        child_vaf <- parent$vaf/nrow(children)
        position <- parent$y.mid-wid/2*parent$vaf
        v$vaf[which(v$parent==p)] <- child_vaf
        children <- v[which(v$parent==p),]
   
      for (c in seq_along(children$lab)){
        child <- children[c,]
        
        print("position")
        print(position)
        v$y.mid[v$lab==child$lab[1]] <- position + child$vaf[1]*wid/2
        v$y1[v$lab==child$lab[1]] <- v$y.mid[v$lab==child$lab[1]]-child$vaf[1]*wid/2
        v$y2[v$lab==child$lab[1]] <- v$y.mid[v$lab==child$lab[1]]+child$vaf[1]*wid/2
        position <- position+child$vaf[1]*wid
      }
      }
    }
  }
  print("position_clones")
  print(v)
  return(v)
}

# estimate_min_len(v,tree){
#   len <- 0
#   tiered_tree <- get_num_tiers(tree)
#   for (i in unique(tiered_tree$tiers)){
#     tier <- tiered_tree[tiered_tree$tier ==i,]
#     if(nrow(v_tier)==1){
#       len <- len + tree$length[which(tree$tip == tier$lab)]
#     }else{
#       for (j in v_tier$tip){
        
#       }
#     }
#   }
# }