adjust_lengths <- function(x, cols, node_df){
  out.df <- x
  for (c in cols){
    if(x[1,c] > 0){
      length_adj <- x[1,c]
    if( x[1,c] == x[1,cols[length(cols)]] ){    #if max
      length_adj <- length_adj + node_df$rad[node_df$lab==x$tip]
    } 
    if(x$parent != -1){
      length_adj <- length_adj + node_df$rad[node_df$lab==x$parent]
     }
     }  else{length_adj <- 0}
        var.name <- paste0(names(x)[c],'.adj')
        out.df <- cbind(out.df,length_adj)
        colnames(out.df)[ncol(out.df)] <- var.name
  }
  return(out.df)
}


adjust_branch_lengths <- function(node_df, tree, rad, scale1){
   if (is.null(node_df$rad)){
    rad <- rad*(1/scale1)
    node_df$rad <- rep(rad,nrow(node_df))
  }
  node_df$rad[node_df$lab==-1] <- 0
  print(node_df)
  length_cols <- grep("length", colnames(tree))
  tree.adj <- adply(tree, 1, function(x) adjust_lengths(x,length_cols,node_df))  

  tree$length <- tree.adj$length.adj
  tree$length1 <- tree.adj$length1.adj
  tree$length2.c <- tree.adj$length2.c.adj
  return(tree)
}


adjust_tree <- function(in.tree.rad, tree.in,rad,scale.x.real){
   if (is.null(in.tree.rad$rad)){
    rad <- rad*(1/scale.x.real)
    in.tree.rad$rad <- rep(rad,nrow(in.tree.rad))
  }
  in.tree.rad$rad[in.tree.rad$lab==-1] <- 0
  length_cols <- grep("length", colnames(tree.in))
  tree.adj <- adply(tree.in,1, function(x) adjust_lengths(x,length_cols,in.tree.rad))  

  tree.in$length <- tree.adj$length.adj
  tree.in$length1 <- tree.adj$length1.adj
  tree.in$length2.c <- tree.adj$length2.c.adj
  return(tree.in)
}
