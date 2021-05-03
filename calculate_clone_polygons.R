f0 <- function(){
	in_env <- new.env(parent=emptyenv())
	in_env$x <- 0
	f1 <- function(env){
		y <- env$x
		y <- y+1
		assign("x",y, envir=env)
		# env$x <- env$x+1
	}
	f1(in_env)
	print(in_env$x)
}


sigmoid = function(params, g) {
  exp(params[1]-params[2]*g) / (1 + exp(params[1] -  (g* params[2])))
}

sigmoid.up = function(params, g) {
  exp(params[1] + params[2]*g) / (1 + exp(params[1] +  (g* params[2])))
}


make_polygon <- function(x0, y0, x1,x2, wid=1, len=1, col='gray',  sig_shape=4, beta_in=3 ){
	vaf <- wid 

	beta <- len/beta_in
	y1 <- y0 + beta

	while(y1 <= y0){
		y1 = y1+1
	}

	yy <- seq(y0, y1, length.out=100)

	#params.d  <- c(-0.7310133, 4.8038079) #generic sigmoid

	params.d  <- c(-0.7310133, sig_shape)
	y.ex <- seq(-1, 1.5, length.out=length(yy))
	xu.2 <- sigmoid(params.d, y.ex)

	#scale and shift each sigmoid
	x.rt.d <- (xu.2-min(xu.2))/(max(xu.2-min(xu.2)))*(x0-x2)+x2
	x.rt.u <- (-xu.2-max(-xu.2))/(max(xu.2+max(-xu.2)))*(x1-x0)+x1 
	 
	yy.plot <- c(y0, yy, y0+len, y0+len, rev(yy))
	xx <- c(x0, x.rt.u, x1, x2, rev(x.rt.d))

	return(list(x=xx, y=yy.plot, col=col))  
}


position_polygons <- function(clone_env, i, wid, x, y, len, sig_shape=4, beta_in=3, branching=TRUE, fixed_angle=NULL, no_ccf=FALSE){
	v <- clone_env$v
	tree <- clone_env$tree
	clones <- clone_env$clones

	# get the row of v that corresponds to the clone
	vi <- v[i,]
	print(i)
	if (!is.na(vi$parent) && vi$parent == -1 && nrow(v[v$parent==-1,]) == 1) { #if root the clone extends the full width of the plot
		x0 <- x 
		y0 <- y
		len0 <- len
		x1 <- vi$x1
		x2 <- vi$x2
	}else{ #parent not root -- not trunk clone
		if(vi$parent == -1){
			par <- data.frame(lab=-1, x=0, y=0, len=len, x.mid=0, x1=min(v$x1), x2=max(v$x2))
		}else{
			par <- v[v$lab == vi$parent,] #get parent clone
		}
		x_mid <- vi$x.mid
		x1 <- vi$x1
		x2 <- vi$x2
		# if(vi$lab==3) browser()
		siblings <- v[which(v$parent == par$lab),]
		if(nrow(siblings) == 1){
			dist <- par$x.mid-par$x
			parent_angle <- ifelse(is.null(fixed_angle) & no_ccf == FALSE, atan(dist/par$len), 0)
			# parent_angle <- ifelse(is.null(fixed_angle) & no_ccf=FALSE, atan(dist/par$len), 0)
		} else if(nrow(siblings) == 2){
			sibling_coords <- c(siblings$x1, siblings$x2)
			x1_max <- sibling_coords[which.max(abs(sibling_coords))]
			x2_max <- sibling_coords[which.max(abs(sibling_coords-x1_max))]

			dist <- abs(x1_max-x2_max)/2
			if (x_mid > par$x.mid){
				parent_angle <- ifelse(is.null(fixed_angle),atan(dist/par$len), fixed_angle)
				parent_angle <- min(parent_angle, 40/180*pi)
			} else if (x_mid < par$x.mid){
				parent_angle <- ifelse(is.null(fixed_angle),atan(-dist/par$len), -fixed_angle)
				parent_angle <- max(parent_angle, -40/180*pi)
			}else{
				dist <- par$x.mid-par$x
				parent_angle <- atan(dist/par$len)
			}
		} else{
				if(v$lab == siblings$lab[which.min(siblings$x.mid)]){ #align leftmost child with the left outer clone border
					parent_angle <- ifelse(is.null(fixed_angle), atan(-1*abs(par$x1)/par$len), -fixed_angle)           
				} else if(v$lab == siblings$lab[which.max(siblings$x.mid)]){ #align rightmost child with the right outer clone border
					parent_angle <- ifelse(is.null(fixed_angle), atan(abs(par$x1)/par$len), fixed_angle)           
				} else{
					parent_angle <- if(par$len > 0) atan((vi$x.mid - par$x.mid)/par$len) else 0
				}
		}

		r <- tree$length[which(tree$parent==par$lab & tree$tip == vi$lab)]
		x.shift <- r*sin(parent_angle)
		x0 <- par$x + x.shift
		y.shift <- r*cos(parent_angle)
		y0 <- par$y + y.shift
		len0 <-  par$len - y.shift
		# len0 <- max(0, par$len - y.shift)
		print("len0")
		print(len0)
		print("parent_angle")
		print(parent_angle)
		if(par$lab != -1 & len0 >= 0){
			#make sure the node isn't outside of the parent clone
			par.coords <- data.frame(x=clones[[as.integer(which(v$lab == par$lab))]][["x"]], y= clones[[as.integer(which(v$lab == par$lab))]][["y"]])

			par$x1 <- clones[[as.integer(which(v$lab == par$lab))]][["x1"]]
			par$x2 <- clones[[as.integer(which(v$lab == par$lab))]][["x2"]]
			par.coords.pos <- par.coords[1:match(par$x1,par.coords$x),]
			par.coords.neg <- par.coords[match(par$x2,par.coords$x):length(par.coords$x),]

			match.x.pos  <- par.coords.pos$x[which.min(abs(par.coords.pos$y-y0))]
			match.x.neg  <- par.coords.neg$x[which.min(abs(par.coords.neg$y-y0))]
			while((match.x.pos > x0 & match.x.neg > x0) | (match.x.pos < x0 & match.x.neg < x0)){
				print(paste("moving node",vi$lab))
				print(match.x.pos)
				print(match.x.neg)
				print(x0)
				# browser()
				closer <-  ifelse(match.x.pos > x0, min(match.x.pos,match.x.neg), max(match.x.pos,match.x.neg))
				further <-  ifelse(match.x.pos > x0, max(match.x.pos,match.x.neg), min(match.x.pos,match.x.neg))
				x0 <-  closer + 0.15*(further-closer)
				# y.shift <- sqrt(r^2-x0^2)
				x.shift <- x0-par$x
				x0 <- par$x+x.shift
				parent_angle <- asin(x.shift/r)
				# parent_angle <- asin(y.shift/r)
				angle_x <- par$x+r*sin(parent_angle)
				y.shift <- r*cos(parent_angle)
				y0 <- par$y + y.shift
				print(paste("x0", x0, "x angle", angle_x))
				match.x.pos  <- par.coords.pos$x[which.min(abs(par.coords.pos$y-y0))]
				match.x.neg  <- par.coords.neg$x[which.min(abs(par.coords.neg$y-y0))]

			}

		}
			len0 <- par$len - y.shift
			tree$angle[which(tree$parent==par$lab & tree$tip == vi$lab)] <- parent_angle 
	}
	v[i,]$len <- len0
	v[i,]$y <- y0
	v[i,]$x <- x0
	clone_env$v <- v
	clone_env$tree <- tree

	clone_points <- make_polygon( x0=x0, y0=y0, x1=x1, x2=x2, wid=wid*vi$vaf, len=len0, col=vi$color, sig_shape=sig_shape, beta_in=beta_in)
 	return(c(clone_points, x0=x0, y0=y0,len=len0,x1=x1,x2=x2,alpha=vi$alpha))
}


get_clones <- function(x=0, y=0, wid=1.2, len=len, sig_shape=3, beta_in=3, branching=FALSE, no_ccf=FALSE, fixed_angle=NULL, spread=1, clone_env=NULL, adjust_beta=FALSE){
	
	clone_env$clones <- list()
	clone_env$coords.df <- data.frame(x0=numeric(length=nrow(clone_env$v)),y0=numeric(length=nrow(clone_env$v)),len=numeric(length=nrow(clone_env$v)),x1=numeric(length=(nrow(clone_env$v))),x2=numeric(length=nrow(clone_env$v)))	   

	for (j in 1:(nrow(clone_env$v))){
	   clone_env$clones[[j]] <- position_polygons(clone_env, j, wid=wid, x=x,y=y,len=len,sig_shape=sig_shape, beta_in=beta_in, branching=branching, no_ccf=no_ccf, fixed_angle=fixed_angle)
		beta.add <- 0.5
		if(adjust_beta & no_ccf == FALSE){
			#if the polygon gets cut off before it can occupy the full width adjust the beta value to make it curve more sharply
			while(all(clone_env$clones[[j]]$y[which(abs(clone_env$clones[[j]]$x) == max(abs(clone_env$clones[[j]]$x)))] > (clone_env$coords.df$len[1]+y))){
			  clone_env$clones[[j]] <- position_polygons(clone_env, j, wid=wid, x=x, y=y, len=len, sig_shape=sig_shape, beta_in=beta_in+beta.add, branching=branching, no_ccf=no_ccf, fixed_angle=fixed_angle)
			  for(var in colnames(clone_env$coords.df)){
			  		clone_env$coords.df[j,var] <- clone_env$clones[[j]][var]
			  }		
			  beta.add <- beta.add + 0.5
			  print(c("new beta",beta.add,clone_env$clones[[j]]$y[which.max(clone_env$clones[[j]]$x[-which.max(clone_env$clones[[j]]$x)])], clone_env$coords.df$len[1]))
			}
		}
	   for(var in colnames(clone_env$coords.df)){
	   		clone_env$coords.df[j,var] <- clone_env$clones[[j]][var]
	   }		
	}
}

compute_clones <- function(v, x=1, y=0, wid=1.2, extra_len=1,tree=NULL, fixed_angle= NULL,
                                sig_shape=3, beta_in=3, branching=TRUE, no_ccf = FALSE, spread=1){  
	
	#make sure the root is properly defined 
	root = v[!is.na(v$parent) & v$parent == -1,]
	v <- v[is.na(v$parent) | v$parent != -1,]
	v <- rbind(root, v)
	if(no_ccf & (is.null(fixed_angle) & nrow(v) > 6) | any(table(v$parent) > 2)){
		v <- count_leaves_per_node(v)
		tmp <-  position_nodes_radial(v, tree, extra.len, spread)
		clone_env <-  new.env(parent = emptyenv())
		clone_env$v <- tmp$v
		clone_env$tree <- tmp$tree
		return(clone_env)
	} else if(no_ccf & !is.null(fixed_angle) ){
		#position nodes fixed angle
		clone_env <-  position_nodes_fixed(v, tree, fixed_angle=fixed_angle, len=extra.len)
		return(clone_env)
	} else{
		v <- position_clones(v,tree,wid)
	}
	v$x <- 0
	v$y <- 0
	v$len <- 0
	len <- extra_len 

	clone_env <-  new.env(parent = emptyenv())
	clone_env$v <- v
	clone_env$tree <- tree

	print("first pass")
	print(v)
	get_clones(x=x, y=y, len=len, sig_shape=sig_shape, beta_in=beta_in, branching=branching, no_ccf=no_ccf, fixed_angle=fixed_angle, spread=spread, clone_env=clone_env)
	
	#if the end of the polygon is shorter than the last clone polygon or the desired length make the polygon longer and recompute
	
	while (max(clone_env$coords.df$y0) > (clone_env$coords.df$len[1]+y) | (min(clone_env$coords.df$len) < extra.len )){
		 print("while loop")
		 len <- len +(extra.len-min(clone_env$coords.df$len))+0.0001
		 print(paste0("len2:",len, " coords ", clone_env$coords.df$len[1]+y, " max ", max(clone_env$coords.df$y0), "min len", min(clone_env$coords.df$len) , "extra ", extra.len, branching=branching))
		 print((clone_env$coords.df$len[1] - max(clone_env$coords.df$y0)) )
		 get_clones(x=x, y=y, wid=wid, len=len, sig_shape=sig_shape, beta_in=beta_in, branching=branching, no_ccf=no_ccf, fixed_angle=fixed_angle, spread=spread, clone_env=clone_env)
	}

	# #move to appropriate place
	# if(no_ccf == TRUE){
	#  assign("v",v,envir=parent.frame())
	#  assign("tree",tree,envir=parent.frame())
	#  return(list(v=v))
	# }

	#if the polygon gets cut off before it can occupy the full width adjust the beta value to make it curve more sharply
	get_clones(x=x, y=y, len=len, sig_shape=sig_shape, beta_in=beta_in,branching=branching,no_ccf=no_ccf, fixed_angle=fixed_angle, spread=spread, clone_env=clone_env, adjust_beta=TRUE)

	return(clone_env)
}


add_clone_grobs <- function(clone.out, ...){
    polygon_list <- list()
    for (j in 1:length(clone.out$clones)){
      polygon_list[[j]] <- polygonGrob(x=clone.out$clones[[j]]$x, y= clone.out$clones[[j]]$y, default.units="native", gp=gpar(fill=clone.out$clones[[j]]$col,col='transparent',alpha=clone.out$clones[[j]]$alpha))
    }
  clone.out$grobs <- c(clone.out$grobs, polygon_list)
}

position_only_nodes <- function(){
        if(branching==FALSE){
          parent_angle <- 0
        }

          siblings <- v[which(v$parent==par$lab),]

          if(nrow(siblings)>1 & nrow(siblings)<4){
            print(vi)
            print(xi)
            if(spread == TRUE){
            	dist_left <- abs(-wid/2 - par$x)
        		dist_right <- abs(wid/2 - par$x)
            	dist <- min(dist_left, dist_right)
            }else{
	            x2_max <- siblings$x2[which.max(abs(siblings$x2))]
	            x1_max <- siblings$x1[which.max(abs(siblings$x1))]
	            # dist <- abs(x1_max-y2_mxx)/2
	            dist_left <- abs(x1_max- par$x)
	            dist_right <- abs(x2_max - par$x)
        	}
            dist <- min(dist_left, dist_right)
            print("dist here")
            print(dist)
            print(dist_left)
            print(dist_right)
           if (abs(xi-par$x.mid)<1e-3 & is.null(fixed_angle)){
                         # dist <- par$x.mid-par$y
                         # parent_angle <- atan(dist/par$len)
                         parent_angle <- 0
            } else if (xi > par$x.mid){

              parent_angle <- ifelse(is.null(fixed_angle),atan(dist/par$len), fixed_angle)
            print(parent_angle)

            } else if (xi<par$x.mid){
              parent_angle <- ifelse(is.null(fixed_angle),atan(-dist/par$len), -fixed_angle)
                    print(parent_angle)

            }
          } else if(nrow(siblings)>3 ){
            print(vi)
            print("gt 3")
            x2_max <- siblings$x2[which.max(abs(siblings$x2))]
            x1_max <- siblings$x1[which.max(abs(siblings$x1))]
            if(spread == TRUE){
            	dist <- min(dist_left, dist_right)
            	}else{
            dist <- abs(x1_max-y2_mxx)/2

            	}
            if(x1_max != vi$x1 & x2_max != vi$x2){
               dist <- abs(xi - par$x)
            }
            print("dist")
            print(dist)
 
           if (abs(xi-par$x.mid)<1e-3){
				parent_angle <- 0
            } else if (xi > par$x.mid){
				parent_angle <- ifelse(is.null(fixed_angle),atan(dist/par$len), fixed_angle)
            } else if (xi<par$x.mid){
				parent_angle <- ifelse(is.null(fixed_angle),atan(-dist/par$len), -fixed_angle)
            }
          } else{
            # dist <- par$x.mid-par$y
            # parent_angle <- ifelse(is.null(fixed_angle), atan(dist/par$len), 0)
            parent_angle <- tree$angle[which(tree$parent == par$parent & tree$tip == par$lab)]
            # parent_angle <- 0
          }
        
        r = tree$length[which(tree$parent == par$lab & tree$tip == vi$lab)]
        x.shift = r*sin(parent_angle)
        x0 = par$x + x.shift
        y.shift = r*cos(parent_angle)
        y0 = par$y+y.shift
        len0 = par$len - y.shift
        print("y.shift")
        print(y.shift)

        v[i,]$len <- len0
        v[i,]$y <- y0
        v[i,]$x <- x0
        tree$angle[which(tree$parent==par$lab & tree$tip == vi$lab)] <- parent_angle
   
        assign("v",v,envir=env)
        assign("tree",tree,envir=env)
       return(c(x0=x0,y0=y0,len=len0,x1=x1,x2=x2)) 
	      
}
