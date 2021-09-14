count_leaves_per_node <- function(v){
	count_env <- new.env()
	v$leaves <- 0
	leaf_nodes <- v$lab[!(v$lab %in% v$parent)]
	v$leaves[v$lab %in% leaf_nodes] <- 1
	assign("leaves_v", v, envir=count_env)

	count_leaves <- function(node=1){
		v <- get("leaves_v",envir=count_env)
		par <- v$parent[v$lab == node ]
		if (par != -1){
			v <- get("leaves_v",envir=count_env)
			v$leaves[v$lab == par] <- v$leaves[v$lab == par] + 1
			assign("leaves_v",v,envir=count_env)
			count_leaves(par)
		}
	}

		for (node in leaf_nodes){
			count_leaves(node)
		}
		v <- get("leaves_v",envir=count_env)

		return(v)
	}

assign_weight <- function(node,v, extra_len, spread){
	node_weight <- v$leaves[v$lab == node]/v$leaves[v$parent == -1]
	return(node_weight)
}

position_nodes_radial <- function(v,tree,extra_len, spread=1){
	w <- spread*pi 
	xpos <- 0
	ypos <- 0
	tau <- -pi/2.5
	vi <- v[v$parent==-1,]
	preorder_traversal <- function(node=NULL, tree=NULL, w=NULL, tau=NULL, eta=NULL, spread=1){
		vi <- v[v$lab == node,]
		d <- tree$length[tree$tip == vi$lab & tree$parent == vi$parent]
		if (vi$parent != -1){
			v$x[v$lab == vi$lab] <<- v$x[v$lab == vi$parent]  + d*sin(tau + w/2)
			v$y[v$lab == vi$lab] <<- v$y[v$lab == vi$parent]  + d*cos(tau + w/2)
			tree$angle[tree$tip==vi$lab & tree$parent == vi$parent] <<- tau + w/2
		} else{
			v$x[v$lab == vi$lab] <<- 0
			v$y[v$lab == vi$lab] <<- d
			tree$angle[tree$tip==vi$lab & tree$parent == vi$parent] <<- 0

		}
		eta <- tau
		for (child in v$lab[v$parent == vi$lab]){
			child_weight <- assign_weight(child,v)
			w <- child_weight*spread*pi
			tau <- eta
			eta <- eta + w
			preorder_traversal(node=child,tree=tree, w=w,tau=tau, eta=eta, spread=spread)
		}
	}

	preorder_traversal(node=1, tree=tree, w=w, tau=tau, spread=spread)

	v$len <- sapply(v$y, function(x) return(max(v$y) + extra_len - x))
	return(list(v=v,tree=tree))
}