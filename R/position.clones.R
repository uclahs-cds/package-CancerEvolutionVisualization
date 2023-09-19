add.vaf <- function(v) {
    v <- v[order(v$parent), ];

    for (i in seq_along(v$id)) {
        if (v$parent[i] == -1) {
            parent <- data.frame(id = -1, vaf = 1);
        } else {
            parent <- v[which(v$id == v$parent[i]), ];
            }

        v$vaf[i] <- parent$vaf / nrow(v[v$parent == parent$id, ]);
        }

    return(v);
    }

position.clones <- function(v, tree, wid) {
    if (!('vaf' %in% colnames(v)) || all(is.na(v[v$parent != -1,]$vaf))) {
        v <- add.vaf(v);
        }

    v$x.mid <- v$x1 <- v$x2 <- 0;

    for (p in unique(v$parent)) {
        children <- v[which(v$parent == p), ];
        parent <- v[which(v$id == p), ];

        #if there is only one child center them with the parent
        if (nrow(children) == 1) {
            if (p == -1) {
                v$x.mid[v$id == children$id] <- 0;
                v$x1[v$id == children$id] <- -(wid / 2);
                v$x2[v$id == children$id] <- wid / 2;
            } else {
                v$x.mid[v$id == children$id] <- v$x.mid[v$id == p];
                v$x1[v$id == children$id] <- v$x.mid[v$id == children$id] - v$vaf[v$id == children$id] * wid / 2;
                v$x2[v$id == children$id] <- v$x.mid[v$id == children$id] + v$vaf[v$id == children$id] * wid / 2;
                }
        } else {
            if (nrow(children) > 2) {
                children$length <- sapply(children$id, function(x) tree$length[tree$tip == x])
                children.ascending <- children[order(children$length),]

                child.order <- unlist(lapply(
                    c(1:(ceiling(nrow(children) / 2))),
                    FUN = function(x) {
                        if (x < (nrow(children) - x + 1)) c(x, (nrow(children) - x + 1)) else x;
                        }
                    ));

                children <- children.ascending[child.order,-ncol(children.ascending)];
                }

            if (p == -1) {
                parent <- data.frame(vaf = 1, x.mid = 0);
                }

            child.vaf.sum <- sum(children$vaf * wid);
            total.space <- parent$vaf * wid - child.vaf.sum; # the gap between subclones
            per.gap <- total.space / (nrow(children) - 1); #if there are more than two subclones split the gap
            position <- parent$x.mid - wid / 2 * parent$vaf; #parent's lower bound

            for (c in seq_along(children$id)) {
                child <- children[c, ];

                v$x.mid[v$id == child$id[1]] <- position + child$vaf[1] * wid / 2;
                position <- position + child$vaf[1] * wid;

                if (c != 1) {
                    v$x.mid[v$id == child$id[1]] <- v$x.mid[v$id == child$id[1]] + per.gap;
                    position <- position + per.gap;
                    }

                v$x1[v$id == child$id[1]] <- v$x.mid[v$id == child$id[1]] - child$vaf[1] * wid / 2;
                v$x2[v$id == child$id[1]] <- v$x.mid[v$id == child$id[1]] + child$vaf[1] * wid / 2;
                }
            }
        }

    return(v);
    }

position.nodes.fixed <- function(v, tree, fixed.angle, len) {
    for (i in seq_along(v$id)) {
        vi <- v[i, ];

        if (!is.na(vi$parent) && vi$parent == -1) {
            angle <- 0;

            # If root the clone extends the full width of the plot
            x0 <- 0;
            y0 <- tree$length[tree$parent == -1];
            len0 <- len + y0;
        } else {
            # Parent not root -- not trunk clone
            par <- v[v$id == vi$parent, ];

            #get parent clone
            siblings <- v[which(v$parent == par$id),];

            if (nrow(siblings) == 1) {
                angle <- 0;
            } else if (nrow(siblings) == 2) {
                if (any(siblings$x > par$x)) {
                    angle <- -(fixed.angle);
                } else {
                    angle <- fixed.angle;
                }
            } else if (nrow(siblings) == 3) {
                if (any(siblings$x > par$x)) {
                    angle <- -(fixed.angle);
                } else if (any(siblings$x < par$x)) {
                    angle <- fixed.angle;
                } else {
                    angle <- 0;
                    }
                }

            r <- tree$length[tree$tip == vi$id];
            x.shift <- r * sin(angle);
            x0 <- par$x + x.shift;
            y.shift <- r * cos(angle);
            y0 <- par$y + y.shift;
            len0 <- par$len + y.shift;
            }

        tree$angle[tree$tip == vi$id] <- angle;

        v[i,]$len <- len0;
        v[i,]$y <- y0;
        v[i,]$x <- x0;
        }

    tree$angle[!is.na(v$angle)] <- v$angle[!is.na(v$angle)];

    for (i in seq_along(v$id)) {
        vi <- v[i, ];
        angle <- tree$angle[tree$tip == vi$id];

        if (!is.na(vi$parent) && vi$parent == -1) {
            x0 <- 0;
            y0 <- tree$length[tree$parent == -1];
            len0 <- 0;
        } else {
            par <- v[v$id == vi$parent, ];

            r <- tree$length[tree$tip == vi$id];
            x.shift <- r * sin(angle);
            x0 <- par$x + x.shift;
            y.shift <- r * cos(angle);
            y0 <- par$y + y.shift;
            len0 <- par$len + y.shift;
            }

        v[i,]$len <- len0;
        v[i,]$y <- y0;
        v[i,]$x <- x0;
        }
    
    clone.env <-  new.env(parent = emptyenv());
    clone.env$v <- v;
    clone.env$tree <- tree;

    return(clone.env);
    }

position.clones.no.vaf <- function(v, wid, spread = TRUE) {
    v$y.mid <- v$y1 <- v$y2 <- 0;

    for (p in sort(unique(v$parent))) {
        children <- v[which(v$parent == p), ];
        parent <- v[which(v$id == p), ];

        # if there is only one child center them with the parent
        if (nrow(children) == 1) {
            if (p == -1) {
                #trunk clone
                v$vaf[v$id == children$id] <- 1;
                v$y.mid[v$id == children$id] <- 0;
                v$y1[v$id == children$id] <- -(wid) / 2;
                v$y2[v$id == children$id] <- wid / 2;
            } else {
                # only children are centered on their parent clones midline
                v$y.mid[v$id == children$id] <- v$y.mid[v$id == p];

                if (spread) {
                    v$vaf[v$id == children$id] <- v$vaf[v$id == p];
                    v$y1[v$id == children$id] <- -(wid / 2);
                    v$y2[v$id == children$id] <- wid / 2;
                } else {
                    v$vaf[v$id == children$id] <- v$vaf[v$id == p];
                    v$y1[v$id == children$id] <- v$y.mid[v$id == children$id] - v$vaf[v$id == children$id] * wid / 2;
                    v$y2[v$id == children$id] <- v$y.mid[v$id == children$id] + v$vaf[v$id == children$id] * wid / 2;
                    }
                }
        } else {
            if (p == -1) {
                parent <- data.frame(vaf = 1, y.mid = 0);
                }

            children <- v[which(v$parent == p), ];

            if (spread) {
                dist.left <- abs(-(wid / 2) - parent$y.mid);
                dist.right <- wid / 2 - parent$y.mid;
                dist.min <- min(dist.left, dist.right);
                bounds <- c(parent$y.mid - dist.min, parent$y.mid + dist.min);
                child.width <- (bounds[2] - bounds[1]) / nrow(children);
                last.bound <- 0;

                for (i in seq_along(children$id)) {
                    child <- children[i,]
                    v$y1[v$id == child$id[1]] <- bounds[1] + child.width * last.bound;
                    v$y2[v$id == child$id[1]] <- v$y1[v$id == child$id[1]] + child.width;
                    v$y.mid[v$id == child$id[1]] <- v$y1[v$id == child$id[1]] + 0.5 * child.width;
                    last.bound <- last.bound + 1;
                    }
            } else {
                child.vaf <- parent$vaf / nrow(children);
                position <- parent$y.mid - wid / 2 * parent$vaf;
                v$vaf[which(v$parent == p)] <- child.vaf;
                children <- v[which(v$parent == p), ];

                for (c in seq_along(children$id)) {
                    child <- children[c,];
                    v$y.mid[v$id == child$id[1]] <- position + child$vaf[1] * wid / 2;
                    v$y1[v$id == child$id[1]] <- v$y.mid[v$id == child$id[1]] - child$vaf[1] * wid / 2;
                    v$y2[v$id == child$id[1]] <- v$y.mid[v$id == child$id[1]] + child$vaf[1] * wid / 2;
                    position <- position + child$vaf[1] * wid;
                    }
                }
            }
        }

    return(v);
    }
