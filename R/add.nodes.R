add_node_ellipse <- function(
    clone.out,
    rad,
    label.nodes = NULL,
    labe.cex = NA,
    add.normal = FALSE,
    scale1,
    ...
    ){

    if (is.na(labe.cex)) {
        labe.cex <- 0.85;
        }
    
    if (!('plot.lab' %in% colnames(clone.out$v))) {
		clone.out$v$plot.lab <- if (!is.null(clone.out$v$label.text)) {
		    clone.out$v$label.text;
		} else {
		    clone.out$v$id;
		    };
	    }

    clone.out$v$plot.lab <- as.character(clone.out$v$plot.lab);
	clone.out$v$circle <- sapply(
	    clone.out$v$plot.lab, 
	    FUN = function(x) {
	        return(nchar(x) < 3);
	        }
	    );

    nodes_grob <- list();
	node.grob.name <- 'node.polygons';

	#more precise than circleGrob
	circle_grobs <- ellipseGrob(
	    name = node.grob.name,
	    x = unit(clone.out$v$x, "native"),
	    y = unit(clone.out$v$y, "native"),
	    size = rad * (1 + 0.2 * nchar(clone.out$v$plot.lab)),
	    ar = 1 - log2(nchar(clone.out$v$plot.lab)) / 10,
	    gp = gpar(fill = clone.out$v$colour, col = clone.out$v$colour),
	    angle = pi / 2,
	    position.units = "native",
	    size.units = "inches",
	    ...
	    );

	clone.out$grobs <- c(clone.out$grobs, list(circle_grobs));

	if (!is.null(label.nodes) && label.nodes == TRUE) {
		if(is.na(labe.cex)){
			labe.cex <- rad * 2 / (get.gpar("fontsize")$fontsize / 72);
		    }

  		node_label_grob <- textGrob(
  		    name = 'node.labels',
  		    clone.out$v$plot.lab,
  		    x = unit(clone.out$v$x, "native"),
  		    y = unit(clone.out$v$y, "native"),
  		    just = c("center", "center"),
  		    gp = gpar(col = '#FFFFFF', cex = labe.cex - log2(nchar(clone.out$v$plot.lab)) / 10)
  		    );

	    clone.out$grobs <- c(clone.out$grobs, list(node_label_grob));
	    }
    }

add.normal <- function(clone.out, rad, labe.cex, normal.cex = 1) {
    normal_box <- rectGrob(
        x = unit(0.5, "npc"),
        y = unit(0.5, "npc"),
        name = "normal.box",
        width = unit(2 * rad * normal.cex,"inches"),
        height = unit(2 * rad * normal.cex, "inches"),
        just = c("center", "center"),
        gp = gpar(col = "black", fill = "transparent", lwd = 1.5, lty = "31")
        );

    normal_label <- textGrob(
        "N",
        x = unit(0.5, "npc"),
        y = unit(0.5, "npc"),
        name = "ormal.label",
        just = "center",
        gp = gpar(
            col = 'black',
            cex = convertY(grobHeight(normal_box), "inches", valueOnly = TRUE) * 1.05 * 72 / 12
            )
        );

    normal_grob <- gTree(
        children = gList(normal_box, normal_label),
        name = "normal.gtree",
        cl = "normal_node",
        vp = vpStack(
            make_plot_viewport(clone.out, clip = "off"),
            viewport(
                y = unit(1, "npc"),
                x = unit(0, "native"),
                height = grobHeight(normal_box),
                width = grobWidth(normal_box),
                just = c("centre", "bottom")
                )
            )
        );

    clone.out$grobs <- c(clone.out$grobs, list(normal_grob));
    }

add_pie_nodes <- function(clone.out, rad, cluster_list) {
	pie_grobs <- list();
	clone.out$v <- clone.out$v[order(clone.out$v$id), ];

	for(i in seq_along(clone.out$v$id)) {
		pie_grobs[[i]] <- pieGrob(
		    x = clone.out$v[i,]$x,
		    y = clone.out$v[i,]$y,
		    rad = rad,
		    prop_list = cluster_list[[i]],
		    col_df = cluster_list$col
		    );
    	}

	clone.out$grobs <- c(clone.out$grobs, pie_grobs);
    }

pieGrob <- function(x, y, rad=.1, prop_list, col_df, xy.units="native"){
	r <-1;
	x0 <- y0 <- 0;

	slice_list <- list();

	for (i in seq_along(prop_list)) {
		angle <- 2 * pi * sum(prop_list[1:i]); 
		x0 <- if (i == 1) { 0 } else { r * sin(2 * pi * sum(prop_list[1:(i - 1)])) };
		x1 <- r * sin(angle);
		y0 <- if (i ==1 ) { 0 } else { r * cos(2 * pi * sum(prop_list[1:(i - 1)])) };
		y1 <- r * cos(angle);
		x_edge1 <- c(0, x0);
		x_edge2 <- c(x1, 0);
		x_arc <- sapply(
		    seq(if(i == 1) { 0 } else { 2 * pi * sum(prop_list[1:(i - 1)]) }, angle, length = 1000),
		    FUN = function(deg) { r * sin(deg) }
		    );

		xc <- c(x_edge1, x_arc, x_edge2);

		y_arc <- sapply(
		    seq(if (i == 1) { 0 } else { 2 * pi * sum(prop_list[1:(i - 1)]) }, angle, length = 1000),
		    FUN = function(deg) { r * cos(deg) }
		    );

		y_edge1 <- c(0, y0);
		y_edge2 <- c(y1, 0);		
		yc <- c(y_edge1, y_arc, y_edge2);
		slice_list[[i]] <- polygonGrob(
		    unit(xc, "native"),
		    unit(yc, "native"),
		    gp = gpar(
		        fill = col_df[col_df$id == names(prop_list)[i], ]$colour,
		        col='transparent'
		        )
		    );
		}

	pie_glist <- do.call(gList, slice_list);

	pie_tree <- gTree(
	    children = pie_glist,
	    vp = viewport(
	        x = unit(x, xy.units),
	        y = unit(y, xy.units),
	        width = unit(2 * rad, "inches"),
	        height = unit(2 * rad, "inches"),
	        xscale = c(-1, 1),
	        yscale = c(-1, 1),
	        angle = if (i == 2) { 90 } else { 0 }
	        )
	    );

	return(pie_tree);
    }

gridPie <- function(
    xpos = 0,
    ypos = 0,
    x,
    edges = 200,
    radius = 1,
    col = NULL, 
    startpos = 0,
    shadow = FALSE,
    shadow.col = c("#ffffff", "#cccccc"), 
    explode = 0,
    ...
    ) {

    if (!is.numeric(x)) { 
            stop("floating.pie: x values must be numeric.")
            }

    if (is.null(dev.list)) {
        plot(
            0,
            xlim = c(-1.5, 1.5) * radius + xpos,
            ylim = c(-1.5, 1.5) * radius + ypos,
            type = "n",
            axes = FALSE,
            xlab = "", 
            ylab = ""
            );
        }
    
    validx <- which(!is.na(x) & x > 0);
    x <- c(0, cumsum(x[validx])/sum(x[validx]));
    dx <- diff(x);
    nx <- length(dx);

    if (is.null(col)) {
        col <- rainbow(nx);
        }

    else if (length(col) < nx) {
        col <- rep(col, ceiling(nx / length(col)));
        }

    xylim <- par("usr");
    plotdim <- par("pin");
    yradius <- radius * (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1] / plotdim[2];
    bc <- 2 * pi * (x[1:nx] + dx / 2) + startpos;

    for (i in 1:nx) {
        n <- max(2, floor(edges * dx[i]))
        t2p <- 2 * pi * seq(x[i], x[i + 1], length = n) + startpos;
        xc <- c(cos(t2p) * radius + xpos, xpos);
        yc <- c(sin(t2p) * yradius + ypos, ypos);

        polygonGrob(xc, yc, gp = gpar(fill = col[i]), ...);
        t2p <- 2 * pi * mean(x[i + 0:1]) + startpos;
        xc <- cos(t2p) * radius;
        yc <- sin(t2p) * radius;
        }
    
    return(bc);
    }
