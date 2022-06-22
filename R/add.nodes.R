add.node.ellipse <- function(
    clone.out,
    node.radius,
    label.nodes = NULL,
    labe.cex = NA,
    add.normal = FALSE,
    scale1,
    ...
    ) {

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

	node.grob.name <- 'node.polygons';

	#more precise than circleGrob
	circle.grobs <- ellipseGrob(
	    name = node.grob.name,
	    x = unit(clone.out$v$x, 'native'),
	    y = unit(clone.out$v$y, 'native'),
	    size = node.radius * (1 + 0.2 * nchar(clone.out$v$plot.lab)),
	    ar = 1 - log2(nchar(clone.out$v$plot.lab)) / 10,
	    gp = gpar(fill = clone.out$v$colour, col = clone.out$v$colour),
	    angle = pi / 2,
	    position.units = 'native',
	    size.units = 'inches',
	    ...
	    );

	clone.out$grobs <- c(clone.out$grobs, list(circle.grobs));

	if (!is.null(label.nodes) && label.nodes == TRUE) {
		if (is.na(labe.cex)) {
			labe.cex <- node.radius * 2 / (get.gpar('fontsize')$fontsize / 72);
		    }

  		node.label.grob <- textGrob(
  		    name = 'node.labels',
  		    clone.out$v$plot.lab,
  		    x = unit(clone.out$v$x, 'native'),
  		    y = unit(clone.out$v$y, 'native'),
  		    just = c('center', 'center'),
  		    gp = gpar(col = '#FFFFFF', cex = labe.cex - log2(nchar(clone.out$v$plot.lab)) / 10)
  		    );

	    clone.out$grobs <- c(clone.out$grobs, list(node.label.grob));
	    }
    }

add.normal <- function(clone.out, node.radius, labe.cex, normal.cex = 1) {
    normal.box <- rectGrob(
        x = unit(0.5, 'npc'),
        y = unit(0.5, 'npc'),
        name = 'normal.box',
        width = unit(2 * node.radius * normal.cex,'inches'),
        height = unit(2 * node.radius * normal.cex, 'inches'),
        just = c('center', 'center'),
        gp = gpar(col = 'black', fill = 'transparent', lwd = 1.5, lty = '31')
        );

    normal.label <- textGrob(
        'N',
        x = unit(0.5, 'npc'),
        y = unit(0.5, 'npc'),
        name = 'ormal.label',
        just = 'center',
        gp = gpar(
            col = 'black',
            cex = convertY(grobHeight(normal.box), 'inches', valueOnly = TRUE) * 1.05 * 72 / 12
            )
        );

    normal.grob <- gTree(
        children = gList(normal.box, normal.label),
        name = 'normal.gtree',
        cl = 'normal.node',
        vp = vpStack(
            make.plot.viewport(clone.out, clip = 'off'),
            viewport(
                y = unit(1, 'npc'),
                x = unit(0, 'native'),
                height = grobHeight(normal.box),
                width = grobWidth(normal.box),
                just = c('centre', 'bottom')
                )
            )
        );

    clone.out$grobs <- c(clone.out$grobs, list(normal.grob));
    }

add.pie.nodes <- function(clone.out, node.radius, cluster.list) {
	pie.grobs <- list();
	clone.out$v <- clone.out$v[order(clone.out$v$id), ];

	for (i in seq_along(clone.out$v$id)) {
		pie.grobs[[i]] <- pieGrob(
		    x = clone.out$v[i,]$x,
		    y = clone.out$v[i,]$y,
		    node.radius = node.radius,
		    prop.list = cluster.list[[i]],
		    col.df = cluster.list$col
		    );
    	}

	clone.out$grobs <- c(clone.out$grobs, pie.grobs);
    }

pieGrob <- function(
    x,
    y,
    node.radius = 0.1,
    prop.list,
    col.df,
    xy.units = 'native'
    ) {

	r <- 1;
	x0 <- y0 <- 0;

	slice.list <- list();

	for (i in seq_along(prop.list)) {
		angle <- 2 * pi * sum(prop.list[1:i]);

		x0 <- if (i == 1) 0 else r * sin(2 * pi * sum(prop.list[1:(i - 1)]));
		x1 <- r * sin(angle);

		y0 <- if (i == 1 ) 0 else r * cos(2 * pi * sum(prop.list[1:(i - 1)]));
		y1 <- r * cos(angle);

		x.edge1 <- c(0, x0);
		x.edge2 <- c(x1, 0);
		x.arc <- sapply(
		    seq(
		        if (i == 1) 0 else 2 * pi * sum(prop.list[1:(i - 1)]),
		        angle,
		        length = 1000
		        ),
		    FUN = function(deg) {
		        r * sin(deg);
		        }
		    );

		xc <- c(x.edge1, x.arc, x.edge2);

		y.arc <- sapply(
		    seq(
		        if (i == 1) 0 else 2 * pi * sum(prop.list[1:(i - 1)]),
		        angle,
		        length = 1000
		        ),
		    FUN = function(deg) {
		        r * cos(deg);
		        }
		    );

		y.edge1 <- c(0, y0);
		y.edge2 <- c(y1, 0);
		yc <- c(y.edge1, y.arc, y.edge2);
		slice.list[[i]] <- polygonGrob(
		    unit(xc, 'native'),
		    unit(yc, 'native'),
		    gp = gpar(
		        fill = col.df[col.df$id == names(prop.list)[i], ]$colour,
		        col = 'transparent'
		        )
		    );
		}

	pie.gList <- do.call(gList, slice.list);

	pie.tree <- gTree(
	    children = pie.gList,
	    vp = viewport(
	        x = unit(x, xy.units),
	        y = unit(y, xy.units),
	        width = unit(2 * node.radius, 'inches'),
	        height = unit(2 * node.radius, 'inches'),
	        xscale = c(-1, 1),
	        yscale = c(-1, 1),
	        angle = if (i == 2) 90 else 0
	        )
	    );

	return(pie.tree);
    }

gridPie <- function(
    xpos = 0,
    ypos = 0,
    x,
    edges = 200,
    node.radiusius = 1,
    col = NULL,
    startpos = 0,
    shadow = FALSE,
    shadow.col = c('#ffffff', '#cccccc'),
    explode = 0,
    ...
    ) {

    if (!is.numeric(x)) {
            stop('floating.pie: x values must be numeric.')
            }

    if (is.null(dev.list)) {
        plot(
            0,
            xlim = c(-1.5, 1.5) * node.radiusius + xpos,
            ylim = c(-1.5, 1.5) * node.radiusius + ypos,
            type = 'n',
            axes = FALSE,
            xlab = '',
            ylab = ''
            );
        }

    validx <- which(!is.na(x) & x > 0);
    x <- c(0, cumsum(x[validx]) / sum(x[validx]));
    dx <- diff(x);
    nx <- length(dx);

    if (is.null(col)) {
        col <- rainbow(nx);
        }

    else if (length(col) < nx) {
        col <- rep(col, ceiling(nx / length(col)));
        }

    xylim <- par('usr');
    plotdim <- par('pin');
    ynode.radiusius <- node.radiusius * (xylim[4] - xylim[3]) / (xylim[2] - xylim[1]) * plotdim[1] / plotdim[2];
    bc <- 2 * pi * (x[1:nx] + dx / 2) + startpos;

    for (i in 1:nx) {
        n <- max(2, floor(edges * dx[i]))
        t2p <- 2 * pi * seq(x[i], x[i + 1], length = n) + startpos;
        xc <- c(cos(t2p) * node.radiusius + xpos, xpos);
        yc <- c(sin(t2p) * ynode.radiusius + ypos, ypos);

        polygonGrob(xc, yc, gp = gpar(fill = col[i]), ...);
        t2p <- 2 * pi * mean(x[i + 0:1]) + startpos;
        xc <- cos(t2p) * node.radiusius;
        yc <- sin(t2p) * node.radiusius;
        }

    return(bc);
    }
