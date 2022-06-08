calculate.main.plot.size <- function(clone.out, scale1, wid, min.width, xaxis.space.left, xaxis.space.right, rad){
	ymax <- max(clone.out$v$len) + clone.out$v$y[1]
	height <- (ymax)*scale1
	
	if(is.null(min.width)){
	  xmax <- wid
	  width <- wid*scale1+xaxis.space.left + xaxis.space.right + 4*rad
	  xlims <- c(-xmax/2-(xaxis.space.left*1/scale1 +2*rad/scale1), xmax/2+(xaxis.space.right*(1/scale1) +2*rad/scale1))
	 } else{	  
	  xmin <- min(c(clone.out$v$x))
	  xmax <- max(c(clone.out$v$x))
	  xlims <- c(xmin-(xaxis.space.left*1/scale1),xmax+xaxis.space.right*(1/scale1))
	  width <- (max(xlims)-min(xlims))*scale1
	  diff <- min.width - width
	  if(diff > 0){
	    xmin <- xmin-0.5*diff*1/scale1
	    xmax <- xmax+0.5*diff*1/scale1
	    xlims <- c(xmin-(xaxis.space.left*1/scale1 +2*rad/scale1),xmax+xaxis.space.right*(1/scale1)+2*rad/scale1)
	    width <- (max(xlims)-min(xlims))*scale1
	  }
	}
	clone.out$height <- height
	clone.out$width <- width
	clone.out$xlims <- xlims
	clone.out$ymax <- ymax
	clone.out$vp <- make.plot.viewport(clone.out, clip= if(clone.out$no.ccf == TRUE) 'off' else 'on', just=c('centre','top'), y=0.9)
}

make.plot.viewport <- function(clone.out, clip='on', just=c('centre', 'centre'), y=0.5){
	vp <- viewport(y=y, height=unit(clone.out$height,'inches'), width=unit(clone.out$width, 'inches'), name='plot.vp', xscale=clone.out$xlims,  yscale=c(clone.out$ymax,0), just=just, gp=gpar(fill='pink'), clip=clip)
	return(vp)
}

extend.axis <- function(axisGrob, limits, type){
	arg.list <- list(getGrob(axisGrob, 'major'), limits)
	names(arg.list) <- c('grob',type)
	axisGrob <- setGrob(axisGrob, 'major', do.call(editGrob, arg.list))
	return(axisGrob)
}

add.axis.label <- function(axisGrob, axis.label, axis.position, axis.label.cex, vp){	
	if(axis.position == 'bottom'){
		d <- 'y'
		just <- c('centre','top')
		rot <- 0
		x <- unit(0.5, 'npc')
		y <- (getGrob(axisGrob, 'labels')$y+getGrob(axisGrob, 'ticks')$y1)*1.75
	} else {
		pushViewport(vp)
		tcklen  <- unit(diff(c(as.numeric(getGrob(axisGrob, 'ticks')$x0), as.numeric(getGrob(axisGrob, 'ticks')$x1))),'lines')
		y <-  convertY(unit(max(as.numeric(getGrob(axisGrob, 'major')$y))*.5,'native'), 'inches')			

		if(axis.position == 'left'){
			d <- 'x'
			just <- c('centre', 'centre')
			rot <- 90
			
			x <- unit(convertX(grobWidth(getGrob(axisGrob, 'labels')), 'mm', valueOnly=TRUE)*axisGrob$gp$cex*-1 + convertX(unit(1,'lines')*axisGrob$gp$cex,'mm', valueOnly=TRUE)*-1+ convertX(getGrob(axisGrob, 'labels')$x*axisGrob$gp$cex, 'mm', valueOnly=TRUE),'mm')
		} else if(axis.position == 'right'){
			d <- 'x'
			just <- c('left','centre')
			x <- (getGrob(axisGrob, 'labels')$x+tcklen)*1.5
			rot <- 270
		}
		popViewport()
	}

	axis.lab <- textGrob(name = 'axis.label', axis.label, gp=gpar(cex=axis.label.cex), vjust=0, x=x, rot=rot, y=y)
	axis.gTree <- gTree(name = paste0('axis.', axis.position), children=gList(axis.lab, axisGrob), vp=vp)
	return(axis.gTree)
}

add.axes <- function(clone.out,  scale1, scale2=NULL, yaxis.position='left', xaxis.label='CCF', yaxis1.label='PGA', yaxis2.label=NULL, yaxis1.interval=10, yaxis2.interval=NA, no.ccf=FALSE, axis.label.cex=list(x=1.55,y=1.55), axis.cex=list(x=1,y=1), ylabels1=NULL, ylabels2=NULL, ylimit=NULL){
	if(no.ccf == FALSE & 'ccf' %in% colnames(clone.out$v) & all(!is.na(clone.out$v$ccf))){		
		add.xaxis(clone.out, scale1=scale1, axis.label=xaxis.label, no.ccf=no.ccf, axis.label.cex=axis.label.cex[['x']], axis.cex=axis.cex[['x']])
	}
	if( yaxis.position == 'both' ){
		if(is.null(yaxis2.label)){
			warning('Missing second y-axis label')
			yaxis2.label <- ''
		}
		conversion.factor <- scale1/scale2
		ymax1 <- add.yaxis(clone.out, yaxis.position='left', axis1.label=yaxis1.label, yaxis1.interval=yaxis1.interval, no.ccf=no.ccf, axis.label.cex=axis.label.cex[['y']], axis.cex=axis.cex[['y']],ylabels=ylabels1, ylimit=ylimit)
		add.yaxis(clone.out, yaxis.position='right', conversion.factor=conversion.factor, axis1.label=yaxis2.label, yaxis1.interval=yaxis2.interval, no.ccf=no.ccf, axis.label.cex=axis.label.cex[['y']], axis.cex=axis.cex[['y']],ylabels=ylabels2, ylimit=ymax1)
	}else{
		add.yaxis(clone.out, yaxis.position=yaxis.position, axis1.label=yaxis1.label, yaxis1.interval=yaxis1.interval, no.ccf=no.ccf, axis.label.cex=axis.label.cex[['y']], axis.cex=axis.cex[['y']],ylabels=ylabels1, ylimit=ylimit)
	}
}

add.yaxis <- function(
    clone.out,
    yaxis.position = 'left',
    conversion.factor = 1,
    axis1.label = 'PGA',
    yaxis2.label = NULL,
    yaxis1.interval = NA,
    no.ccf = FALSE,
    ylimit = NULL,
    axis.label.cex = list(x = 1.55, y = 1.55),
    axis.cex = list(x = 1, y = 1),
    ylabels = NULL
    ) {
    # Necessary to get the right positioning
    vp.unclipped <- make.plot.viewport(clone.out, clip = 'off');

	if (!is.null(ylimit) && ylimit == 'node'|| (is.null(ylimit) && no.ccf)){
		ymax <- max(clone.out$v$y);
	} else if (!is.null(ylimit) && ylimit == 'plot.length'|| (is.null(ylimit) && !no.ccf)) {
		ymax <- clone.out$ymax;
	} else if (!is.null(ylimit)) {
		ymax <- ylimit;
	    }

    # Set up tick labels
	if (is.null(ylabels)) {
		if(!is.na(yaxis1.interval)) {
			ylabels <- seq(0, ymax * conversion.factor, by = yaxis1.interval);

			if (no.ccf && max(ylabels) < ymax) {
				ylabels <- c(ylabels, max(ylabels) + yaxis1.interval);
			    }
	    } else {
		    ylabels <- pretty(seq(0, ymax * conversion.factor));
	        }
        }

	yat <- ylabels / conversion.factor;

	yaxis1 <- yaxisGrob(
	    name = 'axis.content',
	    at = yat,
	    label = ylabels,
	    gp = gpar(cex = axis.cex),
	    main = yaxis.position == 'left'
	    );

    if(max(yat)/conversion.factor != ymax & no.ccf == FALSE){ #extending the axis line beyond the last tick 
	    yaxis1 <- extend.axis(yaxis1, limits=unit(c(0,ymax),'native'), type='y')
        }		

	yaxis.gTree <- add.axis.label(
	    yaxis1,
	    axis1.label,
	    axis.position = yaxis.position,
	    axis.label.cex,
	    vp = vp.unclipped
	    );

	clone.out$grobs <- c(clone.out$grobs, list(yaxis.gTree));

	return(ymax)
    }

add.xaxis <- function(
    clone.out,
    scale1,
    axis.label = 'CCF',
    no.ccf = FALSE,
    axis.label.cex = 1.55,
    axis.cex = 1
    ) {

    # Necessary to get the right positioning
	vp.unclipped <- make.plot.viewport(clone.out, clip='off') 

	# Set up tick labels
	clone.widths <- as.numeric(as.matrix(clone.out$v[, c('x1', 'x2')]));
	xat <- c(min(clone.widths), max(clone.widths));
	xlabels <- c(0, paste0(round(max(clone.out$v$ccf) * 100, 0), '%'));

	xaxis <- xaxisGrob(
	    name = 'axis.content',
	    at = xat,
	    label = xlabels,
	    gp = gpar(cex = axis.label.cex),
	    main = TRUE
	    );

	#move the labels up a little 
	xaxis.labels <- editGrob(
	    getGrob(xaxis, 'labels'),
	    y = unit(-0.09, 'npc'),
	    vjust = 1
	    );

	xaxis <- setGrob(
	    xaxis,
	    'labels',
	    xaxis.labels
	    );

	if (diff(xat) / scale1 != clone.out$width) {
	    # Extending the axis line beyond the clone limits
		xaxis <- extend.axis(
		    xaxis,
		    limits = unit(clone.out$xlims,'native'),
		    type = 'x'
		    );
	    }

	# Add the axis label
	xaxis.gTree <- add.axis.label(
	    xaxis,
	    axis.label,
	    axis.position = 'bottom',
	    axis.label.cex,
	    vp = vp.unclipped
	    );

	clone.out$grobs <- c(clone.out$grobs, list(xaxis.gTree));
    }

add.title <- function(
    clone.out,
    title,
    title.cex,
    title.y = NULL,
    title.y.units = 'npc'
    ) {

	y.pos <- unit(1.08,'npc');

	if (!is.null(title.y)) {
		pushViewport(clone.out$vp);
		plot.top <- convertY(unit(1,'npc'), title.y.units, valueOnly = TRUE);
		popViewport();
		y.pos <- plot.top + title.y;
		}
	
	title.label <- textGrob(
	    title,
	    just = 'center',
	    gp = gpar(
	        col = 'black',
	        cex = title.cex
	        ));

	title.grob <- gTree(
	    children = gList(title.label),
	    name = 'title.gtree',
	    cl = 'title.label',
	    vp = vpStack(
	        make.plot.viewport(
	            clone.out,
	            clip = 'off',
	            just = c('centre', 'centre')
	            ),
	        viewport(
	            y = unit(y.pos, title.y.units),
	            x = unit(0, 'native'),
	            height = grobHeight(title.label),
	            width = grobWidth(title.label),
	            just = c('centre', 'bottom')
	            )
	        )
	    );

	clone.out$grobs <- c(clone.out$grobs, list(title.grob));
    }
