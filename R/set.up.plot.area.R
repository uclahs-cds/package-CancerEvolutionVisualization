get.plot.width <- function(horizontal.padding) {
    # TODO Calculate base.size based on the plot
    # This value is just a reasonable value used as a temporary fixed default
    base.size <- 3;

    # Padding is applied to both sides
    return(base.size + (horizontal.padding * 2))
    }

calculate.main.plot.size <- function(
    clone.out,
    scale1,
    wid,
    min.width,
    node.radius,
    start.angle = 0,
    horizontal.padding = 0,
    vertical.padding = 0
    ) {

    x.padding <- horizontal.padding / scale1;
    y.padding <- vertical.padding / scale1;

    all.x <- clone.out$v$x;
    all.y <- clone.out$v$y;
    if (!is.null(clone.out$clones) && length(clone.out$clones) > 0) {
        all.x <- c(all.x, unlist(lapply(clone.out$clones, function(cl) cl$x)));
        all.y <- c(all.y, unlist(lapply(clone.out$clones, function(cl) cl$y)));
        }

    xmin <- min(all.x) - x.padding;
    xmax <- max(all.x) + x.padding;
    ymin <- min(all.y) - y.padding;
    ymax <- max(all.y) + y.padding;

    # Guard against degenerate scales (e.g. all nodes at x=0 when polygons are disabled)
    if (xmax == xmin) {
        xmin <- xmin - 0.5;
        xmax <- xmax + 0.5;
        }

    xlims <- c(xmin, xmax);
    ylims <- c(ymax, ymin);

    width  <- (xmax - xmin) * scale1;
    height <- (ymax - ymin) * scale1;

    clone.out$height <- height;
    clone.out$width <- width;
    clone.out$xlims <- xlims;
    clone.out$ylims <- ylims;
    clone.out$ymax <- ymax;

    clone.out$vp <- make.plot.viewport(
        clone.out,
        clip = 'off' #if (clone.out$no.ccf) 'off' else 'on'
        );
    }

make.plot.viewport <- function(
    clone.out,
    clip = 'on',
    just = c('centre', 'centre'),
    y = 0.5
    ) {

    vp <- viewport(
        y = y,
        height = unit(clone.out$height, 'inches'),
        width = unit(clone.out$width, 'inches'),
        name = 'plot.vp',
        xscale = clone.out$xlims,
        yscale = clone.out$ylims,
        just = just,
        gp = gpar(fill = 'pink'),
        clip = clip
        );

    return(vp);
    }

extend.axis <- function(axisGrob, limits, type) {
    arg.list <- list(getGrob(axisGrob, 'major'), limits);
    names(arg.list) <- c('grob', type);
    axisGrob <- setGrob(axisGrob, 'major', do.call(editGrob, arg.list));

    return(axisGrob);
    }

add.axis.label <- function(
    axisGrob,
    axis.label,
    axis.position,
    axis.label.cex,
    vp,
    axis.padding = 1
    ) {

    pushViewport(vp);

    label.grob <- getGrob(axisGrob, 'labels');

    # The gap is expressed in the plot's own text lines, so it is measured
    # before the axis graphical parameters are applied.
    gap.mm <- convertX(unit(1.5 * axis.padding, 'lines'), 'mm', valueOnly = TRUE);

    # grid applies the axis gTree's gpar (in particular cex) to the tick labels
    # when the axis is drawn, so the tick labels have to be measured in that
    # context. Measuring the detached child grob understates both its offset
    # from the axis line and its own size, and the shortfall grows with the
    # width of the tick-label text. That left each axis title sitting a
    # different distance from its ticks (issue #183).
    pushViewport(viewport(gp = axisGrob$gp));

    if (axis.position == 'bottom') {
        just       <- c('centre', 'top');
        rot        <- 0;
        x          <- unit(0.5, 'npc');
        label.y.mm <- convertY(label.grob$y, 'mm', valueOnly = TRUE);
        label.h.mm <- convertY(grobHeight(label.grob), 'mm', valueOnly = TRUE);
        # bottom axis: tick labels extend downward from label.y.mm;
        # outer (bottom) edge = label.y.mm - label.h.mm; step down by gap
        y <- unit(label.y.mm - label.h.mm - gap.mm, 'mm');
    } else if (axis.position == 'top') {
        just       <- c('centre', 'bottom');
        rot        <- 0;
        x          <- unit(0.5, 'npc');
        label.y.mm <- convertY(label.grob$y, 'mm', valueOnly = TRUE);
        label.h.mm <- convertY(grobHeight(label.grob), 'mm', valueOnly = TRUE);
        # top axis: tick labels extend upward from label.y.mm;
        # outer (top) edge = label.y.mm + label.h.mm; step up by gap
        y <- unit(label.y.mm + label.h.mm + gap.mm, 'mm');
    } else {
        label.x.mm <- convertX(label.grob$x, 'mm', valueOnly = TRUE);
        label.w.mm <- convertX(grobWidth(label.grob), 'mm', valueOnly = TRUE);
        y <- unit(mean(as.numeric(getGrob(axisGrob, 'major')$y)), 'native');

        if (axis.position == 'left') {
            just <- c('right', 'centre');
            rot  <- 90;
            x <- unit(label.x.mm - label.w.mm - gap.mm, 'mm');
        } else if (axis.position == 'right') {
            just <- c('left', 'centre');
            rot  <- 270;
            x <- unit(label.x.mm + label.w.mm + gap.mm, 'mm');
            }
        }

    popViewport(2);

    axis.lab <- textGrob(
        name = 'axis.label',
        axis.label,
        gp = gpar(cex = axis.label.cex),
        vjust = 0,
        x = x,
        rot = rot,
        y = y
        );

    axis.gTree <- gTree(
        name = paste0('axis.', axis.position),
        children = gList(axis.lab, axisGrob),
        vp = vp
        );

    return(axis.gTree);
    }

is.horizontal.direction <- function(plotting.direction) {
    if (is.numeric(plotting.direction)) {
        return(isTRUE(all.equal(plotting.direction %% 180, 90)));
        }

    return(plotting.direction %in% c('left', 'right'));
    }

add.axes <- function(
    clone.out,
    yat,
    scale1,
    scale2 = NULL,
    scale.bar = FALSE,
    yaxis.position = 'left',
    xaxis.label = 'CCF',
    yaxis1.label = 'PGA',
    yaxis2.label = NULL,
    no.ccf = FALSE,
    axis.label.cex = list(x = 1.55, y = 1.55),
    axis.cex = list(x = 1, y = 1),
    ylab.axis.padding = 1,
    plotting.direction = 'down',
    add.polygons = FALSE
    ) {

    # Skip x-axis if plotting.direction is numeric (custom angle)
    draw.xaxis <- !is.numeric(plotting.direction);

    # A horizontal fish plot lays its clone polygons out along the vertical
    # axis, so the second (nSNV) scale no longer corresponds to anything the
    # reader can measure. Drop that axis only -- the first y-axis stays, and
    # every other plotting direction keeps both.
    if (add.polygons && identical(yaxis.position, 'both') && is.horizontal.direction(plotting.direction)) {
        yaxis.position <- 'left';
        }
    if (!no.ccf && 'ccf' %in% colnames(clone.out$v) && all(!is.na(clone.out$v$ccf)) && draw.xaxis) {
        add.xaxis(
            clone.out,
            scale1 = scale1,
            axis.label = xaxis.label,
            no.ccf = no.ccf,
            axis.label.cex = axis.label.cex[['x']],
            axis.cex = axis.cex[['x']],
            plotting.direction = plotting.direction
            );
        }

    if (yaxis.position != 'none' & scale.bar == FALSE) {
        ylabels1 <- unlist(yat[1]);
        ylabels2 <- unlist(yat[2]);

        if (yaxis.position == 'both') {
            if (is.null(yaxis2.label)) {
                warning('Missing second y-axis label');
                yaxis2.label <- '';
                }

            conversion.factor <- scale1 / scale2

            ymax1 <- add.yaxis(
                clone.out,
                yaxis.position = 'left',
                axis1.label = yaxis1.label,
                no.ccf = no.ccf,
                axis.label.cex = axis.label.cex[['y']],
                axis.cex = axis.cex[['y']],
                ylab.axis.padding = ylab.axis.padding,
                ylabels = ylabels1
                );

            add.yaxis(
                clone.out,
                yaxis.position = 'right',
                conversion.factor = conversion.factor,
                axis1.label = yaxis2.label,
                no.ccf = no.ccf,
                axis.label.cex = axis.label.cex[['y']],
                axis.cex = axis.cex[['y']],
                ylab.axis.padding = ylab.axis.padding,
                ylabels = ylabels2
                );
        } else {
            add.yaxis(
                clone.out,
                yaxis.position = yaxis.position,
                axis1.label = yaxis1.label,
                no.ccf = no.ccf,
                axis.label.cex = axis.label.cex[['y']],
                axis.cex = axis.cex[['y']],
                ylab.axis.padding = ylab.axis.padding,
                ylabels = ylabels1
                );
            }
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
    axis.label.cex = list(x = 1.55, y = 1.55),
    axis.cex = list(x = 1, y = 1),
    ylab.axis.padding = 1,
    ylabels = NULL
    ) {
    # Necessary to get the right positioning
    vp.unclipped <- make.plot.viewport(clone.out, clip = 'off');

    ymax <- clone.out$ymax;

    # Set up tick labels
    if (is.null(ylabels)) {
        ylabels <- get.default.yat(ymax, conversion.factor);
        }

    y.ticks.at <- ylabels / conversion.factor;

    if (length(ylabels) == 0 || length(y.ticks.at) == 0) {
        warning('No y-axis ticks to draw. Skipping axis rendering.');
        return(ymax);
        }

    yaxis1 <- yaxisGrob(
        name = 'axis.content',
        at = y.ticks.at,
        label = ylabels,
        gp = gpar(cex = axis.cex),
        main = yaxis.position == 'left'
        );

    if (max(y.ticks.at) / conversion.factor != ymax && !no.ccf) {
        # Extend the axis line beyond the last tick
        yaxis1 <- extend.axis(
            yaxis1,
            limits = unit(c(0, ymax), 'native'),
            type = 'y'
            );
        }

    yaxis.gTree <- add.axis.label(
        yaxis1,
        axis1.label,
        axis.position = yaxis.position,
        axis.label.cex,
        vp = vp.unclipped,
        axis.padding = ylab.axis.padding
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
    axis.cex = 1,
    plotting.direction = 'down'
    ) {

    # Determine axis position based on plotting direction
    # down -> bottom, up -> top, left -> left, right -> right
    axis.position <- switch(
        as.character(plotting.direction),
        'down' = 'bottom',
        'up' = 'top',
        'left' = 'left',
        'right' = 'right',
        'bottom'  # default
        );

    # Necessary to get the right positioning
    vp.unclipped <- make.plot.viewport(clone.out, clip = 'off');

    # Set up tick labels
    clone.widths <- as.numeric(as.matrix(clone.out$v[, c('x1', 'x2')]));
    xat <- c(min(clone.widths), max(clone.widths));
    xlabels <- c(0, paste0(round(max(clone.out$v$ccf) * 100, 0), '%'));

    # Create appropriate axis grob based on position
    if (axis.position %in% c('left', 'right')) {
        # For horizontal plots, use yaxisGrob
        xaxis <- yaxisGrob(
            name = 'axis.content',
            at = xat,
            label = xlabels,
            gp = gpar(cex = axis.label.cex),
            main = (axis.position == 'left')
            );
        xaxis <- extend.axis(xaxis, unit(clone.out$ylims, 'native'), type = 'y');
    } else {
        # For vertical plots, use xaxisGrob
        xaxis <- xaxisGrob(
            name = 'axis.content',
            at = xat,
            label = xlabels,
            gp = gpar(cex = axis.label.cex),
            main = (axis.position == 'bottom')
            );
        xaxis <- extend.axis(xaxis, unit(clone.out$xlims, 'native'), type = 'x');
        }
    # Add the axis label
    xaxis.gTree <- add.axis.label(
        xaxis,
        axis.label,
        axis.position = axis.position,
        axis.label.cex,
        vp = vp.unclipped
        );

    clone.out$grobs <- c(clone.out$grobs, list(xaxis.gTree));
    }

add.main <- function(
    clone.out,
    main,
    main.cex,
    main.y = NULL,
    size.units = 'npc'
    ) {

    # y.pos <- unit(1.08,'npc');
    y.pos <- unit(0.5,'npc');

    if (!is.null(main.y)) {
        pushViewport(clone.out$vp);
        plot.top <- convertY(unit(1,'npc'), size.units, valueOnly = TRUE);
        popViewport();
        y.pos <- plot.top + main.y;
        }

    main.label <- textGrob(
        main,
        just = 'center',
        gp = gpar(
            col = 'black',
            cex = main.cex
            ));

    main.grob <- gTree(
        children = gList(main.label),
        name = 'main.gtree',
        cl = 'main.label',
        vp = vpStack(
            make.plot.viewport(
                clone.out,
                clip = 'off',
                just = c('centre', 'centre')
                ),
            viewport(
                y = unit(y.pos, size.units),
                x = unit(0, 'native'),
                height = grobHeight(main.label),
                width = grobWidth(main.label),
                just = c('centre', 'bottom')
                )
            )
        );

    clone.out$grobs <- c(clone.out$grobs, list(main.grob));
    }
