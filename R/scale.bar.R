add.scale.bar <- function(
    clone.out,
    scale.length,
    scale1,
    scale2,
    yaxis1.label,
    yaxis2.label,
    pos,
    ...
    ) {

    # Necessary to make sure scale.length corresponds to tree
    vp.unclipped <- make.plot.viewport(clone.out, clip = 'off');
    # Generate the first scale bar
    scale.bar1.glist <- create.scale.bar(
        main = yaxis1.label,
        visual.length = 0.5,
        scale.length = get.scale.bar.length(scale.length[1]),
        edge.col = most.common.value(clone.out$v$edge.colour.1),
        edge.width = most.common.value(clone.out$v$edge.width.1),
        edge.type = most.common.value(clone.out$v$edge.type.1),
        left.x = pos[1],
        top.y = pos[2],
        ...
        );
    clone.out$grobs <- c(clone.out$grobs, scale.bar1.glist);
    # clone.out$grobs <- c(clone.out$grobs, list(
    #     gTree(
    #         children = scale.bar1.glist,
    #         vp = vp.unclipped
    #         )
    #     ));

    # Create second scalebar if specified
    if (!is.null(yaxis2.label)) {
        scale.bar2.glist <- create.scale.bar(
            main = yaxis2.label,
            visual.length = 0.5,
            scale.length = get.scale.bar.length(scale.length[2], scale1 / scale2),
            edge.col = most.common.value(clone.out$v$edge.colour.2),
            edge.width = most.common.value(clone.out$v$edge.width.2),
            edge.type = most.common.value(clone.out$v$edge.type.2),
            left.x = pos[1],
            top.y = pos[2] + 0.1,
            ...
            );
        clone.out$grobs <- c(clone.out$grobs, scale.bar2.glist);
        # clone.out$grobs <- c(clone.out$grobs, list(
        #     gTree(
        #         children = scale.bar2.glist
        #         vp = vp.unclipped
        #         )
        #     ));
        }
    }

get.scale.bar.length <- function(
    scale.length,
    conversion.factor = 1
    ) {

    adjusted.length <- 10 ** floor(log10(as.numeric(scale.length)));
    return(list(
        label = adjusted.length,
        length = adjusted.length / conversion.factor
        ));
    }

most.common.value <- function(x) {
    if (is.null(x)) {
        return(NULL);
        }
    n.table <- table(x);
    return(names(n.table)[which.max(n.table)]);
    }

create.scale.bar <- function(
        main,
        visual.length,
        scale.length,
        left.x,
        top.y,
        edge.col,
        edge.width,
        edge.type,
        main.cex,
        label.cex
        ) {

    # do unit stuff internally
    print('ran title');
    edge.width <- unit(edge.width, "points");
    # xat <- unit(left.x, "npc") + c(0, convertUnit(scale.length$length, 'npc', valueOnly = TRUE));
    # visual.length <- convertUnit(scale.length$length, 'npc');
    visual.length <- unit(scale.length$length, 'native');
    # visual.length <- unit(as.numeric(visual.length), "npc");
    left.x <- unit(left.x - 1, "npc");
    top.y <- unit(top.y, "npc");
    xat <- left.x + unit(c(0, scale.length$length), 'native');
    # main.cex <- unit(14, "points");
    title <- textGrob(
        label = main,
        x = xat[1],
        y = top.y,
        hjust = 0.5,
        vjust = 1,
        gp = gpar(
            cex = main.cex
            )
        );
    print('ran title');
    # scale.bar.y <- convertY(top.y - (main.cex * 2), "npc");
    scale.bar.y <- top.y - unit(0.05, 'npc');
    scale.line <- segmentsGrob(
        x0 = xat[1],
        # x1 = left.x + visual.length,
        x1 = xat[2],
        y0 = scale.bar.y,
        y1 = scale.bar.y,
        gp = gpar(
            col = edge.col,
            lwd = edge.width,
            lty = edge.type,
            lineend = "butt"
            )
        );
    print('ran scale.line');
    # label.cex = unit(12, "points");
    tick.length <- edge.width + unit(label.cex / 100, 'npc');
    ticks <- segmentsGrob(
        # x0 = c(left.x, left.x + visual.length),
        # x1 = c(left.x, left.x + visual.length),
        x0 = xat,
        x1 = xat,
        y0 = scale.bar.y + (edge.width / 2.5),
        y1 = scale.bar.y - tick.length,
        default.units = 'native',
        gp = gpar(
            lineend = "butt"
            )
        );
    print('ran tick.length');
    tick.labels <- textGrob(
        label = c(0, scale.length$label),
        # x = c(left.x, left.x + visual.length),
        x = xat,
        y = scale.bar.y - tick.length * 2,
        gp = gpar(
            cex = label.cex
            )
        );
    print('ran tick.labels');
    return(gList(
        title,
        scale.line,
        ticks,
        tick.labels
        ));
    }