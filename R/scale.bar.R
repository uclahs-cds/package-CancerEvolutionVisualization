create.scale.bar <- function(
        main,
        visual.length,
        scale.length,
        left.x,
        top.y,
        edge.col,
        edge.width,
        edge.type
    ) {

    if (is.numeric(edge.width)) {
        edge.width <- unit(edge.width, "points");
        }

    title.fontsize = unit(14, "points");
    title <- textGrob(
        label = main,
        x = left.x + (visual.length / 2),
        y = top.y,
        hjust = 0.5,
        vjust = 1,
        gp = gpar(
            fontsize = title.fontsize
            )
        );

    scale.bar.y <- convertY(top.y - (title.fontsize * 2), "npc");
    scale.line <- segmentsGrob(
        x0 = left.x,
        x1 = left.x + visual.length,
        y0 = scale.bar.y,
        y1 = scale.bar.y,
        gp = gpar(
            col = edge.col,
            lwd = edge.width,
            lineend = "butt"
            )
        );

    tick.label.fontsize = unit(12, "points");
    tick.length <- edge.width + (tick.label.fontsize / 4);
    ticks <- segmentsGrob(
        x0 = c(left.x, left.x + visual.length),
        x1 = c(left.x, left.x + visual.length),
        y0 = scale.bar.y + (edge.width / 2.5),
        y1 = scale.bar.y - tick.length,
        gp = gpar(
            lineend = "butt"
            )
        );

    tick.labels <- textGrob(
        label = c(0, scale.length),
        x = c(left.x, left.x + visual.length),
        y = scale.bar.y - tick.length - tick.label.fontsize,
        gp = gpar(
            fontsize = tick.label.fontsize
            )
        );

    return(gList(
        title,
        scale.line,
        ticks,
        tick.labels
        ));
    }