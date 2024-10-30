prep.scale.length <- function(
    tree,
    scale.size.1,
    scale.size.2
    ) {

    scale.lengths <- c(scale.size.1, scale.size.2);
    tree.lengths <- c(
        auto.scale.length(tree$length1),
        if ('length2' %in% names(tree)) auto.scale.length(tree$length2) else NA
        );

    # if scale.length is NA, replace with tree.lengths
    scale.lengths[is.na(scale.lengths)] <- tree.lengths[is.na(scale.lengths)];

    return(scale.lengths);
    }

auto.scale.length <- function(edge.lengths) {
    scale.length <- median(edge.lengths[edge.lengths > 0], na.rm = TRUE);
    adjusted.length <- 10 ** floor(log10(as.numeric(scale.length)));
    return(adjusted.length);
    }

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

    # Necessary to get the right positioning
    vp.unclipped <- make.plot.viewport(clone.out, clip = 'off');

    # Generate the first scale bar
    scale.bar1 <- create.scale.bar(
        main = yaxis1.label,
        scale.length = list(
            label = scale.length[1],
            length = scale.length[1]
            ),
        edge.col = most.common.value(clone.out$v$edge.colour.1),
        edge.width = most.common.value(clone.out$v$edge.width.1),
        edge.type = most.common.value(clone.out$v$edge.type.1),
        left.x = pos[1],
        top.y = pos[2],
        ...
        );

    clone.out$grobs <- c(
        clone.out$grobs,
        list(gTree(children = gList(scale.bar1), vp = vp.unclipped))
        );

    # Create second scalebar if specified
    if (!is.null(yaxis2.label)) {
        scale.bar2 <- create.scale.bar(
            main = yaxis2.label,
            scale.length = list(
                label = scale.length[2],
                length = scale.length[2] / scale1 * scale2
                ),
            edge.col = most.common.value(clone.out$v$edge.colour.2),
            edge.width = most.common.value(clone.out$v$edge.width.2),
            edge.type = most.common.value(clone.out$v$edge.type.2),
            left.x = pos[1],
            top.y = pos[2] + 0.1,
            ...
            );
        clone.out$grobs <- c(
            clone.out$grobs,
            list(gTree(children = gList(scale.bar2), vp = vp.unclipped))
            );
        }
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
    scale.length,
    left.x,
    top.y,
    edge.col,
    edge.width,
    edge.type,
    main.cex,
    label.cex
    ) {

    # Viewport for the scale bar that centers it without scaling distortion
    scale.vp <- viewport(
        x = unit(left.x, "npc"),    # Centered in the parent viewport
        y = unit(top.y, "npc"),     # Adjust y-position based on parent viewport
        width = unit(1, "native"),  # Native units for scale length
        height = unit(1, "native"), # Matches native scaling
        just = "center"
        );

    edge.width <- unit(edge.width, 'points');
    main.size <- unit(main.cex * 12, 'points');
    label.size <- unit(label.cex * 12, 'points');

    # Define coordinates within scale.vp
    vp.x <- unit(0.5, "npc");
    vp.y <- unit(0.5, "npc");
    xat <- vp.x + unit(c(-1, 1) * (scale.length$length / 2), 'native');

    title <- textGrob(
        label = main,
        x = vp.x,
        y = vp.y + main.size,
        gp = gpar(
            cex = main.cex
            )
        );

    scale.line <- segmentsGrob(
        x0 = xat[1],
        x1 = xat[2],
        y0 = vp.y,
        y1 = vp.y,
        gp = gpar(
            col = edge.col,
            lwd = edge.width,
            lty = edge.type,
            lineend = 'butt'
            )
        );

    tick.length <- edge.width + (label.size / 4);
    ticks <- segmentsGrob(
        x0 = xat,
        x1 = xat,
        y0 = vp.y + (edge.width / 2.5),
        y1 = vp.y - tick.length,
        default.units = 'native',
        gp = gpar(
            lineend = 'butt'
            )
        );

    tick.labels <- textGrob(
        label = c(0, scale.length$label),
        x = xat,
        y = vp.y - tick.length * 2,
        gp = gpar(
            cex = label.cex
            )
        );

    return(gTree(
        children = gList(title, scale.line, ticks, tick.labels),
        vp = scale.vp
        ));
    }
