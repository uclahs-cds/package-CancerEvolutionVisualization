axis.overlap <- function(
    xpos,
    node.text,
    line.dist,
    axis.type,
    cex,panel.width,
    return.cex = FALSE
    ) {

    # function checks if the node text will cross over the axis
    node.text.xrange <- sort(c(
        xpos + line.dist,
        xpos + line.dist + strwidth(node.text, units = 'inches', cex = cex) * line.dist / abs(line.dist)
        ));

    overlaps <- NULL;

    if (axis.type == 'PGA' | axis.type == 'SNV single' | axis.type == 'left' && any(node.text.xrange < 0)) {
        overlaps <- TRUE;
    } else if (
        (axis.type == 'SNV' && any(node.text.xrange) > panel.width) ||
        axis.type == 'both' && any(node.text.xrange > panel.width) | any(node.text.xrange < 0)
    ) {
        overlaps <- TRUE;
        }

    # find a text size that prevents the axis overlap
    if (return.cex & !is.null(overlaps)) {
        new.cex <- cex;

        while (!is.null(overlaps)) {
            new.cex <- new.cex - 0.05;
            overlaps <- axis.overlap(xpos, node.text, line.dist, axis.type, new.cex, panel.width);
            }

        return(new.cex);
        }

    return(overlaps);
    }

check.overlap <- function(
    xpos,
    ypos,
    node.text,
    tree.max.adjusted,
    hjust,node.radius
    ) {

    # Check if the node text will cross over any of the branch lines
    if (hjust == 'centre') {
        left <- xpos - as.numeric(convertX(stringWidth(node.text), 'in')) / 2;
        right <- xpos + as.numeric(convertX(stringWidth(node.text), 'in')) / 2;
    } else if (hjust == 'left') {
        left <- xpos;
        right <- xpos + as.numeric(convertX(stringWidth(node.text), 'in'));
    } else if (hjust == 'right') {
        right <- xpos;
        left <- xpos - as.numeric(convertX(stringWidth(node.text), 'in'));
        }

    node.text.xrange <- c(left, right);

    node.segs <- tree.max.adjusted[, c('tip', 'parent', 'x', 'y')];
    node.segs$y0 <- tree.max.adjusted$y + node.radius;
    node.segs$y1 <- tree.max.adjusted$y - node.radius;
    node.segs$x0 <- tree.max.adjusted$x - node.radius;
    node.segs$x1 <- tree.max.adjusted$x + node.radius;

    line.intercept <- logical(length = nrow(tree.max.adjusted));
    node.intercept <- logical(length = nrow(tree.max.adjusted));

    for (i in seq_along(tree.max.adjusted[, 1])) {
        if (is.infinite(tree.max.adjusted$slope[i])) {
            #only overlaps with straight lines if the line's ypos is in the range
            line.intercept[i] <- (
                (ypos < tree.max.adjusted$y0[i]) &
                (ypos > tree.max.adjusted$y1[i]) &
                (node.text.xrange[1] < tree.max.adjusted$x0[i]) &
                (node.text.xrange[2] > tree.max.adjusted$x0[i])
                );
        } else {
            line.intercept.x <- (ypos - tree.max.adjusted$intercept[i]) / tree.max.adjusted$slope[i];

            if (
                line.intercept.x < max(c(tree.max.adjusted$x0[i], tree.max.adjusted$x1[i])) &
                line.intercept.x > min(c(tree.max.adjusted$x0[i], tree.max.adjusted$x1[i])) &
                (line.intercept.x > node.text.xrange[1]) & (line.intercept.x < node.text.xrange[2])
            ) {
                line.intercept[i] <- TRUE;
                }
            }

        node.intercept[i] <- (ypos < node.segs$y0[i]) &
                             (ypos > node.segs$y1[i]) &
                             (node.text.xrange[1] < node.segs$x0[i]) &
                             (node.text.xrange[2] > node.segs$x0[i])
        }

    intercepts.lines <- tree.max.adjusted$tip[line.intercept];
    intercepts.nodes <- tree.max.adjusted$tip[node.intercept];

    return(list(lines = intercepts.lines, nodes = intercepts.nodes));
    }

position.node.text <- function(
    tree.max.adjusted = NULL,
    node.list = NULL,
    node.text.col = NULL,
    node.text.fontface = NULL,
    axis.type = axis.type,
    panel.height = NULL,
    panel.width = NULL,
    main.y = NULL,
    line.dist = line.dist,
    hjust = NULL,
    node.radius = node.radius,
    alternating = FALSE,
    split = FALSE,
    label.nodes = FALSE,
    adjust.axis.overlap = TRUE,
    cex = cex
    ) {

    text.grob.list <- vector('list', length(unlist(node.list)));
    orig.cex <- cex;
    idx <- 1;

    for (s in seq_along(node.list)) {
        split.text <- FALSE;

        if (length(node.list[[s]]) == 0) {
            next;
        } else {
            slope <- tree.max.adjusted$slope[s];
            intercept <- tree.max.adjusted$intercept[s];
            y.height <- tree.max.adjusted$y0[s] - tree.max.adjusted$y1[s];

            label.bottom <- str.heightsum <- 0;
            cex <- orig.cex;

            #centre the height of all the text relative to the line
            while (
                str.heightsum == 0 |
                (label.bottom + str.heightsum) > (main.y + panel.height) |
                (label.nodes == FALSE &
                (label.bottom + str.heightsum) > (tree.max.adjusted$y0[s] + node.radius * 0.5))
            ) {
                if ((label.bottom + str.heightsum) > (tree.max.adjusted$y0[s] + node.radius * 0.5) & length(node.list[[s]]) > 1) {
                    split.text <- TRUE;
                    }

                str.heights <- sapply(
                    node.list[[s]],
                    FUN = function(x) {
                        strheight(x, units = 'inches', cex = cex);
                        }
                    );

                spacing <- 0.33 * mean(str.heights);
                str.heightsum <- sum(str.heights) + spacing * length(str.heights) - spacing;

                if (split & split.text) {
                    str.heights.left <- str.heights[1:ceiling(length(node.list[[s]]) / 2)];
                    str.heights.right <- str.heights[(ceiling(length(node.list[[s]]) / 2) + 1):length(node.list[[s]])];
                    str.heightsum.left <- sum(str.heights.left) + spacing * length(str.heights.left) - spacing;
                    str.heightsum.right <- sum(str.heights.right) + spacing * length(str.heights.right) - spacing;
                    str.heightsum <- max(c(str.heightsum.left, str.heightsum.right));
                    }

                if (!label.nodes) {
                    if (length(node.list[[s]]) == 1) {
                        # Centered when there is just one text row
                        # Otherwise position relative to the bottom of the textGrob
                        label.bottom  <- tree.max.adjusted$y1[s] + y.height / 2;
                        vjust <- 'center';
                    } else {
                        label.bottom  <- y.height / 2 - str.heightsum / 2 + tree.max.adjusted$y1[s];
                        vjust <- 'bottom';
                        }

                    if (s == 1 & ((str.heightsum - y.height) > node.radius) & !is.null(node.radius) & !is.null(scale)) {
                        label.bottom <- tree.max.adjusted$y1[s] - node.radius;
                        }
                } else {
                    label.bottom <- tree.max.adjusted$y[s] - 0.5 * str.heightsum;
                    }

                if (
                    (label.bottom + str.heightsum) > (main.y + panel.height) ||
                    (!label.nodes && (label.bottom + str.heightsum) > (tree.max.adjusted$y0[s] + node.radius * 0.5))
                ) {
                    cex <- cex - 0.05;
                    }
                }

            # Iterate through the text for a given node
            for (g in rev(seq_along(node.list[[s]]))) {
                heights <- ifelse(
                    (g - 1) == 0,
                    yes = 0,
                    no = sum(str.heights[c(1:(g - 1))])
                    );

                if (label.nodes) {
                    ypos <- tree.max.adjusted$y[s];
                    xpos <- tree.max.adjusted$x[s];
                    xline.dist <- line.dist + node.radius;
                    vjust <- 'center';
                } else {
                    ypos <- label.bottom + (g - 1) * spacing + heights - spacing;

                    #back computing the x position based on the intercept and the slope
                    xpos <- ifelse(
                        is.infinite(slope),
                        yes = tree.max.adjusted$x1[s],
                        no = (ypos - intercept) / slope
                        );

                    xline.dist <- line.dist;
                    }

                text.positions <- data.frame(
                    labels = character(length = length(node.list[[s]])),
                    x = numeric(length = length(node.list[[s]])),
                    y = numeric(length = length(node.list[[s]]))
                    );

                if (split & split.text) {
                    if (g <= ceiling(length(node.text.col[[s]]) / 2)) {
                        # offset.left <- ceiling(length(node.text.col[[s]])/2)
                        heights <- ifelse(
                            (g - 1) == 0,
                            yes = 0,
                            no = sum(str.heights.left[c(1:(g - 1))])
                            );

                        ypos <- label.bottom + (g - 1) * spacing + heights - spacing;
                        text.grob.list[[idx]] <- textGrob(
                            node.list[[s]][g],
                            x = unit(xpos - xline.dist, 'inches'),
                            y = unit(ypos,'inches'),
                            just = c('right', 'bottom'),
                            gp = gpar(col = node.text.col[[s]][g], cex = cex)
                            );
                    } else {
                        offset.left <- ceiling(length(node.text.col[[s]]) / 2);
                        heights <- ifelse((
                            g - offset.left - 1) == 0,
                            yes = 0,
                            no = sum(str.heights.right[c(1:(g - offset.left - 1))])
                            );

                        ypos <- label.bottom + (g - offset.left - 1) * spacing + heights - spacing;

                        text.grob.list[[idx]] <- textGrob(
                            node.list[[s]][g],
                            x = unit(xpos + xline.dist, 'inches'),
                            y = unit(ypos, 'inches'),
                            just = c('left', 'bottom'),
                            gp = gpar(col = node.text.col[[s]][g], cex = cex)
                            );
                        }
                } else if (alternating) {
                    # Alternate between placing the text to the left and to the right of the node
                    if (s %% 2 > 0) {
                        xline.dist.adj <- -(xline.dist);
                        just <- c('right', 'bottom');
                    } else {
                        just <- c('left', 'bottom');
                        xline.dist.adj <- xline.dist;
                        }

                    text.grob.list[[idx]] <- textGrob(
                        node.list[[s]][g],
                        x = unit(xpos + xline.dist.adj, 'inches'),
                        y = unit(ypos, 'inches'),
                        just = just,
                        gp = gpar(col = node.text.col[[s]][g], cex = cex)
                        );

                    if (adjust.axis.overlap) {
                        overlaps.axis  <- axis.overlap(
                            xpos, node.list[[s]][g],
                            xline.dist.adj,
                            axis.type,cex,
                            panel.width,
                            return.cex = TRUE
                            );

                        if (!is.null(overlaps.axis)) {
                            # If text overlaps the axis, shrink the text
                            text.grob.list <- position.node.text(
                                tree.max.adjusted = tree.max.adjusted,
                                node.list = node.list,
                                node.text.col = node.text.col,
                                node.text.fontface = node.text.fontface,
                                axis.type = axis.type,
                                panel.height = panel.height,
                                panel.width = panel.width,
                                main.y = main.y,
                                line.dist = line.dist,
                                cex = overlaps.axis,
                                node.radius = node.radius,
                                alternating = alternating,
                                split = split,
                                label.nodes = label.nodes
                                );

                            return(text.grob.list);
                            }
                        }
                } else {
                    if (slope > 0 || (is.infinite((slope)) && axis.type == 'SNV' )) {
                        xline.dist <- -(abs(xline.dist));
                    } else {
                        xline.dist <- abs(xline.dist);
                        }

                    hjust <- ifelse(xline.dist > 0, 'left', 'right');

                    if (label.nodes) {
                        node <- tree.max.adjusted[which(tree.max.adjusted$tip == tree.max.adjusted$tip[s]), ];
                        parent <- tree.max.adjusted[which(tree.max.adjusted$tip == tree.max.adjusted$parent[s]), ];
                        children <- tree.max.adjusted[which(tree.max.adjusted$parent == tree.max.adjusted$tip[s]), ];

                        if (nrow(children) > 1) {
                            if (nrow(children[which(children$x > node$x), ]) > nrow(children[which(children$x < node$x), ])) {
                                xline.dist <- -(abs(xline.dist));
                            } else if (nrow(children[which(children$x > node$x), ]) < nrow(children[which(children$x < node$x), ])) {
                                xline.dist <- abs(xline.dist);
                                }

                            if ((max(children$y) + node.radius) > label.bottom) {
                                ypos <- ypos + node.radius;
                                }

                            hjust <- ifelse(xline.dist > 0, 'left', 'right');
                        } else {
                            leaves <- tree.max.adjusted[!(tree.max.adjusted$tip %in% tree.max.adjusted$parent), ];
                            leaves <- leaves[order(leaves$x), ];

                            if (
                                (nrow(leaves) > 2 ||
                                (nrow(tree.max.adjusted) == 3 & nrow(leaves) == 2)) &&
                                node$angle != 0 & node$tip %in% leaves[c(2:(nrow(leaves) - 1)), ]$tip
                            ) {
                                text.height <- as.numeric(convertY(
                                    grobHeight(textGrob(
                                        node.list[[s]][g],
                                        gp = gpar(cex = cex)
                                        )),
                                    'in'
                                    ));

                                text.width <- as.numeric(convertX(
                                    grobWidth(textGrob(
                                        node.list[[s]][g],
                                        gp = gpar(cex = cex)
                                        )),
                                    'in'
                                    ));

                                ypos <- node$y - (text.height * 0.8 + node.radius) * cos(node$angle);
                                xpos <- node$x + (text.width * 0.25) * sin(node$angle);
                                xline.dist <- 0;
                                }
                            }
                        }

                    if (
                        label.nodes &&
                        tree.max.adjusted$parent[s] %in% tree.max.adjusted$tip &&
                        ((tree.max.adjusted[which(tree.max.adjusted$tip == tree.max.adjusted$parent[s]), ]$y - tree.max.adjusted$y[s]) < node.radius)
                    ) {
                        if (tree.max.adjusted$tip[s] %in% tree.max.adjusted$parent) {
                            ypos <- tree.max.adjusted$y[s] + 1 * node.radius + abs(xline.dist);
                            xline.dist <- 0;
                            hjust <- 'centre';
                            cex <- orig.cex;
                        } else {
                            ypos <- tree.max.adjusted$y[s];
                            cex <- orig.cex;

                            if (tree.max.adjusted$x[s] > parent$x) {
                                xline.dist <- abs(xline.dist);
                                hjust <- 'left';
                            } else {
                                hjust <- 'right';
                                xline.dist <- -(abs(xline.dist));
                                }
                            }
                    } else {
                        overlap <- check.overlap(
                            xpos + xline.dist,
                            ypos,
                            node.list[[s]][g],
                            tree.max.adjusted,
                            hjust,
                            node.radius
                            );

                        if (length(unlist(overlap)) > 0) {
                            xline.dist <- -(xline.dist);
                            overlap <- check.overlap(
                                xpos + xline.dist,
                                ypos,
                                node.list[[s]][g],
                                tree.max.adjusted,
                                hjust,
                                node.radius
                                );

                            # May need to modify xline.dist if length(unlist(overlap)) > 0

                            if (xline.dist != 0) {
                                hjust <- ifelse(xline.dist > 0, 'left', 'right');
                                }
                            }

                        if (adjust.axis.overlap) {
                            overlaps.axis  <- axis.overlap(
                                xpos,
                                node.list[[s]][g],
                                xline.dist,
                                axis.type,cex,
                                panel.width,
                                return.cex = TRUE
                                );

                            # Shrink the text if they overlap
                            if (!is.null(overlaps.axis)) {
                                text.grob.list <- position.node.text(
                                    tree.max.adjusted = tree.max.adjusted,
                                    node.list = node.list,
                                    node.text.col = node.text.col,
                                    node.text.fontface = node.text.fontface,
                                    axis.type = axis.type,
                                    panel.height = panel.height,
                                    panel.width = panel.width,
                                    main.y = main.y,
                                    line.dist = line.dist,
                                    cex = overlaps.axis,
                                    node.radius = node.radius,
                                    alternating = alternating,
                                    split = split,
                                    label.nodes = label.nodes
                                    );

                                return(text.grob.list);
                                }
                            }
                        }

                    text.grob.list[[idx]] <- textGrob(
                        node.list[[s]][g],
                        x = unit(xpos + xline.dist, 'inches'),
                        y = unit(ypos, 'inches'),
                        just = c(hjust, vjust),
                        gp = gpar(
                            col = node.text.col[[s]][g],
                            fontface = node.text.fontface[[s]][g],
                            cex = cex
                            )
                        );
                    }

                idx <- idx + 1;
                }
            }
        }

    return(text.grob.list);
    }

add.text2 <- function(
    tree,
    node.text,
    label.nodes = FALSE,
    cex = 1,
    line.dist = 0.5,
    v = NULL,
    main.y = NULL,
    panel.height = NULL,
    panel.width = NULL,
    xlims = NULL,
    ymax = ymax,
    axis.type = NULL,
    scale = NULL,
    node.radius = NULL,
    alternating = TRUE,
    split = TRUE,
    clone.out = NULL
    ) {

    # Radius in native units
    node.radius <- node.radius / scale;
    node.text <- node.text[node.text$node %in% tree$tip, ];

    node.list <- vector('list', nrow(tree));
    node.text.col <- vector('list', nrow(tree));
    node.text.fontface <- vector('list', nrow(tree));

    # Loop to assign text to nodes
    for (i in seq_len(nrow(node.text))) {
        text.row <- node.text[i, ];
        pos <- which(tree$tip == text.row$node);
        text.value <- text.row$name;

        # Check if the text contains an underscore and parse if necessary
        if (length(grep('_', text.value)) > 0) {
            text.split <- strsplit(text.value, split = '_')[[1]];
            node.text.value <- text.split[1];
            amp <- text.split[2];
            call <- paste0(node.text.value, '^\'A', amp, '\'');
            text.value <- parse(text = call);
            }

        # Assign text value, color, and font face
        node.list[[pos]] <- c(node.list[[pos]], text.value);
        node.text.col[[pos]] <- c(node.text.col[[pos]], if (!is.na(text.row$col)) text.row$col else 'black');
        node.text.fontface[[pos]] <- c(node.text.fontface[[pos]], if (!is.na(text.row$fontface)) text.row$fontface else 'plain');
        }

    tree.max <- apply(
        tree,
        MARGIN = 1,
        FUN = function(x) {
            if (x['parent'] == -1) {
                basex <- 0;
                basey <- 0;
            } else {
                basex <- v$x[v$id == x['parent']];
                basey <- v$y[v$id == x['parent']];
                }

            tipx <- v$x[v$id == x['tip']];
            tipy <- v$y[v$id == x['tip']];
            mode <- v$mode[v$id == x['tip']];

            return(data.frame(basex, basey, tipx, tipy, mode));
            }
        );
    tree.max <- do.call('rbind', tree.max);
    rownames(tree.max) <- rownames(tree);
    tree.max <- cbind(tree, tree.max);

    tree.max.adjusted <- as.data.frame(do.call(rbind, lapply(
        1:nrow(tree.max),
        FUN = function(i) {
            x <- tree.max[i, ];
            if (x['mode'] == 'radial') {
                angle <- x['angle'];
            } else if (x['mode'] == 'dendrogram') {
                angle <- 0;
                x['basex'] <- x['tipx'];
                }
            # 1 if positive angle, -1 if negative (or 0 degrees)
            angle.modifier <- (angle > 0) * 2 - 1;

            basex <- x['basex'] + (angle.modifier * node.radius * sin(angle));
            tipx  <- x['tipx']  - (angle.modifier * node.radius * sin(angle));
            basey <- x['basey'] + (angle.modifier * node.radius * cos(angle));
            tipy  <- x['tipy']  - (angle.modifier * node.radius * cos(angle));

            return(data.frame(basex, basey, tipx, tipy));
            })));
    tree.max.adjusted <- cbind(tree, tree.max.adjusted);

    # Push a viewport the same size as the final panel
    # to perform calculations using absolute size units
    if (!is.null(clone.out)) {
        pushViewport(clone.out$vp);
    } else {
        pushViewport(viewport(
            height = unit(panel.height, 'inches'),
            name = 'ref',
            width = unit(panel.width, 'inches'),
            xscale = xlims,
            yscale = c(ymax, -2)
            ));
        }

    tree.max.adjusted$x0 <- convertX(unit(tree.max.adjusted$basex, 'native'), 'inches', valueOnly = TRUE);
    tree.max.adjusted$x1 <- convertX(unit(tree.max.adjusted$tipx, 'native'), 'inches', valueOnly = TRUE);
    tree.max.adjusted$y0 <- convertY(unit(tree.max.adjusted$basey, 'native'), 'inches', valueOnly = TRUE);
    tree.max.adjusted$y1 <- convertY(unit(tree.max.adjusted$tipy, 'native'), 'inches', valueOnly = TRUE);

    tree.max.adjusted$y <- convertY(unit(tree.max$tipy, 'native'), 'inches', valueOnly = TRUE); # Actual node positions
    tree.max.adjusted$x <- convertX(unit(tree.max$tipx, 'native'), 'inches', valueOnly = TRUE);

    tree.max.adjusted$slope <- (tree.max.adjusted$y1 - tree.max.adjusted$y0) / (tree.max.adjusted$x1 - tree.max.adjusted$x0);
    tree.max.adjusted$intercept <- tree.max.adjusted$y1 - tree.max.adjusted$slope * tree.max.adjusted$x1;

    text.grob.list <- position.node.text(
        tree.max.adjusted = tree.max.adjusted,
        node.list = node.list,
        node.text.col = node.text.col,
        node.text.fontface = node.text.fontface,
        axis.type = axis.type,
        panel.height = panel.height,
        panel.width = panel.width,
        main.y = main.y,
        line.dist = line.dist,
        cex = cex,
        node.radius = node.radius,
        alternating = alternating,
        split = split,
        label.nodes = label.nodes
        );

    text.grob.gList <- do.call(gList, text.grob.list);

    grob.name <- 'node.text';

    if (!is.null(clone.out)) {
        popViewport();
        text.tree <- gTree(
            name = grob.name,
            children = text.grob.gList,
            vp = make.plot.viewport(clone.out, clip = 'off')
            );
        return(text.tree);
        }

    text.tree <- gTree(
        name = grob.name,
        children = text.grob.gList,
        childrenvp = viewport(
            height = unit(panel.height, 'inches'),
            name = 'ref',
            width = unit(panel.width, 'inches'),
            xscale = xlims,
            yscale = c(ymax, -2),
            clip = 'off'
            )
        );

    return(list(text.tree, tree.max.adjusted));
    }
