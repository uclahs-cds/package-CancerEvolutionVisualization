###################################################################################################
# prep.scale.length
#
# Description:
# - Prepares the scale lengths for the tree plot based on the provided tree object and scale sizes.
#
# Arguments:
#  - tree          Tree data frame containing 'length1' and optionally 'length2' elements.
#  - scale.size.1  The desireds size for the first scale bar.
#  - scale.size.2  The desireds size for the second scale bar (optional).
#
# Returns:
#  - A vector of length 2 containing the prepared scale lengths.
#  - If 'scale.size.1' or 'scale.size.2' is NA, the corresponding tree length is used.

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

###################################################################################################
# auto.scale.length
#
# Description:
# - Automatically determines an appropriate scale length based on the provided edge lengths.
#
# Arguments:
#  - edge.lengths  A vector of edge lengths.
#
# Returns:
#  - The adjusted scale length, which is the median of the non-zero edge lengths rounded down to the nearest power of 10.

auto.scale.length <- function(edge.lengths) {
    scale.length <- median(edge.lengths[edge.lengths > 0], na.rm = TRUE);
    adjusted.length <- 10 ** floor(log10(as.numeric(scale.length)));
    return(adjusted.length);
    }

###################################################################################################
# add.scale.bar
#
# Description:
# - Adds a scale bar to the 'clone.out' object.
# - Can add one or two scale bars based on the provided arguments.
# - Utilizes the 'create.scale.bar' and 'most.common.value' helper functions.
#
# Arguments:
#  - clone.out      The plot object to which the scale bar(s) will be added.
#  - scale.length   A vector of length 2 specifying the scale lengths for the two scale bars.
#  - scale1         The scale value for the first scale bar.
#  - scale2         The scale value for the second scale bar.
#  - yaxis1.label   The label for the first scale bar.
#  - yaxis2.label   The label for the second scale bar (optional).
#  - pos            A vector of length 2 specifying the position (x, y) of the scale bar(s).
#  - padding        The padding between the two scale bars (if both are present).
#  - ...            Additional arguments passed to 'create.scale.bar'.
#
# Returns:
#  - The modified 'clone.out' object with the added scale bar(s).

add.scale.bar <- function(
    clone.out,
    scale.length,
    scale1,
    scale2,
    yaxis1.label,
    yaxis2.label,
    pos,
    padding,
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
            top.y = pos[2] + (padding / 10),
            ...
            );
        clone.out$grobs <- c(
            clone.out$grobs,
            list(gTree(children = gList(scale.bar2), vp = vp.unclipped))
            );
        }
    }

###################################################################################################
# most.common.value
#
# Description:
# - Finds the most common value in a vector.
#
# Arguments:
#  - x    The input vector.
#
# Returns:
#  - The most common value in the input vector, or NULL if the input is NULL.

most.common.value <- function(x) {
    if (is.null(x)) {
        return(NULL);
        }
    n.table <- table(x);
    return(names(n.table)[which.max(n.table)]);
    }

###################################################################################################
# create.scale.bar
#
# Description:
# - Creates a scale bar grob (graphical object) with a title, scale line, ticks, and labels.
#
# Arguments:
#  - main         The title of the scale bar.
#  - scale.length A list containing the length of the scale bar and its label.
#  - left.x       The left x-coordinate of the scale bar viewport in normalized parent coordinates (npc).
#  - top.y        The top y-coordinate of the scale bar viewport in normalized parent coordinates (npc).
#  - edge.col     The color of the scale bar line and ticks.
#  - edge.width   The width of the scale bar line and ticks in points.
#  - edge.type    The line type of the scale bar line (e.g., "solid", "dashed", "dotted").
#  - main.cex     The character expansion factor for the scale bar title.
#  - label.cex    The character expansion factor for the scale bar labels.
#
# Returns:
#  - A gTree object representing the scale bar and label.
#
# Details:
# - The function creates a viewport (scale.vp) to place the scale bar within a parent vp without scaling distortion.
# - It defines the coordinates and sizes of the scale bar elements (title, scale line, ticks, and labels)

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
        y = vp.y - tick.length * 2.5,
        gp = gpar(
            cex = label.cex
            )
        );

    return(gTree(
        children = gList(title, scale.line, ticks, tick.labels),
        vp = scale.vp
        ));
    }
