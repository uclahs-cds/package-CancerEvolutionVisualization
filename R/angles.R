calculate.angles.radial <- function(v, tree, spread, total.angle) {
    root.node.id <- v$id[[1]];
    node.ids <- c(root.node.id);

    total.angle <- total.angle * spread;
    angles <- numeric(nrow(tree));

    while (length(node.ids) > 0) {
        current.node.id <- node.ids[1];
        node.ids <- node.ids[-1];

        parent.id <- tree$parent[tree$tip == current.node.id];

        if (parent.id == -1) {
            tree$angle[tree$tip == current.node.id] <- 0;
            }

        child.ids <- tree$tip[tree$parent == current.node.id & !is.na(tree$parent)];
        num.children <- length(child.ids);

        if (length(child.ids) > 0) {
            parent.angle <- parent.angle <- tree$angle[tree$tip == current.node.id];
            child.weight <- assign.weight(current.node.id, v);

            start.angle <- parent.angle - (total.angle) * (num.children > 1) / 2;
            num.slices <- max(num.children - 1, 1);
            angle.increment <- total.angle / num.slices;

            for (i in seq_along(child.ids)) {
                child.id <- child.ids[i];
                angle <- start.angle + (i - 1) * (angle.increment);
                angles[tree$tip == child.id] <- angle;
                }

            node.ids <- append(node.ids, child.ids);
            }
        }
    
    angles <- override.angles(tree, v, angles);
    return(angles);
    }

calculate.angles.fixed <- function(v, tree, fixed.angle) {
    angles <- numeric(nrow(tree));
    node.ids <- c(v$id[[1]]);

    while (length(node.ids) > 0) {
        current.node.id <- node.ids[1];
        node.ids <- node.ids[-1];

        child.ids <- tree$tip[tree$parent == current.node.id & !is.na(tree$parent)];

        if (length(child.ids) > 0) {
            # Safe to hardcode temporarily. This will only ever apply to
            # cases with 0, 1, or 2 children. 3+ will use radial calculation.

            # In future, I would like to remove this fixed angle calculation entirely.
            # It would be ideal to handle all calculations in the same way, and
            # rely more on user defined spread and explicit angle overrides.
            child.angles <- if (length(child.ids) == 1) c(0) else c(-1, 1) * fixed.angle;

            for (i in seq_along(child.ids)) {
                child.id <- child.ids[i];
                angle <- child.angles[i];
                angles[tree$tip == child.id] <- angle;
                }
            }

        node.ids <- append(node.ids, child.ids);
        }

    angles <- override.angles(tree, v, angles);
    return(angles);
    }

override.angles <- function(tree, v, angles) {
    angle.index <- as.list(angles);
    names(angle.index) <- v$id;
    angle.index <- angle.index[!is.na(angle.index)];

    angles <- apply(
        tree,
        MARGIN = 1,
        FUN = function(x) {
            node.id <- as.character(x['tip']);
            angle.override <- angle.index[[node.id]];
            angle <- if (is.null(angle.override)) x['angle'] else angle.override;
            return(angle);
            }
        );

    return(angles);
    }
