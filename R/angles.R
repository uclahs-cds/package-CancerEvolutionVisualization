calculate.angles.radial <- function(
    v,
    tree,
    spread,
    total.angle,
    start.angle
    ) {
    root.node.id <- v$id[[1]];
    node.ids <- c(root.node.id);

    angles <- v$angle;
    x <- numeric(nrow(v));

    while (length(node.ids) > 0) { # each iteration assigns an angle to the children of the current node
        # "Pops" next element in FIFO queue node.ids
        current.node.id <- as.numeric(node.ids[1]);
        node.ids <- node.ids[-1];

        parent.id <- tree$parent[tree$tip == current.node.id];

        child.ids <- as.numeric(
            tree$tip[tree$parent == current.node.id & !is.na(tree$parent)]
            );
        num.children <- length(child.ids);

        if (num.children > 0) {
            parent.angle <- angles[tree$tip == current.node.id];

            if (unique(v$mode[v$id %in% child.ids]) == 'radial') {
                if (is.na(parent.angle) || length(parent.angle) == 0) {
                    parent.angle <- start.angle;
                    angles[tree$tip == current.node.id] <- parent.angle;
                    }
                # if all children are radial, spread evenly by angle
                level.spread <- calculate.level.spread(v$spread[v$id %in% child.ids]);
                level.total.angle <- total.angle * level.spread;

                angles <- split.equal.angle(
                    base.total.angle = total.angle,
                    level.total.angle = level.total.angle,
                    child.ids = child.ids,
                    angles = angles,
                    tree = tree,
                    v = v
                    );
                if (v$mode[v$id == current.node.id] == 'radial') {
                    #if parent is 'radial' adjust starting angle
                    angles[tree$tip %in% child.ids] <- parent.angle + angles[tree$tip %in% child.ids];
                    }
            } else if (unique(v$mode[v$id %in% child.ids]) == 'dendrogram') {
                if (is.na(parent.angle) || length(parent.angle) == 0) {
                    parent.angle <- 0;
                    angles[tree$tip == current.node.id] <- parent.angle;
                    }
                # sort children by complexity
                child.ids <- v$id[v$id %in% child.ids];
                # if all children are dendrogram, spread evenly by x distance
                angles <- split.equal.x.dist(
                    current.node.id = current.node.id,
                    child.ids = child.ids,
                    angles = angles,
                    tree = tree,
                    v = v,
                    dx.scale = median(tree$length1)
                    );
                }

            # Appending to end of queue for breadth-first traversal
            node.ids <- append(node.ids, child.ids);
            }
        }
    return(angles);
    }

split.equal.angle <- function(
    base.total.angle,
    level.total.angle,
    child.ids,
    angles,
    tree,
    v
    ) {

    num.children <- length(child.ids);
    num.slices <- max(num.children - 1, 1);
    angle.increment <- base.total.angle / num.slices;
    start.angle <- - (level.total.angle) * (num.children > 1) / 2;

    previous.angle <- start.angle;
    for (i in seq_along(child.ids)) {
        child.id <- child.ids[i];
        angle <- angles[tree$tip == child.id];

        if (is.na(angle)) {
            if (i == 1) {
                angle <- start.angle;
            } else {
                pair.spread <- v$spread[v$id %in% child.ids[c(i - 1, i)]];
                angle <- previous.angle + angle.increment * mean(pair.spread);
                }
            angles[tree$tip == child.id] <- angle;
            }
        previous.angle <- angle;
        }
    return(angles);
    }

split.equal.x.dist <- function(
    current.node.id,
    child.ids,
    angles,
    tree,
    v,
    dx.scale
    ) {

    num.slices <- v$leaves[v$id == current.node.id];
    x.pos <- 1:num.slices - num.slices %/% 2;

    if (num.slices %% 2 == 0) { # if even
        x.pos <- x.pos - 0.5
    } else {
       x.pos <- x.pos - 1
    }

    idx <- 1;
    for (i in seq_along(child.ids)) {
        child.id <- child.ids[i];
        n.leaves <- v$leaves[v$id == child.id];
        j <- idx + (n.leaves %/% 2);
        if (n.leaves %% 2 == 0) { # if even
            current.pos <- x.pos[j] - 0.5
        } else {
            current.pos <- x.pos[j]
            }

        angle <- angles[tree$tip == child.id];
        if (is.na(angle)) {
            y <- tree$length[tree$tip == child.id];
            pair.spread <- v$spread[v$id %in% child.ids[c(i - 1, i)]];
            angle <- atan((current.pos * dx.scale * mean(pair.spread)) / y);
            angles[tree$tip == child.id] <- angle;
            }
        idx <- idx + n.leaves;
        }
    return(angles);
    }

calculate.angles.fixed <- function(
    v,
    tree,
    fixed.angle,
    start.angle
    ) {
    angles <- v$angle;
    node.ids <- c(v$id[[1]]);

    while (length(node.ids) > 0) {
        # "Pops" next element in FIFO queue node.ids
        current.node.id <- as.numeric(node.ids[1]);
        node.ids <- node.ids[-1];

        parent.angle <- angles[current.node.id];
        if (is.na(parent.angle) || length(parent.angle) == 0) {
            parent.angle <- start.angle;
            angles[current.node.id] <- parent.angle;
            }

        child.ids <- as.numeric(
            tree$tip[tree$parent == current.node.id & !is.na(tree$parent)]
            );
        num.children <- length(child.ids);
        if (num.children > 0) {
            # Safe to hardcode temporarily. This will only ever apply to
            # cases with 0, 1, or 2 children. 3+ will use radial calculation.

            # In future, I would like to remove this fixed angle calculation entirely.
            # It would be ideal to handle all calculations in the same way, and
            # rely more on user defined spread and explicit angle overrides.
            level.spread <- mean(v$spread[v$id %in% child.ids]);
            child.angles <- (if (num.children == 1) c(0) else c(-1, 1)) * fixed.angle * level.spread;
            child.angles <- child.angles + parent.angle;

            for (i in seq_along(child.ids)) {
                child.id <- child.ids[i];

                if (is.na(angles[child.id])) {
                    angle <- child.angles[i];
                    angles[tree$tip == child.id] <- angle;
                    }
                }
            }

        # Appending to end of queue for breadth-first traversal
        node.ids <- append(node.ids, child.ids);
        }

    return(angles);
    }

override.angles <- function(tree, v, angles) {
    if (is.null(v$angle)) {
        v$angle <- NA;
        }

    angle.index <- as.list(v$angle);

    names(angle.index) <- v$id;
    angle.index <- angle.index[!is.na(angle.index)];

    angles <- apply(
        data.frame(tip = tree$tip, angle = angles),
        MARGIN = 1,
        FUN = function(x) {
            node.id <- as.character(x['tip']);
            angle.override <- angle.index[[node.id]];
            if (!is.null(angle.override)) {
                angle.override <- as.numeric(angle.override);
                }
            angle <- if (is.null(angle.override) || is.na(angle.override)) x['angle'] else angle.override;
            return(as.numeric(angle));
            }
        );

    return(angles);
    }

calculate.level.spread <- function(level.spread.values) {
    n <- length(level.spread.values);
    if (n <= 1) {
        return(0);
        }

    level.spread <- sum(
        level.spread.values[1] * 0.5,
        level.spread.values[-c(1, n)],
        level.spread.values[n] * 0.5
        );
    return(level.spread / (n - 1));
    }
