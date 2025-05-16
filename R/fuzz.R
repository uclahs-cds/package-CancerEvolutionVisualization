randomize.tree <- function(
    tree.df,
    randomize.angles = TRUE,
    randomize.node.color = TRUE,
    randomize.border.color = TRUE,
    randomize.border.width = TRUE,
    randomize.border.type = TRUE,
    randomize.edge.col = TRUE,
    randomize.edge.width = TRUE,
    randomize.edge.type = TRUE,
    randomize.edge.length = TRUE,
    randomize.plotting.direction = TRUE,
    plotting.direction = NULL,
    ...
    ) {
    node.ids <- c(get.root.node(tree.df));

    default.line.type <- 'solid';
    line.types <- c(default.line.type, 'dotted', 'dashed');

    if (check.randomization.value(randomize.angles)) {
        spread.randomization.sd <- if (is.numeric(randomize.angles)) {
            if (randomize.angles <= 0) {
                stop('"randomize.angles" standard deviation value must be positive.');
                }
            randomize.angles;
        } else {
            0.5;
            }
        default.spread <- 1;
        if (!('spread' %in% colnames(tree.df))) {
            tree.df$spread <- default.spread;
        } else {
            tree.df[is.na(tree.df$spread), 'spread'] <- 1;
            }
        tree.df$spread <- tree.df$spread + rnorm(
            mean = 0,
            sd = spread.randomization.sd, 
            n = nrow(tree.df)
            );
        tree.df[tree.df$spread < 0, 'spread'] <- 0;
        }

    if (check.randomization.value(randomize.plotting.direction)) {
        if (is.null(plotting.direction)) {
            plotting.direction <- sample(c('down', 'right', 'left', 'up'), size = 1);
            }
        plotting.direction <- radians.to.degrees(
            prep.plotting.direction(plotting.direction, radians = FALSE)
            );
        angle.randomization.sd <- if (is.numeric(randomize.plotting.direction)) {
            if (randomize.plotting.direction <= 0) {
                stop('"randomize.plotting.direction" standard deviation value must be positive.');
                }
            randomize.plotting.direction;
        } else {
            30
            };
        plotting.direction <- plotting.direction + rnorm(sd = angle.randomization.sd, n = 1);
        }

    if (check.randomization.value(randomize.node.color)) {
        node.color.randomization.prob <- if (is.numeric(randomize.node.color)) {
            if (randomize.node.color < 0 || randomize.node.color > 1) {
                stop('"randomize.node.color" probability must be between 0 and 1.')
                }
            randomize.node.color;
        } else {
                0.5;
            }
        node.color.scheme <- if (runif(1) <= node.color.randomization.prob) {
            generate.random.color();
        } else {
            NA;
            }

        if (!('node.col' %in% colnames(tree.df))) {
            tree.df$node.col <- node.color.scheme;
        } else {
            tree.df[is.na(tree.df$node.col), 'node.col'] <- node.color.scheme;
            }
        override.node.col.i <- sapply(
            1:nrow(tree.df),
            function(i) runif(1) <= node.color.randomization.prob
            );
        tree.df[override.node.col.i, 'node.col'] <- sapply(
            1:sum(override.node.col.i),
            function(i) generate.random.color()
            );
        }

    if (check.randomization.value(randomize.border.color)) {
        border.color.randomization.prob <- if (is.numeric(randomize.border.color)) {
            if (randomize.border.color < 0 || randomize.border.color > 1) {
                stop('"randomize.border.color" probability must be between 0 and 1.')
            }
            randomize.border.color;
        } else {
            0.3;
            }
        border.color.scheme <- if (runif(1) <= border.color.randomization.prob) {
            generate.random.color();
        } else {
            NA;
            }

        if (!('border.col' %in% colnames(tree.df))) {
            tree.df$border.col <- border.color.scheme;
        } else {
            tree.df[is.na(tree.df$border.col), 'border.col'] <- node.color.scheme;
            }
        override.border.col.i <- sapply(
            1:nrow(tree.df),
            function(i) runif(1) <= border.color.randomization.prob
            );
        tree.df[override.border.col.i, 'border.col'] <- sapply(
            1:sum(override.border.col.i),
            function(i) generate.random.color()
            );
        }

    if (check.randomization.value(randomize.border.width)) {
        border.width.randomization.sd <- if (is.numeric(randomize.border.width)) {
            if (randomize.border.width <= 0) {
                stop('"randomize.border.width" standard deviation value must be positive.');
            }
            randomize.border.width;
        } else {
            1
            };
        default.border.width <- 1;

        if (!('border.width' %in% colnames(tree.df))) {
            tree.df$border.width <- default.border.width;
        } else {
            tree.df[is.na(tree.df$border.width), 'border.width'] <- default.border.width;
            }
        tree.df[, 'border.width'] <- tree.df$border.width + rnorm(
            mean = 0,
            sd = border.width.randomization.sd,
            n = nrow(tree.df)
            );
        tree.df[tree.df$border.width <= 0, 'border.width'] <- 0;
        }

    if (check.randomization.value(randomize.border.type)) {
        default.border.type <- sample(line.types, size = 1);

        border.type.randomization.prob <- if (is.numeric(randomize.border.type)) {
            if (randomize.border.type < 0 || randomize.border.type > 1) {
                stop('"randomize.border.type" probability must be between 0 and 1.')
            }
            randomize.border.type;
        } else {
            0.3;
            }

        if (!('border.type' %in% colnames(tree.df))) {
            tree.df$border.type <- default.border.type;
        } else {
            tree.df[is.na(tree.df$border.type), 'border.type'] <- default.border.type;
            }
        override.border.type.i <- runif(nrow(tree.df)) <= border.type.randomization.prob;
        tree.df[override.border.type.i, 'border.type'] <- sample(
            line.types,
            size = sum(override.border.type.i),
            replace = TRUE
            );
        }

    edge.names <- sort(get.branch.names(tree.df));
    if (length(edge.names) < 1) {
        edge.names <- 1;
        }

    edge.color.randomization.prob <- 0;
    if (check.randomization.value(randomize.edge.col)) {
        edge.color.randomization.prob <- if (is.numeric(randomize.edge.col)) {
            if (randomize.edge.col < 0 || randomize.edge.col > 1) {
                stop('"randomize.edge.col" probability must be between 0 and 1.')
            }
            randomize.edge.col;
        } else {
            0.3;
            }
        }

    edge.width.randomization.sd <- if (is.numeric(randomize.edge.width)) {
        if (randomize.edge.width <= 0) {
            stop('"randomize.edge.width" standard deviation value must be positive.');
        }
        randomize.edge.width;
    } else {
        1
        };

    edge.type.randomization.prob <- 0;
    if (check.randomization.value(randomize.edge.type)) {
        edge.type.randomization.prob <- if (is.numeric(randomize.edge.type)) {
            if (randomize.edge.type < 0 || randomize.edge.type > 1) {
                stop('"randomize.edge.type" probability must be between 0 and 1.')
            }
            randomize.edge.type;
        } else {
            0.3;
            }
        }

    for (edge.name in edge.names) {
        if (check.randomization.value(randomize.edge.col)) {
            edge.color.scheme <- generate.random.color();

            edge.col.column.name <- paste('edge.col', edge.name, sep = '.');
            if (!(edge.col.column.name %in% colnames(tree.df))) {
                tree.df[, edge.col.column.name] <- edge.color.scheme;
            } else {
                tree.df[is.na(tree.df[, edge.col.column.name]), edge.col.column.name] <- edge.color.scheme;
                }
            override.edge.col.i <- runif(n = nrow(tree.df), max = 1) <= edge.color.randomization.prob;
            tree.df[override.edge.col.i, edge.col.column.name] <- sapply(
                1:sum(override.edge.col.i),
                function(i) generate.random.color()
                );
            }

        if (check.randomization.value(randomize.edge.width)) {
            base.edge.width.randomization.prob <- 0.5;
            default.edge.width <- if (runif(1) <= base.edge.width.randomization.prob) {
                max(0, rnorm(1, mean = 3));
            } else {
                3;
                }

            edge.width.column.name <- paste('edge.width', edge.name, sep = '.');
            if (!(edge.width.column.name %in% colnames(tree.df))) {
                tree.df[, edge.width.column.name] <- default.edge.width;
            } else {
                tree.df[is.na(tree.df[, edge.width.column.name]), edge.col.column.name] <- default.edge.width;
                }
            tree.df[, edge.width.column.name] <- tree.df[, edge.width.column.name] + rnorm(
                sd = edge.width.randomization.sd,
                n = nrow(tree.df)
                );
            tree.df[, edge.width.column.name] <- sapply(
                tree.df[, edge.width.column.name],
                function(x) max(0, x)
                );
            }

        if (check.randomization.value(randomize.edge.type)) {
            default.edge.type <- sample(line.types, size = 1);

            edge.type.column.name <- paste('edge.type', edge.name, sep = '.');
            if (!(edge.type.column.name %in% colnames(tree.df))) {
                tree.df[, edge.type.column.name] <- default.edge.type;
            } else {
                tree.df[is.na(tree.df[, edge.type.column.name]), edge.col.column.name] <- default.edge.type;
                }
            override.edge.type.i <- runif(n = nrow(tree.df), max = 1) <= edge.type.randomization.prob;
            tree.df[override.edge.type.i, edge.type.column.name] <- sample(
                line.types,
                size = sum(override.edge.type.i),
                replace = TRUE
                );
            }

        if (randomize.edge.length) {
            edge.length.column.name <- paste('length', edge.name, sep = '.');
            base.edge.length <- 10 ** runif(n = 1, min = 0, max = 6);
            if (!(edge.length.column.name %in% colnames(tree.df))) {
                tree.df[, edge.length.column.name] <- base.edge.length;
            } else {
                tree.df[is.na(tree.df[, edge.length.column.name]), edge.length.column.name] <- base.edge.length;
                }
            edge.length.randomization.sd <- median(tree.df[, edge.length.column.name]) * 0.2;
            tree.df[, edge.length.column.name] <- tree.df[, edge.length.column.name] + rnorm(
                sd = edge.length.randomization.sd,
                n = nrow(tree.df)
                );
            tree.df[tree.df[, edge.length.column.name] < 0, edge.length.column.name]
            }
        }

    result <- SRCGrob(
        tree.df,
        plotting.direction = plotting.direction,
        ...
        );
    return(result);
    }

check.randomization.value <- function(
    randomization,
    default.value,
    randomization.name = NULL
    ) {
    if (is.null(randomization.name)) {
        randomization.name <- 'Randomization value';
        }
    if (length(randomization) != 1) {
        stop(paste(randomization.name, 'must be length 1.'));
        }

    randomize.result <- FALSE;
    if (is.numeric(randomization)) {
        randomize.result <- TRUE;
    } else if (is.logical(randomization)) {
        randomize.result <- randomization;
    } else {
        stop(paste(randomization.name, 'must be numeric or TRUE/FALSE.'));
        }
    return(randomize.result);
    }
