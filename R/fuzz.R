randomize.tree <- function(
    tree.df,
    randomize.angles = TRUE,
    randomize.node.color = TRUE,
    randomize.border.color = TRUE,
    randomize.border.width = TRUE,
    randomize.border.type = TRUE,
    randomize.plotting.direction = TRUE,
    plotting.direction = NULL,
    ...
    ) {
    node.ids <- c(get.root.node(tree.df));

    spread.randomization.sd <- 0.5;
    if (randomize.angles) {
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

    angle.randomization.sd <- 30;
    if (randomize.plotting.direction) {
        if (is.null(plotting.direction)) {
            plotting.direction <- sample(c('down', 'right', 'left', 'up'), size = 1);
            }
        plotting.direction <- radians.to.degrees(
            prep.plotting.direction(plotting.direction, radians = FALSE)
            );
        plotting.direction <- plotting.direction + rnorm(sd = angle.randomization.sd, n = 1);
        }

    if (randomize.node.color) {
        node.color.randomization.prob <- 0.5;
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

    if (randomize.border.color) {
        border.color.randomization.prob <- 0.3;
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

    if (randomize.border.width) {
        border.width.randomization.sd <- 1;
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

    if (randomize.border.type) {
        border.type.randomization.prob <- 0.3;
        default.border.type <- 'solid';

        if (!('border.type' %in% colnames(tree.df))) {
            tree.df$border.type <- default.border.type;
        } else {
            tree.df[is.na(tree.df$border.type), 'border.type'] <- default.border.type;
            }
        override.border.type.i <- sapply(
            1:nrow(tree.df),
            function(i) runif(1) <= border.type.randomization.prob
            );
        tree.df[override.border.type.i, 'border.type'] <- sample(
            c('solid', 'dashed', 'dotted'),
            size = sum(override.border.type.i),
            replace = TRUE
            );
        }

    result <- SRCGrob(
        tree.df,
        plotting.direction = plotting.direction,
        ...
        );
    return(result);
    }
