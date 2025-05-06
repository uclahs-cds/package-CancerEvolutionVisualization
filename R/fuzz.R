randomize.tree <- function(
    tree.df,
    randomize.angles = TRUE,
    randomize.node.color = TRUE,
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

    result <- SRCGrob(
        tree.df,
        plotting.direction = plotting.direction,
        ...
        );
    return(result);
    }
