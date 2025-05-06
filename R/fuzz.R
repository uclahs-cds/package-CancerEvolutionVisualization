randomize.tree <- function(
    tree.df,
    randomize.angles = TRUE,
    randomize.plotting.direction = TRUE,
    plotting.direction = NULL,
    ...
    ) {
    node.ids <- c(get.root.node(tree.df));

    spread.randomization.sd <- 0.5;
    if (randomize.angles) {
        default.spread <- 1;
        if (is.null(tree.df$spread)) {
            tree.df$spread <- default.spread;
        } else {
            tree.df[is.na(tree.df$spread), 'spread'] <- 1;
            }
        tree.df$spread <- tree.df$spread + rnorm(
            mean = 0,
            sd = spread.randomization.sd, 
            n = nrow(tree.df)
            );
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

    result <- SRCGrob(
        tree.df,
        plotting.direction = plotting.direction,
        ...
        );
    return(result);
    }
