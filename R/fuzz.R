randomize.tree <- function(
    tree.df,
    randomize.angles = TRUE,
    randomize.plotting.direction = TRUE,
    plotting.direction = NULL,
    ...
    ) {
    node.ids <- c(get.root.node(tree.df));

    spread.randomization.sd <- 0.5;
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
