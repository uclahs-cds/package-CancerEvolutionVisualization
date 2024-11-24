calculate.density <- function(
    x,
    value = 'genome.pos',
    group = 'clone.id',
    scale = TRUE,
    ...
    ) {

    if (nrow(x) <= 1) {
        return(NULL);
        }
    density <- density(x = x[[value]], bw = 'nrd', na.rm = TRUE, ...);
    density.df <- as.data.frame(density[c('x', 'y')]);
    density.df$clone.id <- unique(x[[group]]);
    if (scale) {
        density.df$y <- nrow(x) / sum(density.df$y) * density.df$y;
        }
    return(density.df)
    }