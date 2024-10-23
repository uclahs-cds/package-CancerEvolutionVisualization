## Function: calculate.density.each.clone  ---------------------------------------------------------
calculate.density.each.clone <- function(cluster.df, cloneID) {
    # Skip clusters with only one SNV
    if (nrow(cluster.df) <= 1) {
        return(NULL);
        }
    density <- density(
        x = cluster.df$CCF,
        bw = 'nrd',
        adjust = 1.2,
        na.rm = TRUE
        );
    density.df <- as.data.frame(density[c('x','y')]);
    density.df$count <- nrow(cluster.df) / sum(density.df$y) * density.df$y;
    density.df$clone.id <- cloneID;
    return(density.df)
    }
