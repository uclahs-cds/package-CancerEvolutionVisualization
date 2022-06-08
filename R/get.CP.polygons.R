get.CP.polygons <- function(clone.out, ...) {
    polygon.list <- list();

    for (j in 1:length(clone.out$clones)) {
        polygon.list[[j]] <- polygonGrob(
            name = paste0('clone.polygon.', j),
            x = clone.out$clones[[j]]$x,
            y = clone.out$clones[[j]]$y,
            default.units = 'native',
            gp = gpar(
                fill = clone.out$clones[[j]]$col,
                col = 'transparent',
                alpha = clone.out$clones[[j]]$alpha
                )
            );
        }

    clone.out$grobs <- c(clone.out$grobs, polygon.list);
    }
