get.colours <- function(
    value.list,
    return.names = FALSE
    ) {
    colours <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)];
    n <- length(unique(value.list));

    col.list <- sample(colours, n);
    if (is.null(levels(value.list))) {
        value.list <- factor(value.list, levels = unique(value.list))
        }
    names(col.list) <- levels(value.list);
    if (return.names) {
        return(col.list);
    } else {
        return(col.list[value.list]);
        }
    }