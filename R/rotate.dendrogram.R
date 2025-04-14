rotate.dendrogram <- function(df, ...) {
    # Rotate base coordinates
    base.rot <- rotate.coords(df$basex, df$basey, ...);

    # Rotate tip coordinates
    tip.rot <- rotate.coords(df$tipx, df$tipy, ...);

    # Combine into a new data.frame with same column names
    df.out <- data.frame(
        basex = base.rot$x,
        basey = base.rot$y,
        tipx = tip.rot$x,
        tipy = tip.rot$y
        );

    return(df.out);
    }

rotate.coords <- function(
    x,
    y,
    rotate.by,
    x.origin = 0,
    y.origin = 0
    ) {

    # Translate points so the center is at the origin
    x.shifted <- x - x.origin
    y.shifted <- y - y.origin

    # Rotate points
    x.rotated <- x.shifted * cos(rotate.by) - y.shifted * sin(rotate.by)
    y.rotated <- x.shifted * sin(rotate.by) + y.shifted * cos(rotate.by)

    # Translate points back
    x.new <- x.rotated + x.origin
    y.new <- y.rotated + y.origin

    return(data.frame(x = x.new, y = y.new))
    }