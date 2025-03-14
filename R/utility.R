get.value.index <- function(old.values, new.values) {
    if (length(old.values) != length(new.values)) {
        stop(paste(
            'New value length is not compatible with old values.',
            'Value index cannot be created.'
            ));
        }

    value.index <- as.list(new.values);
    names(value.index) <- old.values;

    return(value.index);
    }

reindex.column <- function(column.values, new.value.index) {
    return(unlist(
        new.value.index[as.character(column.values)],
        use.names = FALSE
        ));
    }

validate.data.frame.columns <- function(x, expected.columns) {
    missing.columns <- !(expected.columns %in% colnames(x));

    if (any(missing.columns)) {
        stop(paste(
            'Columns missing from input data:',
            paste(expected.columns[which(missing.columns)], collapse = ', ')
            ));
        }
    }

degrees.to.radians <- function(degrees) {
    return(degrees * pi / 180);
    }

get.encoded.distance <- function(points) {
    if (!is.data.frame(points)) {
        stop(paste(
            'Input data \'points\' must be a data.frame',
            paste0('(received \'', class(points), '\').')
            ));
        }
    validate.data.frame.columns(points, expected.columns = c('x', 'y'));

    # Line calculations will not be valid with a single point
    if (nrow(points) < 2) {
        points$encoded.distance <- 0;
        return(points);
        }

    coords <- as.matrix(points);
    line.direction <- matrix(c(1, 1));

    reference.point <- coords[1, ];

    point.vectors <- coords - reference.point;
    dot.products <- point.vectors %*% line.direction;
    projections <- dot.products %*% t(line.direction);
    distances.along.line <- sqrt(rowSums((point.vectors - projections)^2));
    encoded.distances <- ifelse(dot.products >= 0, distances.along.line, -distances.along.line);

    return(encoded.distances);
    }

#' Generate a named vector of colors for every clone ID specified.
#'
#' Assigns colors to clones, ensuring a minimum number of colors are used.
#' Any colors specified in `clone.colours` are maintained in the order
#' specified and are used as the first colors for the `clone.ids`.
#'
#' if `clone.colours` is NULL or an empty vector, _and_
#' `minimum.number.of.colours` is 0, `NULL` is returned
#' so BPG's default color selection can be used without needing to
#' check for `NULL`.
#'
#' @param clone.colours A vector of colors assigned to clones. If `NULL`, colors will be generated automatically.
#' @param clone.ids A vector of clone identifiers.
#' @param minimum.number.of.colours An integer specifying the minimum number of colors required.
#'
#' @return A named vector of colors assigned to each clone.
get.clone.colours <- function(clone.colours, clone.ids, minimum.number.of.colours = 0) {
    if (is.null(clone.colours) && minimum.number.of.colours == 0) {
        return(NULL);
        }

    if (length(clone.colours) < minimum.number.of.colours) {
        clone.colours <- c(
            clone.colours,
            sample(
                colors(),
                size = abs(minimum.number.of.colours - length(clone.colours))
                )
            );
        }

    if (!is.null(clone.colours) && !is.null(clone.ids)) {
        unique.clone.ids <- unique(clone.ids);
        sampled.colors <- sample(colors(), size = length(unique.clone.ids));
        sampled.colors[seq_along(clone.colours)] <- clone.colours;
        return(setNames(
            sampled.colors[seq_along(unique.clone.ids)],
            unique.clone.ids
            ));
        }

    return(NULL);
    }

#' Generate a named vector of colors for every clone ID specified,
#' ordered by the clone IDs in `clone.order`.
#'
#' Assigns colors to clones and ensures they follow a specified order.
#' Any colors specified in `clone.colours` are maintained in the order
#' specified and are used as the first colors for the `clone.ids`.
#'
#' if `clone.colours` is NULL or an empty vector, _and_
#' `minimum.number.of.colours` is 0, `NULL` is returned
#' so BPG's default color selection can be used without needing to
#' check for `NULL`.
#'
#' @param clone.colours A vector of colors assigned to clones. If `NULL`, colors will be generated automatically.
#' @param clone.ids A vector of clone identifiers.
#' @param clone.order An optional vector specifying the order of clones. If `NULL`, clone order is not gauranteed.
#'
#' @return A list containing:
#' \describe{
#'   \item{clone.colours}{A named vector of colors assigned to each clone.}
#'   \item{clone.order}{The ordered clones.}
#' }
get.clone.colours.in.order <- function(
    clone.colours,
    clone.ids,
    clone.order = NULL
    ) {

    if (is.null(clone.colours) && is.null(clone.order)) {
        clone.ids <- NULL;
        }

    if (is.null(clone.order) && !is.null(clone.ids)) {
        clone.order <- unique(clone.ids);
        }

    if (is.null(clone.colours) || is.null(clone.order)) {
        clone.colours <- NULL;
    } else {
        sampled.colours <- sample(colors(), size = length(clone.order));
        sampled.colours[seq_along(clone.colours)] <- clone.colours;
        clone.colours <- setNames(
            sampled.colours[seq_along(clone.order)],
            clone.order
            );
        }

    return(list(
        clone.colours = clone.colours,
        clone.order = clone.order
        ));
    }
