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

radians.to.degrees <- function(radians) {
    return(radians * 180 / pi);
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

generate.random.color <- function() {
    rgb(runif(1), runif(1), runif(1));
    }
