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

data.frame.to.array <- function(
    DF,
    value = 'CCF',
    x.axis = 'snv.id',
    y.axis = 'ID'
    ) {

    if (is.null(DF[[x.axis]]) | is.null(DF[[value]]) | is.null(DF[[y.axis]])) {
        stop(paste('Dataframe does not contain one of the columns:', value, x.axis, y.axis));
        }
    arr <- reshape(
        data = DF[, c(x.axis, y.axis, value)],
        v.names = value,
        timevar = y.axis,
        idvar = x.axis,
        direction = 'wide'
        );

    # set x.axis as rownames
    rows            <- arr[, 1];
    cols            <- gsub(paste0(value, '.'), '', names(arr)[-1]);
    arr             <- as.matrix(arr[, (-1)]);
    rownames(arr)   <- rows;
    colnames(arr)   <- cols;
    arr[is.na(arr)] <- 0;

    if (!is.null(levels(DF[, y.axis])) & ncol(arr) > 1) {
        arr <- arr[, levels(DF[, y.axis])];
        }

    return(arr);
    }

degrees.to.radians <- function(degrees) {
    return(degrees * pi / 180);
    }
