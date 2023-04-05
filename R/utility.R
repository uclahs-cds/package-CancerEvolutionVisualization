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

check.column.exists <- function(
    df,
    column.name,
    data.name = NULL
    ) {

    result <- column.name %in% colnames(df);

    if (!result) {
        message <- paste(
            'No column',
            paste0('"', column.name, '"'),
            'found in',
            if (!is.null(data.name)) data.name else 'data'
            );

        stop(message);
        }
    }

column.contains.all <- function(reference.column, checked.column) {
    vector.error.message <- function(column.type) {
        return(paste(column.type, 'must be a vector.'))
        }

    if (!is.vector(reference.column)) {
        stop(vector.error.message('Reference'));
    }

    if (!is.vector(checked.column)) {
        stop(vector.error.message('Checked'));
        }

    if (is.list(reference.column)) {
        reference.column <- unlist(reference.column);
        }

    reference.values <- sapply(
        reference.column,
        FUN = function(column.name) TRUE,
        USE.NAMES = TRUE
        );   

    values.in.reference <- all(sapply(
        checked.column,
        FUN = function(column.name) !is.na(reference.values[column.name])
        ));
    
    return(values.in.reference);
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
