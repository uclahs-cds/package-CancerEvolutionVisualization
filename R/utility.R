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
            "found in",
            if (!is.null(data.name)) data.name else "data"
            );

        stop(message);
        }
    }
