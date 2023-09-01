get.default.branch.length.colnames <- function(num.columns) {
        if (num.columns > 0) {
            sapply(
                1:num.columns,
                FUN = function(i) {
                    paste0('length', i);
                    }
            );
        } else {
            vector();
            }
    }

get.default.branch.lengths <- function(num.rows, num.cols) {
    lengths <- data.frame(a = rep(num.cols, times = num.rows));
    colnames(lengths) <- get.default.branch.length.colnames(num.cols);

    return(lengths);
    }

validate.branch.colname <- function(column.name) {
    grepl('length', column.name);
    }

get.branch.length.colnames <- function(col.names) {
    Filter(validate.branch.colname, col.names);
    }

validate.branch.length.values <- function(length.column) {
    return(tryCatch({
        numeric.values <- as.numeric(length.column);

        return(
            # Catches dropped NULL values
            length(numeric.values) == length(length.column) &&
            all(!is.na(numeric.values))
            );
        },
        warning = function(cond) {
            return(FALSE);
            }
        ));
    }

# Temporarily limit number of parallel branches
limit.branch.length.columns <- function(column.names, max.cols = 2) {
    if (length(column.names) > max.cols) {
        message(paste(
                'Only the first 2 "length" columns will be used.',
                'More branch lengths will be supported in a future version.'
            ));
        }

    return(head(column.names, max.cols));
    }

prep.branch.lengths <- function(tree.df) {
    length.cols <- limit.branch.length.columns(
        Filter(
            function(column.name) {
                col.name.is.valid <- validate.branch.colname(column.name);
                values.are.valid <- NULL;

                if (col.name.is.valid) {
                    values.are.valid <- validate.branch.length.values(tree.df[, column.name]);

                    if (!values.are.valid) {
                        warning(paste(
                            'Branch length column', column.name, 'contains non-numeric values.',
                            'It will not be used.'
                            ));
                        }
                    }

                return(col.name.is.valid && values.are.valid);
                },
            colnames(tree.df)
            )
        );

    # TODO: Automatically create length2 if an edge.style.2 column is present.
    if (length(length.cols) > 1) {
        lengths.df <- data.frame(tree.df[, length.cols]);
        colnames(lengths.df) <- get.default.branch.length.colnames(length(length.cols));

        return(lengths.df);
    } else {
        return(get.default.branch.lengths(
            num.rows = nrow(tree.df),
            num.cols = 1
            ));
        }
    }
