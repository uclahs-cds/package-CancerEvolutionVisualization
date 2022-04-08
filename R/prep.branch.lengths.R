get.branch.length.colnames <- function(num.columns) {
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

get.default.branch.lengths <- function(num.rows) {
    lengths <- data.frame(a = rep(1, times = num.rows));
    colnames(lengths) <- get.branch.length.colnames(1);

    return(lengths);
    }

validate.branch.colname <- function(column.name) {
    grepl('length', column.name);
    }

validate.branch.length.values <- function(length.column) {
    all(!is.na(as.numeric(length.column)));
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
                validate.branch.colname(column.name) && 
                    validate.branch.length.values(tree.df[, column.name]) 
                },
            colnames(tree.df)
            )
        );

    if (length(length.cols) > 0) {
        lengths.df <- data.frame(tree.df[, length.cols]);
        colnames(lengths.df) <- get.branch.length.colnames(length(length.cols));

        return(lengths.df);
    } else {
        return(get.default.branch.lengths(nrow(tree.df)));
        }
    }
