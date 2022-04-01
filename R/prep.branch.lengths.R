
extract.length.colnames <- function(column.names) {
    # Temporarily limit number of parallel branches
    max.branches <- 2;

    length.cols <- head(
        grep('length', column.names),
        max.branches
        );

    if (length(length.cols) > max.branches) {
        warning(paste(
                'Only the first 2 "length" columns will be used.',
                'More branch lengths will be supported in a future version.'
            ));
        }

    return(length.cols);
    }

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

prep.branch.lengths <- function(tree.df) {
    length.cols <- extract.length.colnames(colnames(tree.df));

    if (length(length.cols) > 0) {
        lengths.df <- data.frame(tree.df[, length.cols]);
        colnames(lengths.df) <- get.branch.length.colnames(length(length.cols));

        return(lengths.df);
    } else {
        return(get.default.branch.lengths(nrow(tree.df)));
        }
    }
