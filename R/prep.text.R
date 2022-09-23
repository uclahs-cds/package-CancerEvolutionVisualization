filter.null.text <- function(text.df) {
    null.text <- which(is.na(text.df$node));

    if (length(null.text) > 0) {
        warning('Text with no node will not be used');

        return(text.df[-(null.text), ]);
    } else {
        return(text.df);
        }
    }

filter.invalid.text.nodes <- function(text.df, node.ids) {
    invalid.text <- which(as.logical(sapply(
        text.df$node,
        FUN = function(node) {
            !(node %in% node.ids);
            }
        )));

    if (length(invalid.text) > 0) {
        warning(paste(
            'Text nodes provided that do not match a tree node ID.',
            'Invalid text will not be used.'
            ));

        return(text.df[-(invalid.text), ]);
    } else {
        return(text.df);
        }
    }

prep.text <- function(text.df, tree.rownames) {
    text.df <- filter.null.text(text.df);

    text.df <- filter.invalid.text.nodes(
        text.df,
        tree.rownames
        );

    text.df <- add.default.text.columns(text.df);

    return(text.df);
    }

add.default.text.columns <- function(text.df) {
    # Internal functions rely on all columns being present
    if (is.null(text.df$col)) {
        text.df$col <- NA;
        }

    if (is.null(text.df$fontface)) {
        text.df$fontface <- NA;
        }

    return(text.df);
    }

prep.text.line.dist <- function(text.line.dist) {
    clamped <- FALSE;

    if (text.line.dist < 0) {
        text.line.dist <- 0;
        clamped <- TRUE;
    } else if (text.line.dist > 1) {
        text.line.dist <- 1;
        clamped <- TRUE;
        }

    if (clamped) {
        warning(paste(
            '"text.line.dist" must be between 0 and 1.',
            paste('A value of', text.line.dist, 'will be used')
            ));
        }

    return(text.line.dist);
    }
