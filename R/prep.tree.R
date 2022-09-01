prep.tree <- function(
    tree.df,
    text.df,
    bells = TRUE,
    axis.type = 'left',
    colour.scheme
    ) {

    if (!('parent' %in% colnames(tree.df))) {
        stop('No parent column provided');
        }

    if ('angle' %in% colnames(tree.df)) {
        message(paste(
            'Overriding branch angles will be supported in a future version.',
            'The angle column will not be used.'
            ));
        }

    tree.df$parent <- prep.tree.parent(tree.df$parent);

    if (!check.parent.values(rownames(tree.df), tree.df$parent)) {
        stop('Parent column references invalid node');
        }

    if (!is.null(text.df)) {
        text.df <- prep.text(
            text.df,
            tree.rownames = rownames(tree.df)
            );
        }

    if (!is.null(tree.df$CP)) {
        tree.df$CP <- suppressWarnings(as.numeric(tree.df$CP));

        if (any(is.na(tree.df$CP))) {
            warning(paste(
                'Non-numeric values found in CP column.',
                'Cellular prevalence will not be used.'
                ));

            tree.df$CP <- NULL;
            }
        }

    tree.df <- reorder.nodes(tree.df);

    # Include -1 value for root node.
    # This may be temporary, as NULL/NA will likely replace -1
    node.id.index <- get.value.index(
        old.values = c(-1, rownames(tree.df)),
        new.values = c(-1, 1:nrow(tree.df))
        );

    tree.df <- reset.tree.node.ids(tree.df, node.id.index);
    tree.df$child <- rownames(tree.df);

    text.df$node <- reindex.column(text.df$node, node.id.index);

    tree.df$label <- as.character(
        if (is.null(tree.df$label)) tree.df$child else tree.df$label
        );

    out.df <- data.frame(
        id = c(-1, tree.df$child),
        label.text = c('', tree.df$label),
        ccf = if (is.null(tree.df$CP)) NA else c(1, tree.df$CP),
        color = colour.scheme[1:(nrow(tree.df) + 1)],
        parent = as.numeric(c(NA,tree.df$parent)),
        excluded = c(TRUE, rep(FALSE, nrow(tree.df))),
        bell = c(FALSE, rep(bells, nrow(tree.df))),
        alpha = rep(0.5, (nrow(tree.df) + 1)),
        stringsAsFactors = FALSE
        );

    out.df$tier <- get.num.tiers(out.df)

    out.tree <- data.frame(
        parent = as.numeric(tree.df$parent),
        tip = as.numeric(tree.df$child),
        prep.branch.lengths(tree.df)
        );

    branching <- any(duplicated(out.tree$parent));

    return(list(
        in.tree.df = out.df,
        tree = out.tree,
        text.df = text.df,
        branching = branching
        ));
    }

prep.tree.parent <- function(parent.column) {
    parent.column[parent.column %in% c(0, NA)] <- -1;
    return(parent.column);
    }

reorder.nodes <- function(tree.df) {
    if (any(!is.na(tree.df$CP))) {
        tree.df <- reorder.nodes.by.CP(tree.df);
        }

    return(reorder.trunk.node(tree.df));
    }

reorder.nodes.by.CP <- function(tree.df) {
    return(tree.df[order(-(tree.df$CP), tree.df$parent), ]);
    }

reorder.trunk.node <- function(tree.df) {
    is.trunk <- is.na(tree.df$parent) | tree.df$parent == -1;

    # Skip reindexing data.frame if trunk node is already first
    if (!is.trunk[[1]]) {
        tree.df[c(which(is.trunk), which(!is.trunk)), ];
    } else {
        tree.df;
        }
    }

reset.tree.node.ids <- function(tree.df, value.index) {
    rownames(tree.df) <- 1:nrow(tree.df);

    # Convert parent values to character to safely index names list
    tree.df$parent <- reindex.column(tree.df$parent, value.index);

    return(tree.df);
    }



check.parent.values <- function(node.names, parent.col) {
    unique.node.names <- as.list(setNames(
        !vector(length = length(unique(node.names))),
        unique(node.names)
        ));

    all(sapply(
        parent.col,
        FUN = function(parent) {
            !is.null(unlist(unique.node.names[parent])) | parent == -1;
            }
        ));
    }
