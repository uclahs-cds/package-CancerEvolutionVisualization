prep.tree <- function(
    tree.df,
    text.df,
    bells = TRUE,
    colour.scheme,
    use.radians = FALSE,
    default.node.colour = 'grey29'
    ) {

    if (!('parent' %in% colnames(tree.df))) {
        stop('No parent column provided');
        }

    # Error on invalid tree structure
    get.root.node(tree.df);

    branch.names <- gsub(
        "length\\.?", "",
        get.branch.length.colnames(colnames(tree.df))
        );
    # Limit to 2 branches. This will
    if (length(branch.names) > 2) {
        warning(paste(
            'Only 2 branches are supported in the current version.',
            shQuote(branch.names[1], type = 'cmd'), 'and',
            shQuote(branch.names[2], type = 'cmd'), 'will be used.'
            ));
        branch.names <- branch.names[1:2];
        }

    if ('angle' %in% colnames(tree.df)) {
        tree.df$angle <- as.numeric(tree.df$angle);
        if (!use.radians) {
            tree.df$angle <- degrees.to.radians(tree.df$angle);
            }
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

    if (!check.circular.node.parents(tree.df)) {
        stop(paste(
            'Circular node reference.',
            'A node cannot be the parent of its own parent.'
            ));
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

    tree.df <- prep.edge.colours(tree.df);

    default.edge.type <- 'solid';
    for (branch in branch.names) {
        edge.type.column <- colnames(tree.df)[grepl(paste0('^edge\\.type\\.?', branch), colnames(tree.df))];
        if (length(edge.type.column) == 1) {
            tree.df[is.na(tree.df[, edge.type.column]), edge.type.column] <- default.edge.type;
        } else {
            tree.df[, paste0('edge.type.', branch)] <- default.edge.type;
            }
        }

    default.edge.width <- 3;
    if ('edge.width.1' %in% colnames(tree.df)) {
        tree.df$edge.width.1[is.na(tree.df$edge.width.1)] <- default.edge.width;
    } else {
        tree.df$edge.width.1 <- default.edge.width;
        }

    if ('edge.width.2' %in% colnames(tree.df)) {
        tree.df$edge.width.2[is.na(tree.df$edge.width.2)] <- default.edge.width;
    } else {
        tree.df$edge.width.2 <- default.edge.width;
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

    tree.df <- prep.branch.mode(tree.df);

    if ('node.col' %in% colnames(tree.df)) {
        tree.df$node.col[is.na(tree.df$node.col)] <- default.node.colour;
    } else {
        tree.df$node.col <- default.node.colour;
        }

    tree.df$node.label.col <- prep.node.label.colours(tree.df);

    tree.df$border.col <- apply(
        tree.df,
        MARGIN = 1,
        FUN = function(row) {
            if (is.na(row['border.col'])) row['node.col'] else row['border.col'];
        }
    );

    if ('border.type' %in% colnames(tree.df)) {
        valid.border.types <- c(
            'blank',
            'solid',
            'dashed',
            'dotted',
            'dotdash',
            'longdash',
            'twodash'
            );

        border.type.is.valid <- tree.df$border.type %in% valid.border.types | is.na(tree.df$border.type);

        if (!all(border.type.is.valid)) {
            stop(paste(
                'Invalid border type specified.',
                'Must be one of', paste(c(valid.border.types, 'or NA.'), collapse = ', ')
                ));
            }

        tree.df$border.type[is.na(tree.df$border.type)] <- if (is.numeric(tree.df$border.type)) 1 else 'solid';
    } else {
        tree.df$border.type <- 'solid';
        }

    if ('border.width' %in% colnames(tree.df)) {
        tree.df$border.width <- as.numeric(tree.df$border.width);
        tree.df$border.width[is.na(tree.df$border.width)] <- 1;
    } else {
        tree.df$border.width <- 1;
        }

    out.df <- data.frame(
        id = c(-1, tree.df$child),
        label.text = c('', tree.df$label),
        ccf = if (is.null(tree.df$CP)) NA else c(1, tree.df$CP),
        color = colour.scheme[1:(nrow(tree.df) + 1)],
        angle = c(NA, tree.df$angle),
        node.colour = c(NA, tree.df$node.col),
        node.label.colour = c(NA, tree.df$node.label.col),
        border.colour = c(NA, tree.df$border.col),
        border.type = c(NA, tree.df$border.type),
        border.width = c(NA, tree.df$border.width),
        parent = as.numeric(c(NA,tree.df$parent)),
        excluded = c(TRUE, rep(FALSE, nrow(tree.df))),
        edge.colour.1 = c(NA, tree.df$edge.col.1),
        edge.colour.2 = c(NA, tree.df$edge.col.2),
        edge.type.1 = c(NA, tree.df$edge.type.1),
        edge.type.2 = c(NA, tree.df$edge.type.2),
        edge.width.1 = c(NA, tree.df$edge.width.1),
        edge.width.2 = c(NA, tree.df$edge.width.2),
        connector.col = c(NA, tree.df$connector.col),
        connector.width = c(NA, tree.df$connector.width),
        connector.type = c(NA, tree.df$connector.type),
        mode = c(NA, tree.df$mode),
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

check.circular.node.parents <- function(tree) {
    has.circular.ref <- all(sapply(
        row.names(tree),
        function(node.name) {
            !is.circular.node.parent(tree, node.name);
            }
        ));

    return(has.circular.ref)
    }

is.circular.node.parent <- function(tree, node) {
    node.parent <- tree[node, 'parent'];
    parent.parent <- tree[node.parent, 'parent'];

    is.root <- function(node.name) {
        is.na(node.name) || node.name == '-1';
        }
    contains.root.node <- (is.root(node.parent)) || is.root(parent.parent);

    is.circular <- !contains.root.node && parent.parent == node;

    return(is.circular)
    }

get.root.node <- function(tree) {
    valid.values <- as.character(c(-1, 0));
    candidates <- which(is.na(tree$parent) | tree$parent %in% valid.values);

    if (length(candidates) > 1) {
        stop('More than one root node detected.');
    } else if (length(candidates) == 0) {
        stop('No root node provided.');
        }

    return(candidates);
    }

get.y.axis.position <- function(tree.colnames) {
    num.branch.length.cols <- length(get.branch.length.colnames(tree.colnames));

    y.axis.position <- if (num.branch.length.cols == 1) 'left' else {
        if (num.branch.length.cols > 1) 'both' else 'none';
        };

    return(y.axis.position);
    }

prep.branch.mode <- function(tree.df) {
    radial.mode.name <- 'radial';
    dendrogram.mode.name <- 'dendrogram';

    if ('mode' %in% colnames(tree.df)) {
        invalid.mode <- !(tree.df$mode %in% c(radial.mode.name, dendrogram.mode.name));
        if (any(invalid.mode)) {
            warning(paste(
                '"mode" must be one of "radial" or "dendrogram".',
                '"radial" will be used where mode is invalid.'
                ));
            }
        tree.df$mode[invalid.mode] <- NA;
    } else {
        tree.df$mode <- NA;
        }

    tree.df$mode[is.na(tree.df$mode)] <- radial.mode.name;
    return(tree.df);
    }

prep.edge.colours <- function(tree.df) {
    edge.colours <- list();

    default.edge.colours <- c('black', 'green');
    edge.colour.column.names <- sapply(
        1:2,
        function(i) paste('edge', 'col', i, sep = '.')
        );

    for (i in 1:length(edge.colour.column.names)) {
        column.name <- edge.colour.column.names[i];
        default.colour <- default.edge.colours[i];

        if (column.name %in% colnames(tree.df)) {
            tree.df[is.na(tree.df[, column.name]), column.name] <- default.colour;
        } else {
            tree.df[, column.name] <- default.colour;
            }
        }

    return(tree.df);
    }

prep.edge.colour.column <- function(tree.df, column.name, default.value) {
    if (column.name %in% colnames(tree.df)) {
        values <- tree.df[, column.name];
        values[is.na(values)] <- default.value;
        return(values);
    } else {
        return(rep(default.value, nrow(tree.df)));
        }
    }

prep.node.label.colours <- function(tree.df) {
    node.col.error.message <- 'Cannot prepare node label colour without node colour values.';

    if (!'node.col' %in% colnames(tree.df)) {
        stop(paste(
            node.col.error.message,
            '"node.col" column not found in tree.df'
            ));
    } else if (any(is.na(tree.df$node.col))) {
        stop(paste(
            node.col.error.message,
            'NA values found in tree.df "node.col" column.'
            ));
        }

    label.colours <- if (!'node.label.col' %in% colnames(tree.df)) {
        rep(NA, nrow(tree.df));
    } else {
        tree.df$node.label.col;
        }

    NA.indices <- is.na(label.colours);
    label.colours[NA.indices] <- as.character(sapply(
        tree.df$node.col[NA.indices],
        FUN = get.default.node.label.colour
        ));

    return(label.colours);
    }

prep.connector.line.width <- function(
    connector.line.width,
    branch.line.width,
    default.line.width
    ) {
    check.lengths(
        connector.line.width,
        branch.line.width,
        a.name = "connector.line.width",
        b.name = "branch.line.width",
        );
    NA.indices <- is.na(branch.line.width)
    connector.line.width[NA.indices] <- branch.line.width[NA.indices];

    connector.line.width <- as.numeric(connector.line.width);
    non.numeric.indices <- is.na(connector.line.width);
    if (any(non.numeric.indices)) {
        warning(
            non.numeric.warning.message("connector.width", "branch line width values")
            );
        connector.line.width[non.numeric.indices] <- branch.line.width[non.numeric.indices];
        }
    return(connector.line.width);
    }

prep.branch.line.width <- function(
    branch.line.width,
    default.line.width,
    edge.name
    ) {
    branch.line.width[is.na(branch.line.width)] <- default.line.width;
    branch.line.width <- is.numeric(branch.line.width);

    non.numeric.indices <- is.na(branch.line.width);
    if (any(non.numeric.indices)) {
        warning(non.numeric.warning.message(
            argument.name = edge.name,
            default.value = default.line.width
            ));
        branch.line.width[non.numeric.indices] <- default.line.width;
        }
    return(branch.line.width);
    }

get.default.node.label.colour <- function(node.colour) {
    white.luminance <- get.colour.luminance('black');
    node.colour.luminance <- get.colour.luminance(node.colour);

    contrast.ratio <- get.contrast.ratio(white.luminance, node.colour.luminance);

    # WCAG minimum contrast for normal/small text
    # https://www.w3.org/TR/2008/REC-WCAG20-20081211/#visual-audio-contrast-contrast
    WCAG.contrast.threshold <- 7;
    return(if (contrast.ratio < WCAG.contrast.threshold) 'white' else 'black');
    }
