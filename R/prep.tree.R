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

    branch.names <- sort(get.branch.names(tree.df));
    # Limit to 2 branches. This will
    if (length(branch.names) > 2) {
        warning(paste(
            'Only 2 branches are supported in the current version.',
            shQuote(branch.names[1], type = 'cmd'), 'and',
            shQuote(branch.names[2], type = 'cmd'), 'will be used.'
            ));
        branch.names <- branch.names[1:2];
    } else if (length(branch.names) == 0) {
        branch.names <- c('1');
        }

    if (!('angle' %in% colnames(tree.df))) {
        tree.df$angle <- NA;
        }
    tree.df$angle <- prep.column.values(
        tree.df$angle,
        default.values = NA,
        conversion.fun = as.numeric
        );
    if (!use.radians) {
        tree.df$angle <- degrees.to.radians(tree.df$angle);
        }

    if (!('spread' %in% colnames(tree.df))) {
        tree.df$spread <- NA;
        }
    tree.df$spread <- prep.column.values(
        tree.df$spread,
        default.values = NA,
        conversion.fun = as.numeric
        );
    tree.df <- prep.tree.spread(tree.df);

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

    tree.df <- prep.draw.node.setting(tree.df);
    tree.df <- prep.edge.colours(tree.df);

    for (i in 1:length(branch.names)) {
        branch <- branch.names[i];

        length.column <- colnames(tree.df)[grepl(paste0('^length\\.', branch), colnames(tree.df))];
        if (length(length.column) < 1) {
            length.column <- paste0('length.', branch);
            tree.df[, length.column] <- NA;
            }
        default.length <- 1;
        tree.df[, length.column] <- prep.column.values(
            tree.df[, length.column],
            default.values = default.length,
            conversion.fun = as.numeric
            );

        edge.type.column <- colnames(tree.df)[grepl(paste0('^edge\\.type\\.', branch), colnames(tree.df))];
        if (length(edge.type.column) < 1) {
            edge.type.column <- paste0('edge.type.', branch);
            tree.df[, edge.type.column] <- NA;
            }

        default.edge.type <- 'solid';
        tree.df[, edge.type.column] <- prep.column.values(
            tree.df[, edge.type.column],
            default.values = default.edge.type,
            conversion.fun = as.character
            );

        edge.width.column <- colnames(tree.df)[grepl(paste0('^edge\\.width\\.', branch), colnames(tree.df))];
        if (length(edge.width.column) < 1) {
            edge.width.column <- paste0('edge.width.', branch);
            tree.df[, edge.width.column] <- NA;
            }

        default.edge.width <- 3;
        tree.df[, edge.width.column] <- prep.column.values(
            tree.df[, edge.width.column],
            default.values = default.edge.width,
            conversion.fun = as.numeric
            );

        edge.col.column <- colnames(tree.df)[grepl(paste0('^edge\\.col\\.', branch), colnames(tree.df))];
        if (length(edge.col.column) < 1) {
            edge.col.column <- paste0('edge.col.', branch);
            tree.df[, edge.col.column] <- NA;
            }

        default.edge.col <- 'black';
        tree.df[, edge.col.column] <- prep.column.values(
            tree.df[, edge.col.column],
            default.values = default.edge.col,
            conversion.fun = as.character
            );
        }

    if (!('connector.type' %in% colnames(tree.df))) {
        tree.df$connector.type <- NA;
        }
    tree.df$connector.type <- prep.column.values(
        tree.df$connector.type,
        default.values = tree.df[, paste0('edge.type.', branch.names[1])],
        conversion.fun = as.character
        );

    if (!('connector.col' %in% colnames(tree.df))) {
        tree.df$connector.col <- NA;
        }
    tree.df$connector.col <- prep.column.values(
        tree.df$connector.col,
        default.values = tree.df[, paste0('edge.col.', branch.names[1])],
        conversion.fun = as.character
        );

    if (!('connector.width' %in% colnames(tree.df))) {
        tree.df$connector.width <- NA;
        }
    tree.df$connector.width <- prep.column.values(
        tree.df$connector.width,
        default.values = tree.df[, paste0('edge.width.', branch.names[1])],
        conversion.fun = as.character
        );

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

    tree.df <- prep.node.border.colours(tree.df);
    tree.df <- prep.node.border.type(tree.df);
    tree.df <- prep.node.border.width(tree.df);

    out.df <- data.frame(
        id = c(-1, tree.df$child),
        label.text = c('', tree.df$label),
        ccf = if (is.null(tree.df$CP)) NA else c(1, tree.df$CP),
        color = colour.scheme[1:(nrow(tree.df) + 1)],
        angle = c(NA, tree.df$angle),
        draw.node = c(NA, tree.df$draw.node),
        spread = c(NA, tree.df$spread),
        node.colour = c(NA, tree.df$node.col),
        node.label.colour = c(NA, tree.df$node.label.col),
        border.colour = c(NA, tree.df$border.col),
        border.type = c(NA, tree.df$border.type),
        border.width = c(NA, tree.df$border.width),
        parent = as.numeric(c(NA,tree.df$parent)),
        excluded = c(TRUE, rep(FALSE, nrow(tree.df))),
        x.length = c(NA, tree.df$x.length),
        edge.colour.1 = c(NA, tree.df[, paste0('edge.col.', branch.names[1])]),
        edge.type.1 = c(NA, tree.df[, paste0('edge.type.', branch.names[1])]),
        edge.width.1 = c(NA, tree.df[, paste0('edge.width.', branch.names[1])]),
        connector.col = c(NA, tree.df$connector.col),
        connector.width = c(NA, tree.df$connector.width),
        connector.type = c(NA, tree.df$connector.type),
        mode = c(NA, tree.df$mode),
        bell = c(FALSE, rep(bells, nrow(tree.df))),
        alpha = rep(0.5, (nrow(tree.df) + 1)),
        stringsAsFactors = FALSE
        );
    if (length(branch.names) > 1) {
        out.df$length2 <- c(NA, tree.df[, paste0('length.', branch.names[2])])
        out.df$edge.type.2 <- c(NA, tree.df[, paste0('edge.type.', branch.names[2])]);
        out.df$edge.colour.2 <- c(NA, tree.df[, paste0('edge.col.', branch.names[2])]);
        out.df$edge.width.2 <- c(NA, tree.df[, paste0('edge.width.', branch.names[2])]);
        }

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

        # check all children of the same parent is specified the same mode
        split.df <- split(tree.df, tree.df$parent);
        n.mode <- sapply(split.df, function(x) length(unique(x$mode)))

        if (any(n.mode > 1)) {
            invalid.parent.str <- paste(names(n.mode[n.mode > 1]), collapse = ' ')
            stop(paste('"mode" must be consistent across children sharing a parent.',
                'Multiple modes specified for children of:', invalid.parent.str
                ));
            }
    } else {
        tree.df$mode <- NA;
        }

    tree.df$mode[is.na(tree.df$mode)] <- radial.mode.name;
    return(tree.df);
    }

prep.tree.spread <- function(tree.df) {
    default.spread <- 1;
    if ('spread' %in% colnames(tree.df)) {
        tree.df$spread[is.na(tree.df$spread)] <- default.spread;
        tree.df$spread <- as.numeric(tree.df$spread);

        non.numeric <- is.na(tree.df$spread);
        if (any(non.numeric)) {
            warning('Non-numeric values found in tree input "spread" column.');
            tree.df$spread[non.numeric] <- default.spread;
            }
        if (any(tree.df$spread < 0, na.rm = TRUE)) {
            stop(paste(
                'Tree input "spread" column values must be positive.',
                'See documentation and User Guide vignette for more information on "spread".'
                ));
            }
    } else {
        tree.df$spread <- default.spread;
        }
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

        if (!(column.name %in% colnames(tree.df))) {
            tree.df[, column.name] <- NA;
            }
        tree.df[, column.name] <- prep.column.values(
            tree.df[, column.name],
            default.values = default.colour,
            conversion.fun = as.character
            );
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

prep.draw.node.setting <- function(tree.df) {
    if ('draw.node' %in% colnames(tree.df)) {
        NA.indices <- is.na(tree.df$draw.node);
        tree.df$draw.node <- as.logical(tree.df$draw.node);

        if (any(is.na(tree.df$draw.node) & !NA.indices)) {
            warning('Non-logical values found in "draw.node" column.');
            }

        tree.df$draw.node[is.na(tree.df$draw.node)] <- TRUE;
    } else {
        tree.df$draw.node <- TRUE;
        }
    return(tree.df);
    }

prep.node.colours <- function(tree.df, default.node.colour) {
    if ('node.col' %in% colnames(tree.df)) {
        tree.df$node.col[is.na(tree.df$node.col)] <- default.node.colour;
    } else {
        tree.df$node.col <- default.node.colour;
        }

    return(tree.df);
    }

prep.node.border.colours <- function(tree.df) {
    tree.df$border.col <- apply(
        tree.df,
        MARGIN = 1,
        FUN = function(row) {
            if (is.na(row['border.col'])) row['node.col'] else row['border.col'];
            }
        );

    return(tree.df);
    }

prep.node.border.type <- function(tree.df) {
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

    return(tree.df);
    }

prep.node.border.width <- function(tree.df) {
    if ('border.width' %in% colnames(tree.df)) {
        tree.df$border.width <- as.numeric(tree.df$border.width);
        tree.df$border.width[is.na(tree.df$border.width)] <- 1;
    } else {
        tree.df$border.width <- 1;
        }

    return(tree.df);
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
        a.name = 'connector.line.width',
        b.name = 'branch.line.width'
        );
    NA.indices <- is.na(branch.line.width)
    connector.line.width[NA.indices] <- branch.line.width[NA.indices];

    connector.line.width <- as.numeric(connector.line.width);
    non.numeric.indices <- is.na(connector.line.width);
    if (any(non.numeric.indices)) {
        warning(
            non.numeric.warning.message('connector.width', 'branch line width values')
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

check.radial.x.conflicts <- function(tree.df) {
    conflicts <- !is.na(tree.df$x) & tree.df$mode != 'dendrogram';
    if (any(conflicts)) {
        warning('"x" values on non-dendrogram nodes will be ignored.')
        }
    }

check.dendrogram.angle.conflicts <- function(tree.df) {
    conflicts <- (
        !is.na(tree.df$x) & tree.df$mode == 'dendrogram' & (!is.na(tree.df$angle) | !is.na(tree.df$spread))
        );
    if (any(conflicts)) {
        warning('"x" values override "angle" and "spread" values in dendrogram mode.')
        }
    }

# default.values must be either a scalar or matching length of column.values.
# A scalar will be applied to all NAs.
# Otherwise, the corresponding value from default.values will be used.
# conversion.fun is optional, and must return a vector of the same length.
prep.column.values <- function(
    column.values,
    default.values,
    conversion.fun = NULL
    ) {

    if (!is.null(conversion.fun)) {
        original.default.NAs <- is.na(default.values);
        default.values <- suppressWarnings(conversion.fun(default.values));
        if (any(is.na(default.values) & !original.default.NAs)) {
            stop('"default.values" incompatible with "conversion.fun" (NAs found after conversion).');
            }
        }

    n <- length(column.values);
    default.values.n <- length(default.values);
    if (default.values.n != 1 && default.values.n != n) {
        stop('"default.values" must be either length 1 or the same length as "column.values".');
        }

    replace.with.default <- function(x, NA.indices) {
        if (default.values.n == 1) {
            x[NA.indices] <- default.values;
        } else {
            x[NA.indices] <- default.values[NA.indices];
            }
        return(x);
        }
    original.NAs <- is.na(column.values);
    column.values <- replace.with.default(column.values, original.NAs);

    if (!is.null(conversion.fun)) {
        column.values <- conversion.fun(column.values);
        converted.n <- length(column.values);
        if (converted.n != n) {
            stop(paste(
                'Conversion function changed length of column values',
                paste0('(', 'expected ', n, ', ', 'received ', converted.n, ')')
                ));
            }
        converted.NAs <- is.na(column.values);
        if (any(converted.NAs & !original.NAs)) {
            warning('NAs found after conversion. Replacing with default values.');
            column.values <- replace.with.default(column.values);
            }
        }
    return(column.values);
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

get.branch.names <- function(tree.df) {
    prefix.regex <- '(edge\\.((type)|(width)|(col))\\.)|(length\\.?)';
    branch.columns <- grep(prefix.regex, colnames(tree.df), value = TRUE);
    branch.names <- gsub(prefix.regex, '', branch.columns);
    return(unique(branch.names));
    }
