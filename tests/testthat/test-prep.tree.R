test_that(
    'prep.tree handles invalid parent column', {
    invalid.parent.tree <- data.frame();

    expect_error(
        prep.tree(
            tree.df = invalid.parent.tree,
            text.df = NULL,
            polygon.colour.scheme = colours
            ),
        regexp = 'parent'
        );
    });

test_that(
    'prep.tree handles invalid CP column', {
    invalid.CP.tree <- data.frame(
        parent = c(NA),
        CP = c('NA')
        );

    result.cp <- suppressWarnings(
        prep.tree(
            tree.df = invalid.CP.tree,
            text.df = NULL,
            polygon.colour.scheme = colours
            )$in.tree.df$ccf
        );

    expect_true((!is.null(result.cp)) && all(is.na(result.cp)));
    });

test_that(
    'prep.tree warns on invalid CP column', {
    invalid.CP.tree <- data.frame(
        parent = c(NA),
        CP = c('invalid')
        );

    expect_warning(
        prep.tree(
            tree.df = invalid.CP.tree,
            text.df = NULL,
            polygon.colour.scheme = colours
            ),
        regexp = 'CP'
        );
    });

test_that(
    'prep.tree passes valid edge 1 colour values', {
        tree.df <- data.frame(
            parent = c(NA, 1:3),
            edge.col.1 = 'red'
            );

        result <- prep.tree(
            tree.df,
            text.df = NULL,
            polygon.colour.scheme = colours
            );

        result.edge.colours <- result$in.tree.df$edge.colour.1;
        expected.edge.colours <- c(NA, tree.df$edge.col.1);

        expect_equal(result.edge.colours, expected.edge.colours);
    });


test_that(
    'prep.tree passes valid edge 2 colour values', {
        tree.df <- data.frame(
            parent = c(NA, 1:3),
            edge.col.1 = 'black',
            edge.col.2 = 'red'
            );

        result <- prep.tree(
            tree.df,
            text.df = NULL,
            polygon.colour.scheme = colours
            );

        result.edge.colours <- result$in.tree.df$edge.colour.2;
        expected.edge.colours <- c(NA, tree.df$edge.col.2);

        expect_equal(result.edge.colours, expected.edge.colours);
    });

test_that(
    'prep.tree passes valid edge 1 width values', {
        tree.df <- data.frame(
            parent = c(NA, 1:3),
            edge.width.1 = 1:4
            );

        result <- prep.tree(
            tree.df,
            text.df = NULL,
            polygon.colour.scheme = colours
            );

        result.edge.widths <- result$in.tree.df$edge.width.1;
        expected.edge.widths <- c(NA, tree.df$edge.width.1);

        expect_equal(result.edge.widths, expected.edge.widths);
    });


test_that(
    'prep.tree passes valid edge 2 width values', {
        tree.df <- data.frame(
            parent = c(NA, 1:3),
            edge.width.1 = 3,
            edge.width.2 = 1:4
            );

        result <- prep.tree(
            tree.df,
            text.df = NULL,
            polygon.colour.scheme = colours
            );

        result.edge.widths <- result$in.tree.df$edge.width.2;
        expected.edge.widths <- c(NA, tree.df$edge.width.2);

        expect_equal(result.edge.widths, expected.edge.widths);
    });

test_that(
    'prep.tree passes valid edge 1 linetype values', {
        tree.df <- data.frame(
            parent = c(NA, 1:3),
            edge.type.1 = 'dotted'
            );

        result <- prep.tree(
            tree.df,
            text.df = NULL,
            polygon.colour.scheme = colours
            );

        result.edge.linetypes <- result$in.tree.df$edge.type.1;
        expected.edge.linetypes <- c(NA, tree.df$edge.type.1);

        expect_equal(result.edge.linetypes, expected.edge.linetypes);
    });


test_that(
    'prep.tree passes valid edge 2 linetype values', {
        tree.df <- data.frame(
            parent = c(NA, 1:3),
            length1 = 5,
            edge.type.2 = 'solid'
            );

        result <- prep.tree(
            tree.df,
            text.df = NULL,
            polygon.colour.scheme = colours
            );

        result.edge.linetypes <- result$in.tree.df$edge.type.2;
        expected.edge.linetypes <- c(NA, tree.df$edge.type.2);

        expect_equal(result.edge.linetypes, expected.edge.linetypes);
    });

test_that(
    'prep.tree.parent handles values of 0', {
        parent <- c(0:5);

        expect_equal(
            CancerEvolutionVisualization:::prep.tree.parent(parent)[1],
            -1
            );
    });

test_that(
    'prep.tree.parent handles values of NA', {
        parent <- c(NA, 1:5);

        expect_equal(
            CancerEvolutionVisualization:::prep.tree.parent(parent)[1],
            -1
            );
    });

test_that(
    'prep.tree.parent has correct output length', {
        parent <- c(0, NA, 1, 3, 2);
        expected.length <- length(parent);

        expect_length(
            CancerEvolutionVisualization:::prep.tree.parent(parent),
            expected.length
            );
    });

test_that(
    'reorder.nodes.by.CP result sorts by CP', {
        tree.in.order <- data.frame(
            CP = c(1, 0.65, 0.32),
            parent = c(NA, 1, 1)
            );

        reordered <- reorder.nodes.by.CP(tree.in.order[c(3, 1, 2), ]);

        expected.order <- rownames(tree.in.order);

        expect_equal(
            rownames(reordered),
            expected.order
            );
    });

test_that(
    'reorder.nodes.by.CP handles nodes with equal CP', {
        tree.in.order <- data.frame(
            CP = c(0.3, 0.3),
            parent = c(1, 2)
            );

        reordered <- reorder.nodes.by.CP(tree.in.order[c(2, 1), ]);

        expected.order <- rownames(tree.in.order);

        expect_equal(
            rownames(reordered),
            expected.order
            );
    });

test_that(
    'reorder.nodes.by.CP handles nodes with NA CP', {
        tree.in.order <- data.frame(
            CP = c(NA, NA),
            parent = c(1, 2)
            );

        reordered <- reorder.nodes.by.CP(tree.in.order[c(2, 1), ]);

        expected.order <- rownames(tree.in.order);

        expect_equal(
            rownames(reordered),
            expected.order
            );
    });

test_that(
    'reorder.trunk.node handles incorrect trunk placement', {
        trunk.index <- 3;

        tree <- data.frame(
            parent = c(rep(trunk.index, trunk.index - 1), NA),
            CP = c(0.6, 0.4, 1)
            );

        reordered.tree <- reorder.trunk.node(tree);

        expect_equal(
            rownames(reordered.tree)[[1]],
            as.character(trunk.index)
            );
    });

test_that(
    'reorder.trunk.node handles valid trunk placement', {
        tree <- data.frame(
            parent = c(NA, 1),
            CP = c(1, 0.5)
            );

        reordered.tree <- reorder.trunk.node(tree);

        expect_equal(
            rownames(reordered.tree),
            rownames(tree)
            );
    });

test_that(
    'reset.tree.node.ids handles new row names', {
        tree <- data.frame(
            parent = c(-1, 'one', 'two', 'two'),
            CP = c(0.96, 0.76, 0.28, 0.31),
            row.names = c('one', 'two', 'three', 'four')
            );

        value.index <- as.list(c(-1, 1:nrow(tree)));
        names(value.index) <- c(-1, rownames(tree));

        tree.new.names <- reset.tree.node.ids(tree, value.index);

        expected.row.names <- as.character(1:length(rownames(tree)));

        expect_equal(
           rownames(tree.new.names),
           expected.row.names
        );
    });

test_that(
    'check.parent.values handles valid values', {
        node.names <- 1:10
        parents <- c(1:10, 5, 6, 4);

        expect_true(check.parent.values(node.names, parents));
    });

test_that(
    'check.parent.values handles invalid values', {
        node.names <- 1:5
        parents <- c(1:10, 2, 1);

        expect_false(check.parent.values(node.names, parents));
    });

test_that(
    'check.parent.values handles -1 values', {
        node.names <- 1:10
        parents <- c(-1, 1:5, 4, 1);

        expect_true(check.parent.values(node.names, parents));
    });

test_that(
    'is.circular.node.parent handles a circular reference', {
        parent <- c(NA, 3, 2);
        node.names <- c(1, 2, 3);

        tree <- data.frame(
            parent,
            row.names = node.names,
            drop = FALSE
            );

        expect_true(is.circular.node.parent(tree, node = 2));
    });

test_that(
    'is.circular.node.parent handles valid parent', {
        parent <- c(NA, 1, 1);
        node.names <- c(1, 2, 3);

        tree <- data.frame(
            parent,
            row.names = node.names,
            drop = FALSE
            );

        expect_false(is.circular.node.parent(tree, node = 3));
    });

test_that(
    'is.circular.node.parent handles the root node', {
        parent <- c(NA, 1, 1, 2);

        tree <- data.frame(parent, drop = FALSE);

        expect_false(is.circular.node.parent(tree, node = 1));
    });

test_that(
    'get.root.node handles valid NA root', {
        parent <- c(2, NA, 1);
        expected.root <- which(is.na(parent));

        tree <- data.frame(parent = parent);
        root <- get.root.node(tree);

        expect_equal(root, expected.root);
    });

test_that(
    'get.root.node handles valid -1 root', {
        parent <- c(2, -1, 1);
        expected.root <- which(parent == -1);

        tree <- data.frame(parent = parent);
        root <- get.root.node(tree);

        expect_equal(root, expected.root);
    });

test_that(
    'get.root.node handles valid 0 root', {
        parent <- c(2, 0, 1);
        expected.root <- which(parent == 0);

        tree <- data.frame(parent = parent);
        root <- get.root.node(tree);

        expect_equal(root, expected.root);
    });

test_that(
    'get.root.node handles multiple root nodes', {
        root.parent <- -1;
        tree <- data.frame(parent = rep(root.parent, 2));

        expect_error(get.root.node(tree), regexp = 'root')
    });

test_that(
    'get.root.node handles a missing root node', {
        tree <- data.frame(parent = 1:3);

        expect_error(get.root.node(tree), regexp = 'root');
    });

test_that(
    'get.y.axis.position handles a single branch length column', {
        valid.colname <- 'length1';
        cols <- c(valid.colname, 'invalid');

        yaxis.position <- get.y.axis.position(cols);
        expected.position <- 'left';

        expect_equal(yaxis.position, expected.position);
    });

test_that(
    'get.y.axis.position handles multiple valid branch length columns', {
        valid.colnames <- sapply(
            1:3,
            FUN = function(x) {
                paste0('length', x);
                }
            );

        cols <- c(
            valid.colnames,
            'invalid.col'
            );

        yaxis.position <- get.y.axis.position(cols);
        expected.position <- 'both';

        expect_equal(yaxis.position, expected.position);
    });

test_that(
    'get.y.axis.position handles no valid branch length columns', {
        cols <- c('parent');

        yaxis.position <- get.y.axis.position(cols);
        expected.position <- 'none';

        expect_equal(yaxis.position, expected.position);
    });

test_that(
    'prep.edge.colours handles valid colour columns', {
        tree <- data.frame(
            parent = 1:10,
            edge.col.1 = c('red'),
            edge.col.2 = c('blue')
            );

        result <- prep.edge.colours(tree);

        expect_equal(result, tree);
    });

test_that(
    'prep.edge.colours adds missing colour columns', {
        tree <- data.frame(
            parent = 1:10
            );

        result <- prep.edge.colours(tree);
        expected.edge.colnames <- sapply(
            1:2, function(i) paste('edge', 'col', i, sep = '.')
            );
        edge.columns.found <- expected.edge.colnames %in% colnames(result);

        expect_true(all(edge.columns.found));
    });

test_that(
    'prep.edge.colours includes all tree columns', {
        tree <- data.frame(
            parent = c(NA, 1:9),
            length1 = 1:10
            );

        result <- prep.edge.colours(tree);
        tree.columns.found <- colnames(tree) %in% colnames(result);

        expect_true(all(tree.columns.found));
    });

test_that(
    'prep.edge.colour.column handles valid column values', {
        tree <- data.frame(
            parent = c(NA, 1:9)
            );

        valid.colour <- 'green';
        column.name <- 'edge.col.1';
        tree[, column.name] <- valid.colour;

        result <- prep.edge.colour.column(tree, column.name, 'blue');

        expect_equal(result, tree[, column.name]);
    });

test_that(
    'prep.edge.colour.column replaces NA with default', {
        tree <- data.frame(
            parent = c(NA, 1:9)
        );

        column.name <- 'edge.col.1';
        tree[, column.name] <- 'green';

        NA.indices <- 2:4;
        tree[NA.indices, column.name] <- NA;

        default.colour <- 'black';
        result <- prep.edge.colour.column(tree, column.name, default.colour);
        result.replaced <- result[NA.indices] == default.colour;

        expect_true(all(result.replaced));
    });

test_that(
    'prep.edge.colour.column returns correct length with missing column', {
        tree <- data.frame(
            parent = c(NA, 1:9)
            );

        result <- prep.edge.colour.column(tree, 'edge.col.1', 'black');
        expected.num.rows <- nrow(tree);

        expect_equal(length(result), expected.num.rows);
    });

test_that(
    'prep.edge.colour.column returns default with missing column', {
        tree <- data.frame(
            parent = c(NA, 1:9)
            );

        default.colour <- 'black';
        result <- prep.edge.colour.column(tree, 'edge.col.1', default.colour);
        match.default <- result == default.colour;

        expect_true(all(match.default));
    });

test_that(
    'prep.node.label.colours returns valid values', {
        node.label.colours <- c('green', 'white');
        tree.df <- data.frame(
            node.label.col = node.label.colours,
            node.col = 'red'
            );

        result <- prep.node.label.colours(tree.df);
        expected.label.colours <- node.label.colours;

        expect_equal(result, expected.label.colours);
    });

test_that(
    'prep.node.label.colours replaces NAs with default value', {
        tree.df <- data.frame(
            node.label.col = 'black',
            node.col = rep('red', 10)
            );

        NA.indices <- 3:(nrow(tree.df));
        tree.df$node.label.col[NA.indices] <- NA;

        default.label.colour <- 'white';

        local({
            get.default.node.label.colour <- function(node.colour) {
                default.label.colour;
                }

            result <- prep.node.label.colours(tree.df);

            expected.label.colours <- tree.df$node.label.col;
            expected.label.colours[NA.indices] <- default.label.colour;
            expect_equal(result, expected.label.colours);
            });
    });

test_that(
    'prep.node.label.colours errors if "node.col" column does not exist', {
        tree.df <- data.frame(parent = 1:5);

        expect_error(
            prep.node.label.colours(tree.df),
            regexp = '"node.col"'
            );
    });

test_that(
    'prep.node.label.colours errors if "node.col" column contains NAs', {
        tree.df <- data.frame(node.col = c(NA, 1:3));

        expect_error(
            prep.node.label.colours(tree.df),
            regexp = '"node.col"'
            );
    });

test_that(
    'prep.column.values errors if "default.values" vector length does not match "column.values"', {
        n <- 5;
        column.values <- rep(1, n);
        default.values <- rep(2, n + 2);

        expect_error(
            prep.column.values(column.values, default.values),
            regexp = 'default'
            );
    });

test_that(
    'prep.column.values replaces NAs with scalar default value', {
        NA.indices <- c(1, 4);
        column.values <- rep(1, 5);
        column.values[NA.indices] <- NA;

        default.value <- 2;

        result <- prep.column.values(column.values, default.value);
        expect_equal(
            result[NA.indices],
            rep(default.value, length(NA.indices))
            );
    });

test_that(
    'prep.column.values replaces NAs with scalar default value', {
        n <- 5;
        column.values <- rep(1, n);
        NA.indices <- c(1, 4);
        column.values[NA.indices] <- NA;

        default.values <- 1:n;

        result <- prep.column.values(column.values, default.values);
        expect_equal(
            result[NA.indices],
            NA.indices
            );
    });

test_that(
    'prep.column.values leaves non-NA values unchanged', {
        valid.indices <- c(1, 2, 4);
        value <- 3;
        column.values <- rep(NA, 5);
        column.values[valid.indices] <- value;

        result <- prep.column.values(column.values, default.value = 2);
        expect_equal(
            result[valid.indices],
            rep(value, length(valid.indices))
            );
    });

test_that(
    'prep.column.values allows "default.values" to contains NAs', {
        n <- 5;
        column.values <- rep(1, n);
        default.values <- rep(2, n);
        default.values[3] <- NA;

        expect_no_error(prep.column.values(column.values, default.values));
    });

test_that(
    'prep.column.values errors if "conversion.fun" changes column length', {
        column.values <- rep(1, 5);
        conversion.fun <- function(x) rep(x, 2);

        expect_error(
            prep.column.values(column.values, 2, conversion.fun = conversion.fun),
            regexp = 'length'
            );
    });

test_that(
    'prep.column.values errors if "conversion.fun" introduces NAs in "default.values"', {
        column.values <- rep(1, 5);
        default.values <- 'test';
        conversion.fun <- as.numeric;

        expect_error(
            prep.column.values(
                column.values,
                default.values,
                conversion.fun = conversion.fun
                ),
            regexp = 'conversion'
            );
    });

test_that(
    'prep.column.values warns if "conversion.fun" introduces NAs in "column.values"', {
        column.values <- rep('hello', 5);
        default.values <- 1;
        conversion.fun <- as.numeric;

        expect_warning(
            prep.column.values(
                column.values,
                default.values,
                conversion.fun = conversion.fun
                ),
            regexp = 'conversion'
            );
    });

test_that(
    'prep.column.values applies conversion function', {
        column.values <- rep('10', 5);
        default.values <- 1;
        conversion.fun <- as.numeric;

        result <- prep.column.values(
            column.values,
            default.values,
            conversion.fun = conversion.fun
            );
        expect_true(is.numeric(result));
    });

test_that(
    'prep.column.values replaces NA values after conversion with default', {
        column.values <- rep(4, 5);
        non.numeric.indices <- c(2, 3);
        column.values[non.numeric.indices] <- 'test';
        default.values <- 1;
        conversion.fun <- as.numeric;

        result <- suppressWarnings(prep.column.values(
            column.values,
            default.values,
            conversion.fun = conversion.fun
            ));
        expect_equal(
            result[non.numeric.indices],
            rep(default.values, length(non.numeric.indices))
            );
        expect_true(is.numeric(result));
    });

test_that(
    'get.branch.names handles "length" columns', {
        branch.names <- c('test', 'CEV');

        tree.df <- data.frame(lapply(1:length(branch.names), function(i) 1:10));
        colnames(tree.df) <- paste('length', branch.names, sep = '');

        result <- get.branch.names(tree.df);
        expect_equal(result, branch.names);
    });

test_that(
    'get.branch.names handles "edge.col" columns', {
        branch.names <- c('test', 'SRC');

        tree.df <- data.frame(lapply(1:length(branch.names), function(i) 1:10));
        colnames(tree.df) <- paste('edge.col', branch.names, sep = '.');

        result <- get.branch.names(tree.df);
        expect_equal(result, branch.names);
    });

test_that(
    'get.branch.names handles "edge.type" columns', {
        branch.names <- c('test', 'SRC');

        tree.df <- data.frame(lapply(1:length(branch.names), function(i) 1:10));
        colnames(tree.df) <- paste('edge.type', branch.names, sep = '.');

        result <- get.branch.names(tree.df);
        expect_equal(result, branch.names);
    });

test_that(
    'get.branch.names handles "edge.width" columns', {
        branch.names <- c('test', 'SRC');

        tree.df <- data.frame(lapply(1:length(branch.names), function(i) 1:10));
        colnames(tree.df) <- paste('edge.width', branch.names, sep = '.');

        result <- get.branch.names(tree.df);
        expect_equal(result, branch.names);
    });

test_that(
    'get.branch.names handles different edge columns', {
        branch.names <- c('test', 'SRC');

        tree.df <- data.frame(lapply(1:length(branch.names), function(i) 1:10));
        colnames(tree.df) <- paste(c('edge.type', 'length'), branch.names, sep = '.');

        result <- get.branch.names(tree.df);
        expect_equal(result, branch.names);
    });

test_that(
    'get.branch.names returns unique values', {
        branch.names <- c('test', 'SRC');
        branch.columns <- paste(
            c('edge.type', 'length', 'length'),
            c(rep(branch.names[1], 2), branch.names[2]),
            sep = '.'
            );

        tree.df <- data.frame(lapply(1:length(branch.columns), function(i) 1:10));
        colnames(tree.df) <- branch.columns;

        result <- get.branch.names(tree.df);
        expect_equal(result, branch.names);
    });

test_that(
    'get.branch.names returns unique values', {
        branch.names <- c('test', 'SRC');
        branch.columns <- paste(
            c('edge.type', 'length', 'length'),
            c(rep(branch.names[1], 2), branch.names[2]),
            sep = '.'
            );

        tree.df <- data.frame(lapply(1:length(branch.columns), function(i) 1:10));
        colnames(tree.df) <- branch.columns;

        result <- get.branch.names(tree.df);
        expect_equal(result, branch.names);
    });

test_that(
    'check.radial.x.conflicts does not warn when "x" present in dendrogram mode', {
        tree.df <- data.frame(
            x = 1:10,
            mode = 'dendrogram'
            );
        expect_no_warning(check.radial.x.conflicts(tree.df));
    });

test_that(
    'check.radial.x.conflicts does not warn when "x" is NA in radial mode', {
        tree.df <- data.frame(
            parent = c(NA, 1, 2),
            mode = 'radial',
            x = NA
            );
        expect_no_warning(check.radial.x.conflicts(tree.df));
    });

test_that(
    'check.radial.x.conflicts does not warn when "x" is NA in dendrogram mode', {
        tree.df <- data.frame(
            parent = c(NA, 1, 2),
            mode = 'dendrogram',
            x = NA
            );
        expect_no_warning(check.radial.x.conflicts(tree.df));
    });

test_that(
    'check.radial.x.conflicts warns when "x" is present in radial mode', {
        tree.df <- data.frame(
            x = 1:10,
            mode = 'radial'
            );
        expect_warning(
            check.radial.x.conflicts(tree.df),
            regexp = 'dendrogram'
            );
    });

test_that(
    'check.dendrogram.angle.conflicts warns when "x" is present with "angle" in dendrogram mode', {
        tree.df <- data.frame(
            x = 1:10,
            angle = 4,
            mode = 'dendrogram',
            spread = NA
            );
        expect_warning(
            check.dendrogram.angle.conflicts(tree.df),
            regexp = 'angle'
            );
    });

test_that(
    'check.dendrogram.angle.conflicts does not warn when "x" is present with "angle" in radial mode', {
        tree.df <- data.frame(
            x = 1:10,
            angle = 15,
            mode = 'radial',
            spread = NA
            );
        expect_no_warning(check.dendrogram.angle.conflicts(tree.df));
    });

test_that(
    'check.dendrogram.angle.conflicts does not warn when "x" is present with "spread" in radial mode', {
        tree.df <- data.frame(
            x = 1:10,
            spread = 1.5,
            mode = 'radial',
            angle = NA
            );
        expect_no_warning(check.dendrogram.angle.conflicts(tree.df));
    });

test_that(
    'check.dendrogram.angle.conflicts warns when "x" is present with "angle" in dendrogram mode', {
        tree.df <- data.frame(
            x = 1:10,
            angle = 15,
            mode = 'dendrogram',
            spread = NA
            );
        expect_warning(
            check.dendrogram.angle.conflicts(tree.df),
            regexp = 'angle'
            );
    });

test_that(
    'prep.tree.spread result is numeric', {
        tree.df <- data.frame(
            spread = 1:7
            );
        result <- prep.tree.spread(tree.df);

        expect_true(all(is.numeric(result$spread)));
    });

test_that(
    'prep.tree.spread replaces NAs with 1', {
        tree.df <- data.frame(
            spread = 1:10
            );
        na.indices <- 3:5;
        tree.df$spread[na.indices] <- NA;

        result <- prep.tree.spread(tree.df);
        expect_equal(result$spread[na.indices], rep(1, length(na.indices)));
    });

test_that(
    'prep.tree.spread warns on non-numeric values', {
        tree.df <- data.frame(
            spread = c(1:3, 'test')
            );

        expect_warning(
            prep.tree.spread(tree.df),
            regexp = 'spread'
            );
    });

test_that(
    'check.dendrogram.angle.conflicts warns when "x" is present with "spread" in dendrogram mode', {
        tree.df <- data.frame(
            x = 1:10,
            spread = 1.5,
            mode = 'dendrogram',
            angle = NA
            );
        expect_warning(
            check.dendrogram.angle.conflicts(tree.df),
            regexp = 'spread'
        );
    });

test_that(
    'prep.tree.spread errors on negative values', {
        tree.df <- data.frame(
            spread = c(1:3, -2)
            );

        expect_error(
            prep.tree.spread(tree.df),
            regexp = 'spread'
            );
    });

test_that(
    'prep.node.size replaces NAs with 1', {
        tree.df <- data.frame(
            node.size = rep(2, 4),
            draw.node = TRUE
            );

        NA.indices <- 2:3;
        tree.df$node.size[NA.indices] <- NA;

        result <- prep.node.size(tree.df);
        expected.result <- tree.df$node.size;
        expected.result[NA.indices] <- 1;

        expect_equal(result, expected.result);
    });

test_that(
    'prep.node.size sets node size to 0 when node is not drawn', {
        tree.df <- data.frame(
            node.size = rep(2, 4),
            draw.node = TRUE
        );

        hidden.nodes <- 2:3;
        tree.df$draw.node[hidden.nodes] <- FALSE;

        result <- prep.node.size(tree.df);
        expected.result <- tree.df$node.size;
        expected.result[hidden.nodes] <- 0;

        expect_equal(result, expected.result);
    });

test_that(
    'prep.plotting.direction errors if value is not a scalar', {
        direction <- 1:10;
        expect_error(
            {
                prep.plotting.direction(direction);
            },
            regexp = 'scalar'
        );
    });

test_that(
    'prep.plotting.direction handles "down" plotting direction', {
        direction <- 'down';
        result <- prep.plotting.direction(direction);
        expected.result <- 0;
        expect_equal(result, expected.result);
    });

test_that(
    'prep.plotting.direction handles "up" plotting direction', {
        direction <- 'up';
        result <- prep.plotting.direction(direction);
        expected.result <- pi;
        expect_equal(result, expected.result);
    });

test_that(
    'prep.plotting.direction handles "left" plotting direction', {
        direction <- 'left';
        result <- prep.plotting.direction(direction);
        expected.result <- -pi / 2;
        expect_equal(result, expected.result);
    });

test_that(
    'prep.plotting.direction handles "right" plotting direction', {
        direction <- 'right';
        result <- prep.plotting.direction(direction);
        expected.result <- pi / 2;
        expect_equal(result, expected.result);
    });

test_that(
    'prep.plotting.direction handles numeric radian plotting direction', {
        direction <- pi * 0.76;
        result <- prep.plotting.direction(direction, radians = TRUE);
        expect_equal(result, direction);
    });

test_that(
    'prep.plotting.direction converts degrees plotting direction to radians', {
        degrees <- 76;
        result <- prep.plotting.direction(degrees, radians = FALSE);
        expected.result <- degrees.to.radians(degrees)
        expect_equal(result, expected.result);
    });

test_that(
    'prep.plotting.direction simplifies numeric plotting direction larger than 360 degrees', {
        true.angle <- pi / 3.5;
        result <- prep.plotting.direction(true.angle + (2 * pi), radians = TRUE);
        expect_equal(result, true.angle);
    });

test_that(
    'prep.plotting.direction errors on invalid character direction', {
        direction <- 'invalid';
        expect_error(
            {
                prep.plotting.direction(direction);
                },
            regexp = 'direction'
            );
    });

test_that(
    'prep.plotting.direction errors on non-character/numeric input', {
        direction <- TRUE;
        expect_error(
            {
                prep.plotting.direction(direction);
            },
            regexp = 'numeric or one of'
        );
    });
