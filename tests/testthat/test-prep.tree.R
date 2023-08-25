test_that(
    'prep.tree handles invalid parent column', {
    invalid.parent.tree <- data.frame();

    expect_error(
        prep.tree(
            tree.df = invalid.parent.tree,
            text.df = NULL,
            colour.scheme = colours
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
            colour.scheme = colours
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
            colour.scheme = colours
            ),
        regexp = 'CP'
        );
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
    'prep.node.label.colours returns valid values', {
        node.label.colours <- c('green', 'white');
        tree.df <- data.frame(
            node.label.col = node.label.colours,
            node.col = 'red'
            );

        result <- prep.node.label.colours(tree.df);
        expected.label.colours <- node.label.colours;
        
        print(result)
        print(expected.label.colours)

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
            local_mock(
                get.default.node.label.colour = function(node.colour) {
                    default.label.colour;
                    }
                );

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
            { prep.node.label.colours(tree.df); },
            regexp = '"node.col"'
            );
    });

test_that(
    'prep.node.label.colours errors if "node.col" columb contains NAs', {
        tree.df <- data.frame(node.col = c(NA, 1:3));

        expect_error(
            { prep.node.label.colours(tree.df); },
            regexp = '"node.col"'
            );
    });
