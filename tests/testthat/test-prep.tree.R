test_that(
    'prep.tree handles invalid parent column', {
    invalid.parent.tree <- data.frame();
    
    expect_error(
        prep.tree(tree.df = invalid.parent.tree),
        regexp = 'parent'
        );
    });

test_that(
    'prep.tree handles invalid CP column', {
    invalid.CP.tree <- data.frame(parent = c(NA));
    
    expect_error(
        prep.tree(tree.df = invalid.CP.tree),
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
    'reorder.clones output is correct', {
        parent <- c(0, 1);
        child <- c(1, 2);
        CP <- c(0.52, 0.43);

        tree <- data.frame(
             parent = parent,
             child = child,
             CP = CP
             );
        
        reordered <- reorder_clones(tree);

        expected.output <- data.frame(
            parent = c(-1, parent[2:length(parent)]),
            child = child,
            CP = CP
            );

        expect_equal(
            as.numeric(reordered$parent),
            expected.output$parent
            );

        expect_equal(
            as.numeric(reordered$child),
            expected.output$child
            );

        expect_equal(
            as.numeric(reordered$CP),
            expected.output$CP
            );
    });

test_that(
    'reorder.nodes result is in the correct order', {
        tree.in.order <- data.frame(
            CP = c(1, 0.65, 0.32),
            parent = c(NA, 1, 1)
            );

        reordered <- reorder.nodes(tree.in.order[c(3, 1, 2), ]);

        expected.order <- rownames(tree.in.order);

        expect_equal(
            rownames(reordered),
            expected.order
            );
    });

test_that(
    'reset.node.names handles new row names', {
        tree <- data.frame(
            parent = c(-1, 'one', 'two', 'two'),
            CP = c(0.96, 0.76, 0.28, 0.31),
            row.names = c('one', 'two', 'three', 'four')
            );

        tree.new.names <- reset.node.names(tree);

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
