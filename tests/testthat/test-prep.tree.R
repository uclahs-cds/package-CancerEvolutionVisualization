test_that(
    'prep.tree handles invalid parent column', {
    invalid.parent.tree <- data.frame();

    expect_error(
        prep.tree(
            tree.df = invalid.parent.tree,
            genes.df = NULL
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
            genes.df = NULL
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
            genes.df = NULL
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
    'filter.null.genes removes genes with no node', {
        num.valid.genes <- 4;
        num.invalid.genes <- 2;

        genes <- data.frame(
            node = c(
                rep(1, num.valid.genes),
                rep(NA, num.invalid.genes)
                ),
            gene = rep('gene', num.valid.genes + num.invalid.genes)
            );

        filtered.genes <- suppressWarnings(filter.null.genes(genes));

        expect_equal(nrow(filtered.genes), num.valid.genes);
    });

test_that(
    'filter.null.genes warns on invalid genes', {
        genes <- data.frame(
            node = c(NA)
            );

        expect_warning(filter.null.genes(genes));
    });

test_that(
    'filter.null.genes handles valid genes', {
        num.genes <- 3;

        genes <- data.frame(
            node = 1:num.genes,
            gene = rep('gene', num.genes)
            );

        filtered.genes <- filter.null.genes(genes);

        expect_equal(nrow(genes), nrow(filtered.genes));
    });

test_that(
    'filter.invalid.gene.nodes removes invalid node IDs', {
        tree <- data.frame(
            parent = c(NA, 1, 1)
            );

        valid.node.id <- rownames(tree)[[1]];
        invalid.node.id <- -1;

        num.valid.genes <- 3;
        num.invalid.genes <- 2;

        genes <- data.frame(
            node = c(
                rep(valid.node.id, num.valid.genes),
                rep(invalid.node.id, num.invalid.genes)
                ),
            gene = rep('gene', num.valid.genes + num.invalid.genes)
            );

        filtered.genes <- suppressWarnings(filter.invalid.gene.nodes(
            genes,
            rownames(tree)
            ));

        expect_equal(nrow(filtered.genes), num.valid.genes);
    });

test_that(
    'filter.invalid.gene.nodes warns on invalid node IDs', {
        tree <- data.frame(
            parent = 1:2
            );

        genes <- data.frame(
            node = c(-1)
            );

        expect_warning(filter.invalid.gene.nodes(genes, rownames(tree)));
    });

test_that(
    'filter.invalid.gene.nodes handles valid node IDs', {
        tree <- data.frame(
            parent = c(1:3)
            );

        genes <- data.frame(
            node = rownames(tree),
            gene = rep('gene', nrow(tree))
            );

        filtered.genes <- filter.invalid.gene.nodes(genes, rownames(tree));

        expect_equal(nrow(filtered.genes), nrow(genes));
    });
