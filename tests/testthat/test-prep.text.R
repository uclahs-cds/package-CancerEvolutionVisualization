
test_that(
    'filter.null.text removes genes with no node', {
        num.valid.genes <- 4;
        num.invalid.genes <- 2;

        genes <- data.frame(
            node = c(
                rep(1, num.valid.genes),
                rep(NA, num.invalid.genes)
                ),
            gene = rep('gene', num.valid.genes + num.invalid.genes)
            );

        filtered.genes <- suppressWarnings(filter.null.text(genes));

        expect_equal(nrow(filtered.genes), num.valid.genes);
    });

test_that(
    'filter.null.text warns on invalid genes', {
        genes <- data.frame(
            node = c(NA)
            );

        expect_warning(filter.null.text(genes));
    });

test_that(
    'filter.null.text handles valid genes', {
        num.genes <- 3;

        genes <- data.frame(
            node = 1:num.genes,
            gene = rep('gene', num.genes)
            );

        filtered.genes <- filter.null.text(genes);

        expect_equal(nrow(genes), nrow(filtered.genes));
    });

test_that(
    'filter.invalid.text.nodes removes invalid node IDs', {
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

        filtered.genes <- suppressWarnings(filter.invalid.text.nodes(
            genes,
            rownames(tree)
            ));

        expect_equal(nrow(filtered.genes), num.valid.genes);
    });

test_that(
    'filter.invalid.text.nodes warns on invalid node IDs', {
        tree <- data.frame(
            parent = 1:2
            );

        genes <- data.frame(
            node = c(-1)
            );

        expect_warning(filter.invalid.text.nodes(genes, rownames(tree)));
    });

test_that(
    'filter.invalid.text.nodes handles valid node IDs', {
        tree <- data.frame(
            parent = c(1:3)
            );

        genes <- data.frame(
            node = rownames(tree),
            gene = rep('gene', nrow(tree))
            );

        filtered.genes <- filter.invalid.text.nodes(genes, rownames(tree));

        expect_equal(nrow(filtered.genes), nrow(genes));
    });

test_that(
    'add.default.text.columns handles omitted CNA data' , {
        genes <- data.frame(
            name = c('EXAMPLE', 'GENE', 'DATA')
            );

        result <- add.default.text.columns(genes);

        expect_true(all(is.na(result$CN)));
    });

test_that(
    'add.default.text.columns handles omitted SNV data' , {
        genes <- data.frame(
            name = c('GENE', 'EXAMPLE')
            );

        result <- add.default.text.columns(genes);

        expect_true(all(!(result$SNV)));
    });

test_that(
    'add.default.text.columns does not modify existing CNA data' , {
        expected.cna <- c(1, -1);

        genes <- data.frame(
            name = c('TEST', 'GENES'),
            CNA = expected.cna
            );

        result <- add.default.text.columns(genes);

        expect_equal(result$CNA, expected.cna);
    });

test_that(
    'add.default.text.columns does not modify existing CNA data' , {
        expected.snv <- c(TRUE, TRUE, FALSE);

        genes <- data.frame(
            name = c('GENES', 'FOR', 'TESTING'),
            SNV = expected.snv
            );

        result <- add.default.text.columns(genes);

        expect_equal(result$SNV, expected.snv);
    });

test_that(
    'reorder.text sorts by gene ID', {
        genes <- data.frame(
            node = 4:1,
            CNA = NA
            );

        out.of.order <- genes[c(4, 1, 3, 2), ];
        reordered <- reorder.text(out.of.order);

        expect_equal(rownames(reordered), rownames(genes));
        ;
    });

test_that(
    'reorder.text resolves node ID ties by CNA', {
        genes <- data.frame(
            node = c(1, 1),
            CNA = c(-1, 1)
            );

        reordered <- reorder.text(genes);
        expected.rownames <- as.character(c(2, 1));

        expect_equal(rownames(reordered), expected.rownames);
        ;
    });

test_that(
    'prep.text.line.dist handles valid values', {
        valid <- 0.5;
        result <- prep.text.line.dist(valid);

        expect_equal(result, valid);
    });

test_that(
    'prep.text.line.dist changes invalid values less than 0', {
        invalid <- -1;
        result <- suppressWarnings(prep.text.line.dist(invalid));

        expect_equal(result, 0);
    });

test_that(
    'prep.text.line.dist changes invalid values greater than 1', {
        invalid <- 2.5;
        result <- suppressWarnings(prep.text.line.dist(invalid));

        expect_equal(result, 1);
    });

test_that(
    'prep.text.line.dist warns on invalid values', {
        invalid <- 30;

        expect_warning(prep.text.line.dist(invalid));
    });