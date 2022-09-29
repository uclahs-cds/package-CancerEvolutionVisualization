test_that(
    'filter.null.text removes rows with no node', {
        num.valid.rows <- 4;
        num.invalid.rows <- 2;

        node.text <- data.frame(
            node = c(
                rep(1, num.valid.rows),
                rep(NA, num.invalid.rows)
                ),
            gene = rep('gene', num.valid.rows + num.invalid.rows)
            );

        filtered.text <- suppressWarnings(filter.null.text(node.text));

        expect_equal(nrow(filtered.text), num.valid.rows);
    });

test_that(
    'filter.null.text warns on invalid rows', {
        node.text <- data.frame(
            node = c(NA)
            );

        expect_warning(filter.null.text(node.text));
    });

test_that(
    'filter.null.text handles valid text input', {
        num.valid.rows <- 3;

        node.text <- data.frame(
            node = 1:num.valid.rows,
            gene = rep('gene', num.valid.rows)
            );

        filtered.text <- filter.null.text(node.text);

        expect_equal(nrow(node.text), nrow(filtered.text));
    });

test_that(
    'filter.invalid.text.nodes removes invalid node IDs', {
        tree <- data.frame(
            parent = c(NA, 1, 1)
            );

        valid.node.id <- rownames(tree)[[1]];
        invalid.node.id <- -1;

        num.valid.rows <- 3;
        num.invalid.rows <- 2;

        node.text <- data.frame(
            node = c(
                rep(valid.node.id, num.valid.rows),
                rep(invalid.node.id, num.invalid.rows)
                ),
            gene = rep('gene', num.valid.rows + num.invalid.rows)
            );

        filtered.text <- suppressWarnings(filter.invalid.text.nodes(
            node.text,
            rownames(tree)
            ));

        expect_equal(nrow(filtered.text), num.valid.rows);
    });

test_that(
    'filter.invalid.text.nodes warns on invalid node IDs', {
        tree <- data.frame(
            parent = 1:2
            );

        node.text <- data.frame(
            node = c(-1)
            );

        expect_warning(filter.invalid.text.nodes(node.text, rownames(tree)));
    });

test_that(
    'filter.invalid.text.nodes handles valid node IDs', {
        tree <- data.frame(
            parent = c(1:3)
            );

        node.text <- data.frame(
            node = rownames(tree),
            gene = rep('gene', nrow(tree))
            );

        filtered.text <- filter.invalid.text.nodes(node.text, rownames(tree));

        expect_equal(nrow(filtered.text), nrow(node.text));
    });

test_that(
    'add.default.text.columns handles omitted colour data', {
        node.text <- data.frame(
            name = c('EXAMPLE', 'GENE', 'DATA')
            );

        result <- add.default.text.columns(node.text);

        expect_true(all(is.na(result$col)));
    });

test_that(
    'add.default.text.columns handles omitted fontface data' , {
        node.text <- data.frame(
            name = c('GENE', 'EXAMPLE')
            );

        result <- add.default.text.columns(node.text);

        expect_true(all(is.na(result$fontface)));
    });

test_that(
    'add.default.text.columns does not modify existing colour data' , {
        expected.col <- c('red', NA, 'blue');

        node.text <- data.frame(
            name = c('GENES', 'FOR', 'TESTING'),
            col = expected.col
            );

        result <- add.default.text.columns(node.text);

        expect_equal(result$col, expected.col);
    });

test_that(
    'add.default.text.columns does not modify existing fontface data' , {
        expected.style <- c(NA, 'plain', 'italic');

        node.text <- data.frame(
            name = c('GENES', 'FOR', 'TESTING'),
            fontface = expected.style
            );

        result <- add.default.text.columns(node.text);

        expect_equal(result$fontface, expected.style);
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
