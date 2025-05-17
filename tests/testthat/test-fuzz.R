test_that(
    'randomize.tree errors on invalid angle randomization type', {
        expect_error(
            {
                randomize.tree(
                    data.frame(parent = c(NA, 1)),
                    randomize.angle = 'test'
                    );
                },
            regexp = 'randomize.angle'
            );
        }
    );

test_that(
    'randomize.tree errors on invalid plotting direction randomization type', {
        expect_error(
            {
                randomize.tree(
                    data.frame(parent = c(NA, 1)),
                    randomize.plotting.direction = 'test'
                    );
                },
            regexp = 'plotting.direction'
            );
        }
    );

test_that(
    'randomize.tree errors on invalid node color randomization type', {
        expect_error(
            {
                randomize.tree(
                    data.frame(parent = c(NA, 1)),
                    randomize.node.color = 'test'
                    );
                },
            regexp = 'randomize.node.color'
            );
        }
    );

test_that(
    'randomize.tree errors on invalid border color randomization type', {
        expect_error(
            {
                randomize.tree(
                    data.frame(parent = c(NA, 1)),
                    randomize.border.color = 'test'
                    );
                },
            regexp = 'randomize.border.color'
            );
        }
    );

test_that(
    'randomize.tree errors on invalid border width randomization type', {
        expect_error(
            {
                randomize.tree(
                    data.frame(parent = c(NA, 1)),
                    randomize.border.width = 'test'
                    );
                },
            regexp = 'randomize.border.width'
            );
        }
    );

test_that(
    'randomize.tree errors on invalid border type randomization type', {
        expect_error(
            {
                randomize.tree(
                    data.frame(parent = c(NA, 1)),
                    randomize.border.type = 'test'
                    );
                },
            regexp = 'randomize.border.type'
            );
        }
    );

test_that(
    'randomize.tree errors on invalid edge color randomization type', {
        expect_error(
            {
                randomize.tree(
                    data.frame(parent = c(NA, 1)),
                    randomize.edge.col = 'test'
                    );
                },
            regexp = 'randomize.edge.col'
            );
        }
    );

test_that(
    'randomize.tree errors on invalid edge width randomization type', {
        expect_error(
            {
                randomize.tree(
                    data.frame(parent = c(NA, 1)),
                    randomize.edge.width = 'test'
                    );
                },
            regexp = 'randomize.edge.width'
            );
        }
    );


test_that(
    'randomize.tree errors on invalid edge length randomization type', {
        expect_error(
            {
                randomize.tree(
                    data.frame(parent = c(NA, 1)),
                    randomize.edge.length = 'test'
                    );
                },
            regexp = 'randomize.edge.length'
            );
        }
    );

test_that(
    'randomize.tree errors on invalid edge type randomization type', {
        expect_error(
            {
                randomize.tree(
                    data.frame(parent = c(NA, 1)),
                    randomize.edge.type = 'test'
                    );
                },
            regexp = 'randomize.edge.type'
            );
        }
    );
