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
