test_that(
    'prep.tree handles invalid parent column', {
    invalid.tree <- data.frame();
    
    expect_error(
        prep.tree(tree.df = invalid.tree),
        regexp = 'parent'
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
