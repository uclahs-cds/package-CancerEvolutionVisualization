load('data/multisample.test.data.Rda');

test_that('create.ccf.heatmap runs with valid data input', {
    mutation.CCF.data <- data.frame.to.array(multisample.result.data$mutation);

    CCF.heatmap <- create.ccf.heatmap(mutation.CCF.data);
    expect_is(CCF.heatmap, 'trellis');
    });

test_that('create.cluster.heatmap runs with valid data input', {
    CCF.heatmap <- create.cluster.heatmap(multisample.result.data$mutation);
    expect_is(CCF.heatmap, 'trellis');
    });

test_that('create.ccf.summary.heatmap runs with valid data input', {
    CCF.heatmap <- create.ccf.summary.heatmap(multisample.result.data$mutation);
    expect_is(CCF.heatmap, 'trellis');
    });
