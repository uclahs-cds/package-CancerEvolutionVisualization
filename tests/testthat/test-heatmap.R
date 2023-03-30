load('data/multisample.test.data.Rda');

test_that('plot.ccf.hm runs with valid data input', {
    mutation.CCF.data <- data.frame.to.array(multisample.result.data$mutation);

    CCF.heatmap <- plot.ccf.hm(mutation.CCF.data);
    expect_is(CCF.heatmap, 'trellis');
    });

test_that('plot.cluster.hm runs with valid data input', {
    CCF.heatmap <- plot.cluster.hm(multisample.result.data$mutation);
    expect_is(CCF.heatmap, 'trellis');
    });

test_that('plot.summary.ccf.hm runs with valid data input', {
    CCF.heatmap <- plot.summary.ccf.hm(multisample.result.data$mutation);
    expect_is(CCF.heatmap, 'trellis');
    });
