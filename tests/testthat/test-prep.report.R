load('data/report.data.Rda');

test_that('prep.phylogeny errors on missing clone.id column', {
    invalid.phylogeny <- data.frame(clone.id = c(1));

    expect_error(
        prep.phylogeny(invalid.phylogeny),
        regexp = 'parent'
        );
    });

test_that('prep.phylogeny errors on missing parent column', {
    invalid.phylogeny <- data.frame(parent = c(1));

    expect_error(
        prep.phylogeny(invalid.phylogeny),
        regexp = 'clone.id'
        );
    });


test_that('prep.SNV.assignment errors on missing SNV.id column', {
    invalid.SNV.assignment <- data.frame(clone.id = c(1));

    expect_error(
        prep.SNV.assignment(invalid.SNV.assignment),
        regexp = 'SNV.id'
        );
    });

test_that('prep.SNV.assignment errors on missing clone.id column', {
    invalid.SNV.assignment <- data.frame(SNV.id = c(1));

    expect_error(
        prep.SNV.assignment(invalid.SNV.assignment),
        regexp = 'clone.id'
        );
    });

test_that('prep.SNV.counts errors on missing clone.id column', {
    invalid.SNV.counts <- data.frame(num.snv = c(1), CP = c(1));

    expect_error(
        prep.SNV.counts(invalid.SNV.counts),
        regexp = 'clone.id'
        );
    });

test_that('prep.SNV.counts errors on missing num.snv column', {
    invalid.SNV.counts <- data.frame(clone.id = c(1), CP = c(1));

    expect_error(
        prep.SNV.counts(invalid.SNV.counts),
        regexp = 'num.snv'
        );
    });

test_that('prep.SNV.counts errors on missing CP column', {
    invalid.SNV.counts <- data.frame(clone.id = c(1), num.snv = c(1));

    expect_error(
        prep.SNV.counts(invalid.SNV.counts),
        regexp = 'CP'
        );
    });

test_that('prep.CCF.values errors on missing sample.id column', {
    invalid.CCF.values <- data.frame(SNV.id = c(1), CCF = c(1));

    expect_error(
        prep.CCF.values(invalid.CCF.values),
        regexp = 'sample.id'
        );
    });

test_that('prep.CCF.values errors on missing SNV.id column', {
    invalid.CCF.values <- data.frame(sample.id = c(1), CCF = c(1));

    expect_error(
        prep.CCF.values(invalid.CCF.values),
        regexp = 'SNV.id'
        );
    });

test_that('prep.CCF.values errors on missing CCF column', {
    invalid.CCF.values <- data.frame(sample.id = c(1), SNV.id = c(1));

    expect_error(
        prep.CCF.values(invalid.CCF.values),
        regexp = 'CCF'
        );
    });

test_that('validate.clone.ids handles valid clone IDs', {
    input <- data.frame(clone.ids = c('A', 'B', 'C'));
    expect_silent(validate.clone.ids(input, input, input));
    });

test_that('validate.clone.ids handles invalid SNV assignment clone IDs', {
    reference.clone.ids <- c('B', 'A', 'B', 'C');
    invalid.clone.ids <- sapply(reference.clone.ids, function(x) paste0(x, 'B'));

    phylogeny <- SNV.counts <- data.frame(clone.ids = reference.clone.ids);
    SNV.assignment <- data.frame(clone.ids = invalid.clone.ids);

    expect_error(
        validate.clone.ids(phylogeny, SNV.assignment, SNV.counts), 'clone');
    });

test_that('validate.clone.ids handles invalid SNV count clone IDs', {
    reference.clone.ids <- c('1', '3', 'B', '4');
    invalid.clone.ids <- sapply(reference.clone.ids, function(x) paste0(x, 'A'));

    phylogeny <- SNV.assignment <- data.frame(clone.ids = reference.clone.ids);
    SNV.counts <- data.frame(clone.ids = invalid.clone.ids);

    expect_error(
        validate.clone.ids(phylogeny, SNV.assignment, SNV.counts), 'clone');
    });

test_that('create.report.tree.input result can create SRCGrob', {
    input <- create.report.summary.tree.input(phylogeny, SNV.counts);
    tree <- SRCGrob(input);

    expect_is(tree, 'SRCGrob');
    });

test_that('create.report.heatmap.input result can create CCF heatmap', {
    input <- data.frame.to.array(
        create.report.heatmap.input(SNV.assignment, CCF.values)
        );

    heatmap <- plot.ccf.hm(input);

    expect_is(heatmap, 'trellis');
    });

test_that('create.report.heatmap.input result can create summary heatmap', {
    input <- create.report.heatmap.input(SNV.assignment, CCF.values);
    heatmap <- plot.summary.ccf.hm(input);

    expect_is(heatmap, 'trellis');
    });

test_that('create.report.heatmap.input result can create cluster heatmap', {
    input <- create.report.heatmap.input(SNV.assignment, CCF.values);
    heatmap <- plot.cluster.hm(input);

    expect_is(heatmap, 'trellis');
    });
