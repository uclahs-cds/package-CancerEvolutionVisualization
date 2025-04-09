test_that(
    'Linear case values', {
        load('data/linear.plots.Rda');
        load('data/linear.data.Rda')

        result.tree <- SRCGrob(
            linear.test.data$tree,
            linear.test.data$node.text,
            main = 'WHO003',
            node.text.cex = 0.85,
            scale1 = 0.9,
            yaxis1.label = 'PGA',
            yaxis2.label = 'SNV',
            xaxis.label = 'CP',
            main.cex = 1.55,
            main.y = 0.3,
            size.units = 'inches',
            horizontal.padding = -1,
            add.normal = TRUE,
            );
        expect_true(compare.trees(linear.example, result.tree));
        }
    );

test_that(
    'Linear 30-degree plotting direction case values', {
        load('data/linear.plots.Rda');
        load('data/linear.data.Rda')
        
        result.tree <- SRCGrob(
            linear.test.data$tree[, c('parent', 'length.1', 'length.2')],
            yaxis2.label = '',
            plotting.direction = 30
            );
            expect_true(compare.trees(linear.30deg.example, result.tree));
        }
    );
