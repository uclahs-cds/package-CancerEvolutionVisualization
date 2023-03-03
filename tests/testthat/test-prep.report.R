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
        regexp = 'snv.id'
        );
    });

test_that('prep.SNV.assignment errors on missing clone.id column', {
    invalid.SNV.assignment <- data.frame(snv.id = c(1));
    
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
    invalid.CCF.values <- data.frame(snv.id = c(1), CCF = c(1));
    
    expect_error(
        prep.CCF.values(invalid.CCF.values),
        regexp = 'sample.id'
        );
    });

test_that('prep.CCF.values errors on missing snv.id column', {
    invalid.CCF.values <- data.frame(sample.id = c(1), CCF = c(1));
    
    expect_error(
        prep.CCF.values(invalid.CCF.values),
        regexp = 'snv.id'
        );
    });

test_that('prep.CCF.values errors on missing CCF column', {
    invalid.CCF.values <- data.frame(sample.id = c(1), snv.id = c(1));
    
    expect_error(
        prep.CCF.values(invalid.CCF.values),
        regexp = 'CCF'
        );
    });
