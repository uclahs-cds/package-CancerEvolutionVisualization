test_that('create.clone.genome.distribution.plot enforces data.frame columns', {
    snv.df <- data.frame();
    expect_error(
        create.clone.genome.distribution.plot(snv.df),
        'must contain the columns chr, pos, and clone.id; snv.df is missing chr, pos, and clone.id.'
    );
    snv.df <- data.frame(chr = 1);
    expect_error(
        create.clone.genome.distribution.plot(snv.df),
        'must contain the columns chr, pos, and clone.id; snv.df is missing pos and clone.id.'
    );
    snv.df <- data.frame(chr = 1, pos = 1);
    expect_error(
        create.clone.genome.distribution.plot(snv.df),
        'must contain the columns chr, pos, and clone.id; snv.df is missing clone.id.'
    );
    snv.df <- data.frame(clone.id = 1);
    expect_error(
        create.clone.genome.distribution.plot(snv.df),
        'must contain the columns chr, pos, and clone.id; snv.df is missing chr and pos.'
    );
});
