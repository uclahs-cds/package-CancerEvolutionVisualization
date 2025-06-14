test_that('data.frame.to.array enforces data.frame columns', {
    DF <- data.frame();
    expect_error(
        data.frame.to.array(DF),
        'Dataframe must contain the columns: CCF, SNV.id, and ID; Dataframe is missing CCF, SNV.id, and ID.'
    );
    DF <- data.frame(CCF = 1);
    expect_error(
        data.frame.to.array(DF),
        'Dataframe must contain the columns: CCF, SNV.id, and ID; Dataframe is missing SNV.id and ID.'
    );
    DF <- data.frame(CCF = 1, SNV.id = 1);
    expect_error(
        data.frame.to.array(DF),
        'Dataframe must contain the columns: CCF, SNV.id, and ID; Dataframe is missing ID.'
    );
    DF <- data.frame(SNV.id = 1);
    expect_error(
        data.frame.to.array(DF),
        'Dataframe must contain the columns: CCF, SNV.id, and ID; Dataframe is missing CCF and ID.'
    );
});
