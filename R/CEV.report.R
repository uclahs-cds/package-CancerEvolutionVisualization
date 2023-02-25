CEV.report <- function(
    phylogeny,
    SNV.assignment,
    SNV.counts,
    CCF.values,
    output.filename,
    title,
    author,
    date = NULL
    ) {
    phylogeny <- prep.phylogeny(phylogeny);
    SNV.assignment <- prep.SNV.assignment(SNV.assignment);
    SNV.counts <- prep.SNV.counts(SNV.counts);
    CCF.values <- prep.CCF.values(CCF.values);
    
    report.params <- list(
        title = title,
        author = author,
        date = if (!is.null(date)) date else Sys.Date()
        );

    rmarkdown::render(
        'inst/test.Rmd',
        output_file = output.filename,
        params = report.params
        );
    }
