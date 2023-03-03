prep.report <- function(
    phylogeny,
    SNV.assignment,
    SNV.counts,
    CCF.values
    ) {
    phylogeny <- prep.phylogeny(phylogeny);
    SNV.assignment <- prep.SNV.assignment(SNV.assignment);
    SNV.counts <- prep.SNV.counts(SNV.counts);
    CCF.values <- prep.CCF.values(CCF.values);
    
   return(list(
        phylogeny = phylogeny,
        SNV.assignment = SNV.assignment,
        SNV.counts = CCF.values,
        CCF.values = CCF.values
        ));
    }

prep.phylogeny <- function(phylogeny) {
    phylogeny.data.name <- "Phylogeny";

    if (!is.data.frame(phylogeny)) {
        stop(paste(phylogeny.data.name, 'input is not a data.frame.'));
        }

    check.column.exists(phylogeny, "clone.id", phylogeny.data.name);
    check.column.exists(phylogeny, "parent", phylogeny.data.name);

    return(phylogeny);
    }

prep.SNV.assignment <- function(SNV.assignment) {
    SNV.assignment.data.name <- "SNV Assignment";

    if (!is.data.frame(SNV.assignment)) {
        stop(paste(SNV.assignment.data.name, 'input is not a data.frame.'));
        }

    check.column.exists(SNV.assignment, 'snv.id', SNV.assignment.data.name);
    check.column.exists(SNV.assignment, 'clone.id', SNV.assignment.data.name);

    return(SNV.assignment);
    };

prep.SNV.counts <- function(SNV.counts) {
    SNV.count.data.name <- 'SNV Count';
    if (!is.data.frame(SNV.counts)) {
        stop(paste(SNV.count.data.name, 'input is not a data.frame.'));
        }

    check.column.exists(SNV.counts, 'clone.id', SNV.count.data.name);
    check.column.exists(SNV.counts, 'num.snv', SNV.count.data.name);
    check.column.exists(SNV.counts, 'CP', SNV.count.data.name);

    return(SNV.counts);
    };

prep.CCF.values <- function(CCF.values) {
    CCF.value.data.name <- 'CCF';

    if (!is.data.frame(CCF.values)) {
        stop(paste(CCF.value.data.name, 'input is not a data.frame.'));
        }

    check.column.exists(CCF.values, 'sample.id', CCF.value.data.name);
    check.column.exists(CCF.values, 'snv.id', CCF.value.data.name);
    check.column.exists(CCF.values, 'CCF', CCF.value.data.name);

    return(CCF.values);
    };
