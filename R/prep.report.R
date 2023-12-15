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

    validate.clone.ids(phylogeny, SNV.assignment, SNV.counts);

    summary.tree.input <- create.report.summary.tree.input(phylogeny, SNV.counts);
    heatmap.input <- create.report.heatmap.input(SNV.assignment, CCF.values);

    return(list(
        summary.tree.input = summary.tree.input,
        heatmap.input = heatmap.input
        ));
    }

prep.phylogeny <- function(phylogeny) {
    phylogeny.data.name <- 'Phylogeny';

    if (!is.data.frame(phylogeny)) {
        stop(paste(phylogeny.data.name, 'input is not a data.frame.'));
        }

    check.column.exists(phylogeny, 'clone.id', phylogeny.data.name);
    check.column.exists(phylogeny, 'parent', phylogeny.data.name);

    return(phylogeny);
    }

prep.SNV.assignment <- function(SNV.assignment) {
    SNV.assignment.data.name <- 'SNV Assignment';

    if (!is.data.frame(SNV.assignment)) {
        stop(paste(SNV.assignment.data.name, 'input is not a data.frame.'));
        }

    check.column.exists(SNV.assignment, 'SNV.id', SNV.assignment.data.name);
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
    check.column.exists(CCF.values, 'SNV.id', CCF.value.data.name);
    check.column.exists(CCF.values, 'CCF', CCF.value.data.name);

    return(CCF.values);
    };

validate.clone.ids <- function(
    phylogeny,
    SNV.assignment,
    SNV.counts
    ) {

    reference.clone.ids <- unique(phylogeny$clone.id);

    get.clone.error.message <- function(input.name) {
        return(paste(input.name, 'clone IDs do not match phylogeny clone IDs.'))
        }

    if (!column.contains.all(reference.clone.ids, SNV.assignment$clone.id)) {
        stop(get.clone.error.message('SNV Assignment'));
        }

    if (!column.contains.all(reference.clone.ids, SNV.counts$clone.id)) {
        stop(get.clone.error.message('SNV Count'))
        }
    }

create.report.summary.tree.input <- function(phylogeny, SNV.counts) {
    clone.ids <- phylogeny$clone.id;
    rownames(SNV.counts) <- SNV.counts$clone.id;

    return(data.frame(
       node.id = clone.ids,
       parent = phylogeny$parent,
       length1 = SNV.counts[clone.ids, 'num.snv']
        ));
    }

create.report.heatmap.input <- function(SNV.assignment, CCF.values) {
    rownames(SNV.assignment) <- SNV.assignment$SNV.id;

    return(data.frame(
        ID = CCF.values$sample.id,
        SNV.id = CCF.values$SNV.id,
        CCF = CCF.values$CCF,
        clone.id = SNV.assignment[CCF.values$SNV.id, 'clone.id']
        ));
    }
