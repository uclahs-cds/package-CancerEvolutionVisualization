filter.null.genes <- function(gene.df) {
    null.genes <- which(is.na(gene.df$node));

    if (length(null.genes) > 0) {
        warning('Genes with no node will not be used');

        return(gene.df[-(null.genes), ]);
    } else {
        return(gene.df);
        }
    }

filter.invalid.gene.nodes <- function(gene.df, node.ids) {
    invalid.genes <- which(as.logical(sapply(
        gene.df$node,
        FUN = function(node) {
            !(node %in% node.ids);
            }
        )));

    if (length(invalid.genes) > 0) {
        warning(paste(
            'Gene nodes provided that do not match a tree node ID.',
            'Invalid genes will not be used.'
            ));

        return(gene.df[-(invalid.genes), ]);
    } else {
        return(gene.df);
        }
    }


prep.genes <- function(genes.df, tree.rownames) {
    genes.df <- filter.null.genes(genes.df);

    genes.df <- filter.invalid.gene.nodes(
        genes.df,
        tree.rownames
        );

    genes.df <- add.default.gene.columns(genes.df);
    genes.df <- reorder.genes(genes.df);

    return(genes.df);
    }

add.default.gene.columns <- function(genes.df) {
    # Internal functions rely on all columns being present
    if (is.null(genes.df$CNA)) {
        genes.df$CNA <- NA;
        }

    if (is.null(genes.df$SNV)) {
        genes.df$SNV <- FALSE;
        }

    return(genes.df);
    }

reorder.genes <- function(genes.df) {
    return(genes.df[order(genes.df$node, genes.df$CNA, decreasing = TRUE), ]);
    }

prep.gene.line.dist <- function(gene.line.dist) {
    clamped <- FALSE;

    if (gene.line.dist < 0) {
        gene.line.dist <- 0;
        clamped <- TRUE;
    } else if (gene.line.dist > 1) {
        gene.line.dist <- 1;
        clamped <- TRUE;
        }

    if (clamped) {
        warning(paste(
            '"gene.line.dist" must be between 0 and 1.',
            paste('A value of', gene.line.dist, 'will be used') 
            ));
        }

    return(gene.line.dist);
    }
