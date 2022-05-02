prep.tree <- function(
    tree.df,
    genes.df,
    bells = TRUE,
    axis.type = 'left',
    w.padding = NULL,
    colour.scheme = colours) {
    
    if (!('parent' %in% colnames(tree.df))) {
        stop('No parent column provided');
        }

    tree.df$parent <- prep.tree.parent(tree.df$parent);

    if (!check.parent.values(rownames(tree.df), tree.df$parent)) {
        stop('Parent column references invalid node');
        }
    
        if (!is.null(genes.df)) {
        genes.df <- filter.null.genes(genes.df);

        genes.df <- filter.invalid.gene.nodes(
            genes.df,
            rownames(tree.df)
            );

        genes.df <- genes.df[order(genes.df$node,genes.df$cn), ];
        }

    if (!is.null(tree.df$CP)) {
        tree.df$CP <- suppressWarnings(as.numeric(tree.df$CP));

        if (all(!is.na(tree.df$CP))) {
            tree.df <- reset.node.names(reorder.nodes(tree.df));
        } else {
            warning(paste(
                'Non-numeric values found in CP column.',
                'Cellular prevalence will not be used.'
                ));

            tree.df$CP <- NULL;
            }
        }

    tree.df$child <- rownames(tree.df);
    
    tree.df$label <- as.character(
        if (is.null(tree.df$label)) { tree.df$child } else { tree.df$label }
        );

    out.df <- data.frame(
        id = c(-1, tree.df$child),
        label.text = c('', tree.df$label),
        ccf = if (is.null(tree.df$CP)) { NA } else { c(1, tree.df$CP) },
        color = colour.scheme[1:(nrow(tree.df) + 1)],
        parent = as.numeric(c(NA,tree.df$parent)),
        excluded = c(TRUE, rep(FALSE, nrow(tree.df))),
        bell = c(FALSE, rep(bells, nrow(tree.df))),
        alpha = rep(0.5, (nrow(tree.df) + 1)),
        stringsAsFactors = FALSE
        );

    out.tree <- data.frame(
        parent = as.numeric(tree.df$parent),
        tip = as.numeric(tree.df$child),
        prep.branch.lengths(tree.df)
        );

    if (is.null(w.padding)) {
        w.padding <- 1.05;
    
        if (axis.type == "none") {
            w.padding <- 0.85;
            }
        }

    branching <- ifelse(any(duplicated(out.tree$parent) == TRUE), TRUE, FALSE);

    return(list(
        in.tree.df = out.df,
        tree = out.tree,
        genes.df = genes.df,
        w.padding = w.padding,
        branching = branching
        ));
    }

prep.tree.parent <- function(parent.column) {
    parent.column[parent.column %in% c(0, NA)] <- -1;
    return(parent.column);
    }

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

process_1C <- function(out_1C){
  in.df <- read.table(out_1C, header=FALSE)
  colnames(in.df)[1:2] <- c("tip","length1")
  if(ncol(in.df) == 3){
    colnames(in.df)[3] <- "ccf"
  }
  return(in.df)
}

process_3A <- function(out_3A){
  in.df <- read.table(out_3A, header=FALSE)
  out.df <- data.frame(parent = in.df$V2, tip = in.df$V1)
  out.df$parent[out.df$parent==0] <- -1
  if(ncol(in.df)==3){
    out.df$plot.lab <- in.df$V3
  }
  return(out.df[order(out.df$tip),])
}

process_2A <- function(truth=NULL, pred=NULL){
  pred <- scan(pred, what="numeric")
  true <- scan(truth, what="numeric")
  out2A <- data.frame(ssm=seq_along(pred), truth=true, pred=pred)
  origins <- dlply(out2A, .(pred), function(x) {props=table(x$truth)/nrow(x); props[props!=0]})
  names(origins) <- paste0("N", names(origins))
  return(origins)
}

reorder.nodes <- function(tree.df) {
    if (any(!is.na(tree.df$CP))) {
        tree.df <- reorder.nodes.by.CP(tree.df);
        }

    return(reorder.trunk.node(tree.df));
    }

reorder.nodes.by.CP <- function(tree.df) {
    return(tree.df[order(-(tree.df$CP), tree.df$parent), ]);
    }

reorder.trunk.node <- function(tree.df) {
    is.trunk <- is.na(tree.df$parent);

    # Skip reindexing data.frame if trunk node is already first
    if (!is.trunk[[1]]) {
        tree.df[c(which(is.trunk), which(!is.trunk)), ];
    } else {
        tree.df;
        }
    }

reset.node.names <- function(tree.df) {
    new.names <- get.value.index(
        old.values = rownames(tree.df),
        new.values = 1:nrow(tree.df)
        );

    rownames(tree.df) <- new.names;

    # Include -1 value for root node.
    # This may be temporary, as NULL/NA will likely replace -1
    new.names['-1'] <- -1;

    # Convert parent values to character to safely index names list
    tree.df$parent <- as.numeric(unlist(new.names[as.character(tree.df$parent)]));

    return(tree.df);
    }

check.parent.values <- function(node.names, parent.col) {
    unique.node.names <- as.list(setNames(
        !vector(length = length(unique(node.names))),
        unique(node.names)
        ));

    all(sapply(
        parent.col,
        FUN = function(parent) {
            !is.null(unlist(unique.node.names[parent])) | parent == -1;
            }
        ));
    }
