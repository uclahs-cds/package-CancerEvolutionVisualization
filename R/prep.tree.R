prep.tree <- function(
    tree.df = NULL,
    cnas = NULL,
    snvs = NULL,
    pga.df = NULL,
    pga.percent = FALSE,
    bells = TRUE,
    normal.included = TRUE,
    axis.type = 'both',
    w.padding = NULL,
    colours = colours) {
    
    if (!('parent' %in% colnames(tree.df))) {
        stop('No parent column provided');
        }
    
    if (!('CP' %in% colnames(tree.df))) {
        stop('No CP column provided');
        }

    tree.df$parent <- prep.tree.parent(tree.df$parent);
    
    if (!check.parent.values(rownames(tree.df), tree.df$parent)) {
        stop('Parent column references invalid node');
        }
    
    tree.df$CP <- as.numeric(tree.df$CP);

    if (all(!is.na(tree.df$CP))) {
        tree.df <- reset.node.names(reorder.nodes(tree.df));
        }

    tree.df$child <- rownames(tree.df);

    out.df <- data.frame(
        lab = c(-1, tree.df$child),
        ccf = as.numeric(c(1, tree.df$CP)),
        color = colours[1:(nrow(tree.df) + 1)],
        parent = as.numeric(c(NA,tree.df$parent)),
        excluded = c(TRUE, rep(FALSE, nrow(tree.df))),
        bell = c(FALSE, rep(bells, nrow(tree.df))),
        alpha = rep(0.5, (nrow(tree.df) + 1)),
        stringsAsFactors = FALSE
        );

    if (!(is.null(pga.df))) {
        names.pga <- colnames(pga.df);

        if (!normal.included) {
            pga.df <- rbind(c(0, 0, 0, 'Normal', 1), pga.df);
            }

        pga.df$Node <- as.numeric(pga.df$Node);
        pga.df$PGA <- as.numeric(pga.df$PGA);

        if (pga.percent == TRUE) {
            pga.df$PGA <- pga.df$PGA / 100;
            } 

        pga.df$CP <- as.numeric(pga.df$CP);
        pga.df$Node[pga.df$Node == 0] <- -1;

        pga.df$Node <- order(-pga.df$CP);
        
        out.tree <- data.frame(
            parent = as.numeric(tree.df$parent),
            tip = as.numeric(tree.df$child),
            length1 = (pga.df$PGA[-1] * 100),
            length2 = as.numeric(tree.df$num_ssms),
            stringsAsFactors = FALSE
            );
    } else {
        out.tree <- data.frame(
            parent = as.numeric(tree.df$parent),
            tip = as.numeric(tree.df$child),
            length1 = as.numeric(tree.df$num_ssms),
            stringsAsFactors = FALSE
            );
        }
 
    genes.df <-  NULL

    if (!is.null(cnas) | !is.null(snvs)) {
        if (!is.null(cnas)) {
            cnas.df <- cnas[, c(2:4)];
            cnas.df <- cnas.df[which(!is.na(cnas.df$node)), ];
            }
    
        if (!is.null(snvs)) {
            snvs.df <- snvs[, c(2:3)];
            snvs.df$cn <- NA;
            snvs.df <- snvs.df[which(!is.na(snvs.df$node)), ];
            }

        if (!is.null(cnas) & !is.null(snvs)) {
            genes.df <- rbind(cnas.df, snvs.df);
        } else if (!is.null(snvs)) {
            genes.df <- snvs.df;
        } else if (!is.null(cnas)) {
            genes.df <- cnas.df;
            }

        genes.df <- genes.df[order(genes.df$node,genes.df$cn), ];
        }

    out.name <- paste0("SR_", axis.type, ".pdf");
    add.genes <- ifelse((is.null(genes.df) || nrow(genes.df) == 0 ), FALSE, TRUE);

    if (is.null(w.padding)) {
        w.padding <- 1.05;
    
        if(axis.type =="none") {
            w.padding <- 0.85;
            }
        }

    branching <- ifelse(any(duplicated(out.tree$parent) == TRUE), TRUE, FALSE);

    return(list(
        in.tree.df = out.df,
        tree = out.tree,
        genes.df = genes.df,
        out.name = out.name,
        w.padding = w.padding,
        branching = branching,
        add.genes = add.genes,
        axis.type = axis.type
        ));
    }

prep.tree.parent <- function(parent.column) {
    parent.column[parent.column %in% c(0, NA)] <- -1;
    return(parent.column);
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

reorder_clones <- function(in.df) {
    new.df <- in.df;

    new.df$new.lab <- order(-as.numeric(new.df$CP));
    new.df$new.par <- sapply(new.df$parent, function(x) {
        new.df$new.lab[new.df$child == x];
        }
    );

    in.df$child <- new.df$new.lab;
    in.df$parent <- new.df$new.par;
    in.df$parent[in.df$child == 1] <- -1;

    return(in.df);
    }

reorder.nodes <- function(tree.df) {
    return(tree.df[order(tree.df$CP, decreasing = TRUE), ]);
    }

reset.node.names <- function(tree.df) {
    new.names <- as.list(1:length(rownames(tree.df)));
    names(new.names) <- rownames(tree.df);

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
