prep.tree <- function(
    samp = NULL,
    trees = NULL,
    cnas = NULL,
    snvs = NULL,
    pga = NULL,
    CF_col = "cellular_prevalence",
    pga.percent = FALSE,
    bells = TRUE,
    normal.included = TRUE,
    axis.type = 'both',
    w.padding = NULL,
    colours = colours) {

    if (!(samp %in% trees[, 1])){
        stop("Sample not found in trees file");
        }

    tree.df <- trees[trees[,1] == samp,];

    if (any(grepl("parent", tree.df[1, ]) == TRUE)) {
        colnames(tree.df) <- tree.df[1, ];
        colnames(tree.df)[1] <- "Sample";
        tree.df <- tree.df[c(2:nrow(tree.df)), ];
    } else if (any(grepl("parent",colnames(trees)) == TRUE)) {
        colnames(tree.df) <- colnames(trees);
        colnames(tree.df)[1] <- "Sample";
    } else {
        stop("No column names detected");
        }

    tree.df$parent[tree.df$parent == 0] <- -1;
    tree.df$cellular_prevalence <- as.numeric(tree.df[, CF_col]);

    if (all(!is.na(tree.df$cellular_prevalence))) {
        tree.df <- reorder_clones(tree.df);
        }

    out.df <- data.frame(
        lab = c(-1, tree.df$child),
        ccf = as.numeric(c(1, tree.df$cellular_prevalence)),
        color = colours[1:(nrow(tree.df) + 1)],
        parent = as.numeric(c(NA,tree.df$parent)),
        excluded = c(TRUE, rep(FALSE, nrow(tree.df))),
        bell = c(FALSE, rep(bells, nrow(tree.df))),
        alpha = rep(0.5, (nrow(tree.df) + 1)),
        stringsAsFactors = FALSE
        );

    if ("#FCF9BF" %in% out.df$color) {
        out.df$alpha[which(out.df$color == "#FCF9BF")] <- 0.8;
        }

    if (!(is.null(pga))) {
        if (!(samp %in% pga$Sample)) {
            stop("Sample not found in PGA file");
            }

        pga.df <- pga[pga$Sample == samp, ];
        names.pga <- colnames(pga);

        if (!normal.included) {
            pga.df <- rbind(c(samp, 0, 0, 0, 'Normal', 1), pga.df);
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

    if ((!is.null(cnas) | !is.null(snvs)) & ((samp %in% cnas$Sample ) | (samp %in% snvs$Sample ))) {
        if (!is.null(cnas) & (samp %in% cnas$Sample)) {
            cnas.df <- cnas[cnas$Sample == samp,c(2:4)];
            cnas.df <- cnas.df[which(!is.na(cnas.df$node)), ];
            }
    
        if (!is.null(snvs) & (samp %in% snvs$Sample)) {
            snvs.df <- snvs[snvs$Sample == samp,c(2:3)];
            snvs.df$cn <- NA;
            snvs.df <- snvs.df[which(!is.na(snvs.df$node)), ];
            }

        if (!is.null(cnas) & !is.null(snvs) & (samp %in% cnas$Sample) & (samp %in% snvs$Sample)) {
            genes.df <- rbind(cnas.df, snvs.df);
        } else if (!is.null(snvs) & (samp %in% snvs$Sample)) {
            genes.df <- snvs.df;
        } else if (!is.null(cnas) & (samp %in% cnas$Sample)) {
            genes.df <- cnas.df;
            }

        genes.df <- genes.df[order(genes.df$node,genes.df$cn), ];
        }

    out.name <- paste0("SR_", samp, "_", axis.type, ".pdf");
    samp.name <- samp;
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
        samp.name = samp.name,
        branching = branching,
        add.genes = add.genes,
        axis.type = axis.type
        ));
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

reorder_clones <- function(in.df){
  new.df <- in.df
  new.df$new.lab <- order(-as.numeric(new.df$cellular_prevalence))
  new.df$new.par <- sapply(new.df$parent, function(x){ return(new.df$new.lab[new.df$child==x])})
  in.df$child <- new.df$new.lab
  in.df$parent <- new.df$new.par
  in.df$parent[in.df$child == 1] <- -1
  return(in.df)
}