test.tree <- function(tree, cnas, snvs, sample, ...) {
    samp_pga <- tree[,c(1,3,9,6,10)]
    colnames(samp_pga) <- c('Sample','Node','PGA','CP','pipeline')
    samp_pga <- samp_pga[which(samp_pga$Node !='child'),]
    
    title_dist <- 0.905
    scale.x.real <- 1/30
    scale2 <- 0.5/1500
    snv.interval<- 1000
    pga.interval <-10
    extra.len <- 17
    
    inputs <- prep.tree(samp=sample,trees=tree, cnas=cnas,snvs=snvs,colours=colours,pga=samp_pga,pga.percent=TRUE, normal.included = FALSE)
    
    scale.x.real <- 1/20
    
    out <- SRCGrob(inputs$in.tree.df, inputs$tree, genes_df= inputs$genes.df,filename = NULL,scale2=scale2,wid=2, extra_len=.1,
                   scale1 = scale.x.real,  w_padding = inputs$w.padding, h_padding=1, rad=0.1, gene.cex=0.85, fixed_angle=pi/6,
                   seg1.col='navy', seg2.col='gold',  node_col="grey40", sig_curve=3, 
                   line.lwd=4, xaxis_space_left=0.1, xaxis_space_right=0.1, yaxis1_interval=10, min_width=1,
                   yaxis1_label="PGA",yaxis2_label="SNV", yaxis2_interval=1000,
                   spread=1.1, xaxis_label="CP" ,title=inputs$samp.name, title.cex=1.55, title.y=0.3, title.y.units="inches",
                   ...
                   );
    
    expect_true(!is.null(out[[1]]));
    }

test.sample <- function(sample, tree, cnas, snvs) {
    samp_tree <- trees_df[trees_df$Sample == sample, ];
    samp_cnas <- cnas_df[cnas_df$Sample == sample, ];
    samp_snvs <- snvs_df[snvs_df$Sample == sample, ];
    
    return(list(tree = samp_tree, cnas = samp_cnas, snvs = samp_snvs));
    }