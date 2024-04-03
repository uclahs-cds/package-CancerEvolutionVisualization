get.value.index <- function(old.values, new.values) {
    if (length(old.values) != length(new.values)) {
        stop(paste(
            'New value length is not compatible with old values.',
            'Value index cannot be created.'
            ));
        }

    value.index <- as.list(new.values);
    names(value.index) <- old.values;

    return(value.index);
    }

reindex.column <- function(column.values, new.value.index) {
    return(unlist(
        new.value.index[as.character(column.values)],
        use.names = FALSE
        ));
    }

data.frame.to.array <- function(
    DF,
    value = 'CCF',
    x.axis = 'snv.id',
    y.axis = 'ID'
    ) {

    if (is.null(DF[[x.axis]]) | is.null(DF[[value]]) | is.null(DF[[y.axis]])) {
        stop(paste('Dataframe does not contain one of the columns:', value, x.axis, y.axis));
        }
    arr <- reshape(
        data = DF[, c(x.axis, y.axis, value)],
        v.names = value,
        timevar = y.axis,
        idvar = x.axis,
        direction = 'wide'
        );

    # set x.axis as rownames
    rows            <- arr[, 1];
    cols            <- gsub(paste0(value, '.'), '', names(arr)[-1]);
    arr             <- as.matrix(arr[, (-1)]);
    rownames(arr)   <- rows;
    colnames(arr)   <- cols;
    arr[is.na(arr)] <- 0;

    if (!is.null(levels(DF[, y.axis])) & ncol(arr) > 1) {
        arr <- arr[, levels(DF[, y.axis])];
        }

    return(arr);
    }

# conver chr coordinates to genome position
get.genome.pos <- function(
    snv.df,
    genome.build = 'GRCh37',
    chr.order = c(1:22, 'X', 'Y')
    ) {

    if (!(genome.build %in% c('GRCh37', 'GRCh38'))) {
        stop('genome.build must be either GRCh37 or GRCh38')
        }
    assign('chr.info', get(genome.build));

    snv.df$chr      <- droplevels(factor(snv.df$chr, levels = chr.order));
    chr.info        <- chr.info[chr.info$chr %in% levels(snv.df$chr), c('chr', 'length')];
    chr.info$chr    <- droplevels(factor(chr.info$chr, levels = chr.order));
    chr.info$length <- as.numeric(chr.info$length);
    chr.info$start  <- c(0, cumsum(chr.info$length[-length(chr.info$length)]));

    snv.df$genome.pos <- chr.info[match(snv.df$chr, chr.info$chr), 'start'] + as.integer(snv.df$pos);

    return(list('snv' = snv.df, 'chr.info' = chr.info));
    }

degrees.to.radians <- function(degrees) {
    return(degrees * pi / 180);
    }
