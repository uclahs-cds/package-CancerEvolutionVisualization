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
    x.axis = 'SNV.id',
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

validate.data.frame.columns <- function(x, expected.columns) {
    missing.columns <- !(expected.columns %in% colnames(x));

    if (any(missing.columns)) {
        stop(paste(
            'Columns missing from input data:',
            paste(expected.columns[which(missing.columns)], collapse = ', ')
            ));
        }
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

get.encoded.distance <- function(points) {
    if (!is.data.frame(points)) {
        stop(paste(
            'Input data \"points\" must be a data.frame',
            paste0('(received \"", class(points), "\").')
            ));
        }
    validate.data.frame.columns(points, expected.columns = c('x', 'y'));

    # Line calculations will not be valid with a single point
    if (nrow(points) < 2) {
        points$encoded.distance <- 0;
        return(points);
        }

    coords <- as.matrix(points);
    line.direction <- matrix(c(1, 1));

    reference.point <- coords[1, ];

    point.vectors <- coords - reference.point;
    dot.products <- point.vectors %*% line.direction;
    projections <- dot.products %*% t(line.direction);
    distances.along.line <- sqrt(rowSums((point.vectors - projections)^2));
    encoded.distances <- ifelse(dot.products >= 0, distances.along.line, -distances.along.line);

    return(encoded.distances);
    }

check.file.extension <- function(
    filename,
    supported.ext,
    default.ext
    ) {

    ext <- tools::file_ext(filename);
    if (!(ext %in% supported.ext)) {
        message(paste(
            'File extension', ext, 'is not in the list of supported extensions. Switching to default extension', default.ext
            ));
        filename <- gsub(paste0('.', ext), paste0('.', default.ext), filename);
        ext <- default.ext;
        }
    return(list(name = filename, ext = ext));
    }

save.plot <- function(
    grob,
    filename,
    units,
    res,
    bg,
    ...
    ) {

    supported.extensions <- c('png', 'pdf', 'tiff', 'svg');
    f <- check.file.extension(filename, supported.extensions, 'tiff');
    if (f$ext == "png") {
        png(file = f$name, units = units, res = res, bg = bg...);
    } else if (f$ext == "tiff") {
        tiff(file = f$name, units = units, res = res, bg = bg, ...);
    } else if (f$ext == "pdf") {
        pdf(file = f$name, ...);
    } else if (f$ext == "svg") {
        svg(file = f$name, bg = bg, ...);
        }

    grid.newpage();
    grid.draw(grob);
    dev.off();
    };