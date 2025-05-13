data.frame.to.array <- function(
    DF,
    value = 'CCF',
    x.axis = 'SNV.id',
    y.axis = 'ID'
    ) {

    required.cols <- c(value, x.axis, y.axis);
    missing.cols <- required.cols[!(required.cols %in% names(DF))];
    if (length(missing.cols) != 0) {
        stop(paste0(
            'Dataframe must contain the columns: ',
            oxford.comma.vector.concat(required.cols),
            '; Dataframe is missing ',
            oxford.comma.vector.concat(missing.cols, paste(required.cols, collapse = ', ')), '.'));
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
