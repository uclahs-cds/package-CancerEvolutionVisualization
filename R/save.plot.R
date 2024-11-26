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
    if (f$ext == 'png') {
        png(file = f$name, units = units, res = res, bg = bg, ...);
    } else if (f$ext == 'tiff') {
        tiff(file = f$name, units = units, res = res, bg = bg, ...);
    } else if (f$ext == 'pdf') {
        pdf(file = f$name, ...);
    } else if (f$ext == 'svg') {
        svg(file = f$name, bg = bg, ...);
        }

    grid.newpage();
    grid.draw(grob);
    dev.off();
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
