get.colours <- function(
    value.list,
    return.names = FALSE
    ) {
    colours <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)];
    n <- length(unique(value.list));

    col.list <- sample(colours, n);
    if (is.null(levels(value.list))) {
        value.list <- factor(value.list, levels = unique(value.list))
        }
    names(col.list) <- levels(value.list);
    if (return.names) {
        return(col.list);
    } else {
        return(col.list[value.list]);
        }
    }

get.colour.luminance <- function(colour) {
    # Formulas and values documented in:
    # https://www.w3.org/WAI/GL/wiki/Relative_luminance
    sRGB.values <- col2rgb(colour) / 255;
    sRGB.values <- sapply(
        sRGB.values,
        FUN = function(sRGB.value) {
            if (sRGB.value <= 0.03928) {
                return(sRGB.value / 12.92);
            } else {
                return(((sRGB.value + 0.055 ) / 1.055) ** 2.4);
                }
            }
        );

    luminance.modifiers <- c(0.2126, 0.7152, 0.0722);
    luminance <- sum(sRGB.values * luminance.modifiers);

    return(luminance);
    }

get.contrast.ratio <- function(luminance1, luminance2) {
    luminance <- sort(
        c(luminance1, luminance2),
        decreasing = TRUE
        );
    luminance <- luminance + 0.05;

    contrast.ratio <- luminance[1] / luminance[2];
    return(contrast.ratio);
    }
