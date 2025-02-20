gradient.color.scheme <- function(color.scheme, gradient.length) {
    if (length(color.scheme) == 1) {
        palette.ramp <- colorRampPalette(c('white', color.scheme, 'black'))
        gradient <- palette.ramp(gradient.length + 2)[2:(gradient.length + 1)];
    } else {
        palette.ramp <- colorRampPalette(color.scheme);
        gradient <- palette.ramp(gradient.length);
        }
    return(gradient);
    }
