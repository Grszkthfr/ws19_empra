# generate random color
# some colors seem to be not html/icon::wahtever compatible!
# FUTURE: https://stackoverflow.com/questions/1754211/evaluate-whether-a-hex-value-is-dark-or-light#1754281
random_color <- function(n_colors=1){
    colors = colors()

    sample(
        colors[!grepl(
            x=colors,
            pattern="[0-9]+|grey|gray|black|white")],
        n_colors)
}
