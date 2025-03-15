#' Custom ggplot2 theme
#'
#' 
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' iris |>
#'  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
#'  geom_point() +
#'  my_theme()
usethis::use_package("ggplot2")

my_theme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        text = ggplot2::element_text(family = "Arial")
    )
}