#' plot_singbar()
#' #'  Custom version of the classical Bullet Graph
#'  \url{http://en.wikipedia.org/wiki/Bullet_graph}
#'  This function plots only the body of a single bullet chart: No text
#'
#' @param df:  data.frame with 1 rows and the following 7 columns
#'                     measure  : label of what's being measured
#'                     target   : the target value for the measure
#'                     value    : the actual value of the measure
#'                     n        : Sample size
#' @return ggplot object
#' @export
#' @import ggplot2
#' @examples
#' df <- data.frame(measure = c("% enrolled", "% assessed", "% gaining skills"),
#' target=c(.75, .9, .72),
#' value=c(232, 50, 36),
#' n = c(407, 232, 50))
#'
#' plot_singbar(df[1, ])

plot_singbar <- function(df,
                        my_palette = c('low' = '#B10318', 'medium' = '#DBA13A', 'high' = '#309343'),
                        my_font = 'Impact',
                        my_background = '#E2E2E3'){

    # Custom constant for plot
  my_width <- .6

  # Bullet plot
  p <- ggplot(df)
  p <- p + geom_bar(aes(x = y_title, y = 1),
                    stat = "identity", width = my_width, alpha = .6, fill = 'white')
  p <- p + geom_bar(aes(x = y_title, y = value_scaled, fill = target_met),
                    stat = "identity", width = my_width, alpha = .6)
  p <- p + geom_errorbar(aes(x=y_title, y = target, ymin = target, ymax = target),
                         color = "black", width = my_width, size = rel(1))
  p <- p + geom_text(aes(y_title, value_scaled + 0.08, label = paste0(value_scaled*100, '%')),
                     stat = "identity", size = rel(4), family = my_font)
  p <- p + scale_fill_manual(name = "Target met", values = my_palette, drop = FALSE)
  p <- p + scale_y_continuous(label = scales::percent_format(), expand = c(0, 0))
  p <- p + scale_x_discrete(expand = c(0, 0))
  p <- p + coord_flip()
  p <- p + laycUtils::theme_layc(base_family = my_font)
  p <- p + theme(panel.border = element_blank(),
                 plot.margin = grid::unit(c(0, 0, 0, 0), 'lines'),
                 plot.margin = grid::unit(c(0, 0, 0, 0), 'lines'),
                 plot.title = element_text(size = rel(1.5), face = 'bold'),
                 axis.title = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 legend.position = 'none',
                 panel.background = element_rect(fill = my_background),
                 plot.background = element_rect(fill = my_background, colour = my_background))
  return(p)
}
