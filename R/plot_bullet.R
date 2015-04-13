#' plot_bullet()
#' #'  Custom version of the classical Bullet Graph
#'  \url{http://en.wikipedia.org/wiki/Bullet_graph}
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
#' plot_bullet(df)

plot_bullet <- function(df,
                        my_title = 'Overview of JRT results for FY14\n',
                        my_palette = c('#B10318', '#DBA13A', '#309343'),
                        my_font = 'Impact'){
  # Data cleaning and transformation
  df$value_scaled <- round(df$value / df$n, 3)
  df$y_title <- paste(df$measure, '\nn =', df$n)
  df$target_met[df$value_scaled < df$target / 2] <- 'low'
  df$target_met[df$value_scaled >= df$target / 2 & df$value_scaled < df$target] <- 'medium'
  df$target_met[df$value_scaled >= df$target] <- 'high'
  df$target_met <- ordered(df$target_met)
  df$target_met <- ordered(df$target_met, levels = c("low", "medium", "high"))

  # Custom constant for plot
  my_width <- .5
  my_title <- 'Overview of JRT results for FY14\n'
  my_palette <- c('#B10318', '#DBA13A', '#309343')

  # Bullet plot
  p <- ggplot(df)
  p <- p + geom_bar(aes(x = reorder(y_title, n), y = 1, fill = target_met),
                    stat = "identity", width = my_width, alpha = .6)
  p <- p + geom_errorbar(aes(x=y_title, y = target, ymin = target, ymax = target),
                         color = "#60636A", width = my_width + .05, size = 3,
                         position = position_dodge(width = 0.2))
  p <- p + geom_point(aes(y_title, value_scaled), color = "black",  stat = "identity", size = 8)
  p <- p + scale_fill_manual(values = my_palette)
  p <- p + scale_y_continuous(label = scales::percent_format(), expand = c(0, 0))
  p <- p + scale_x_discrete(expand = c(0, 0))
  p <- p + coord_flip()
  p <- p + ggtitle(my_title)
  p <- p + laycUtils::theme_layc(font_family = my_font)
  p <- p + theme(panel.border = element_blank(),
                 plot.title = element_text(size = 20, face = 'bold'),
                 axis.title = element_blank(),
                 axis.text.y = element_text(size = 15, face = 'bold'),
                 axis.text.x = element_text(size = 15, face = 'bold'),
                 axis.ticks.y = element_blank(),
                 legend.position = 'bottom')


  return(p)
}
