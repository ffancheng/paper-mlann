# Add global axis label to a patchwork object
# Modified from https://github.com/thomasp85/patchwork/issues/43#issuecomment-692781479
add_global_label <- function(pwobj, Xlab = NULL, Ylab = NULL, Xgap = 0.06, Ygap = 0.04, remove.xlabs = TRUE, remove.ylabs = TRUE, ...) {
  ylabgrob <- patchwork::plot_spacer()
  if (!is.null(Ylab)) {
    ylabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Ylab, angle = 90, ...) +
      theme_void()
    if(remove.ylabs) pwobj <- pwobj & labs(y="")
  }
  if (!is.null(Xlab)) {
    xlabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Xlab, ...) +
      theme_void()
    if(remove.xlabs) pwobj <- pwobj & labs(x=NULL)
  }
  if (!is.null(Ylab) & is.null(Xlab)) {
    return((ylabgrob + patchworkGrob(pwobj)) + 
             patchwork::plot_layout(widths = 100 * c(Ygap, 1 - Ygap)))
  }
  if (is.null(Ylab) & !is.null(Xlab)) {
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = c(0, 100),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  if (!is.null(Ylab) & !is.null(Xlab)) {
    if(remove.xlabs) pwobj <- pwobj & labs(x=NULL)
    if(remove.ylabs) pwobj <- pwobj & labs(y="")
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = 100 * c(Ygap, 1 - Ygap),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  return(pwobj)
}


# # example 
# require(ggplot2)
# require(patchwork)
# p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
# p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
# p3 <- ggplot(mtcars) + geom_bar(aes(gear)) + facet_wrap(~cyl)
# p4 <- ggplot(mtcars) + geom_bar(aes(carb))
# p5 <- ggplot(mtcars) + geom_violin(aes(cyl, mpg, group = cyl))
# 
# add_global_label((p1 + p2 + p3 + p4 + p5),
#                  Ylab = "Global Y title",
#                  remove.ylabs = FALSE
#                  #       size = 8,
#                  #      Ygap = 0.04
# )
# # or use pipe
# require(magrittr)
# (p1 + p2 + p3 + p4 + p5) %>%
#   add_global_label(Xlab = "Global X title",
#                    remove.xlabs = FALSE
#                    #       size = 8,
#                    #      Ygap = 0.04
#   )
