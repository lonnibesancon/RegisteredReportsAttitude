################################################################
# HelperFunctionsPlot.R
# Code for plotting confidence intervals.
# "Publishing Visualization Studies as Registered Reports: Expected Benefits and Researchers’ Attitudes"
# CC-BY Lonni Besançon et al., 2021
# See https://osf.io/4nrma/
################################################################


plotCI <- function(data, ymin = 0, ymax = 1.0, xlab = "XLAB", ylab = "YLAB") {
  pd <- position_dodge(.6) ### How much to jitter the points on the plot
  g <- ggplot(
    data, ### The data frame to use.
    aes(
      x = y_axis_items,
      y = measure,
      color = legend_factor
    )
  ) +
    geom_point(size = 2, position = pd) +
    geom_errorbar(aes(
      ymin = upperBound_CI,
      ymax = lowerBound_CI
    ),
    width = 0.2,
    size = 0.7,
    position = pd
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme(panel.background = element_rect(fill = "white", colour = "white"), 
          axis.title = element_text(size = rel(1.2), colour = "black"), 
          axis.text = element_text(size = rel(1.2), colour = "black"), 
          panel.grid.major = element_line(colour = "#DDDDDD"), 
          panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    theme(axis.title = element_text(face = "bold")) +
    xlab(xlab) +
    ylab(ylab) +
    guides(color = guide_legend(reverse = TRUE));
  
  g
}