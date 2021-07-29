#####################################################################################
# Helper functions for plotting confidence intervals and true values
# Lonni Besancon 2015--2019 CC-BY-SA
#####################################################################################

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("reshape2")) {
  install.packages("reshape2")
  library(reshape2)
}


plotCI <- function(data, ymin = 0, ymax = 1.0, xlab = "XLAB", ylab = "YLAB"){
  pd = position_dodge(.6)    ### How much to jitter the points on the plot
  g <- ggplot(data,                ### The data frame to use. 
              aes(x     = factor(y_axis_items),
                  y     = measure,
                  color = factor(legend_factor))) +
    
    geom_point(size  = 2, position = pd) +
    
    geom_errorbar(aes(ymin  = upperBound_CI,
                      ymax  = lowerBound_CI),
                  width = 0.2, 
                  size  = 0.7, 
                  position = pd) +
    coord_flip() +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),axis.title=element_text(size = rel(1.2), colour = "black"),axis.text=element_text(size = rel(1.2), colour = "black"),panel.grid.major = element_line(colour = "#DDDDDD"),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ theme(axis.title = element_text(face = "bold")) +
    xlab(xlab)+
    ylab(ylab) +
    guides(color = guide_legend(reverse = TRUE))
    
  print(g)
}


plotRatio <- function(data, ymin = 0, ymax = 1.0, xlab = "XLAB", ylab = "YLAB"){
  pd = position_dodge(.6)    ### How much to jitter the points on the plot
  g <- ggplot(data,                ### The data frame to use. 
              aes(x     = question,
                  y     = mean_time,
                  color = technique)) +
    
    geom_point(size  = 2, position = pd) +
    
    geom_errorbar(aes(ymin  = upperBound_CI,
                      ymax  = lowerBound_CI),
                  width = 0.2, 
                  size  = 0.7, 
                  position = pd) +
    coord_flip() +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),axis.title=element_text(size = rel(1.2), colour = "black"),axis.text=element_text(size = rel(1.2), colour = "black"),panel.grid.major = element_line(colour = "#DDDDDD"),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ theme(axis.title = element_text(face = "bold")) +
    xlab(xlab)+
    ylab(ylab)+
    geom_hline(yintercept = 1);
  
  
  print(g)
}



plotDiff <- function(data, ymin = 0, ymax = 1.0, xlab = "XLAB", ylab = "YLAB"){
  pd = position_dodge(.6)    ### How much to jitter the points on the plot
  g <- ggplot(data,                ### The data frame to use. 
              aes(x     = question,
                  y     = mean_time,
                  color = technique)) +
    
    geom_point(size  = 2, position = pd) +
    
    geom_errorbar(aes(ymin  = upperBound_CI,
                      ymax  = lowerBound_CI),
                  width = 0.2, 
                  size  = 0.7, 
                  position = pd) +
    coord_flip() +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),axis.title=element_text(size = rel(1.2), colour = "black"),axis.text=element_text(size = rel(1.2), colour = "black"),panel.grid.major = element_line(colour = "#DDDDDD"),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ theme(axis.title = element_text(face = "bold")) +
    xlab(xlab)+
    ylab(ylab)+
    geom_hline(yintercept = 0);
  
  
  print(g)
}