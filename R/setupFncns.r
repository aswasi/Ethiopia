library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(RColorBrewer)
library(animation)
library(gridExtra)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

roundMean = function(x) {
  round(mean(x, na.rm = TRUE), 2)
}

roundStd = function(x) {
  round(sd(x, na.rm = TRUE), 2)
}

rmExcept = function(x) {
  # x must be a string or a list of strings which encode the var names.
  rm(list=setdiff(ls(), x))
}

removeAttributes <- function (data) {
  data <- lapply(data, function(x) {attr(x, 'labels') <- NULL; x})
  data <- lapply(data, function(x) {attr(x, 'label') <- NULL; x})
  data <- lapply(data, function(x) {attr(x, 'class') <- NULL; x})
  data <- lapply(data, function(x) {attr(x, 'levels') <- NULL; x})
  data = data.frame(data)
}

pullAttributes <- function (data) {
  label = lapply(data, function(x) attr(x, 'label'))
  
  label = data.frame(label)
  # labels = lapply(data, function(x) attr(x, 'labels'))
  
  # attrs = data.frame(label = label, labels = labels)
}


percent = function(x, ndigits = 1) {
  paste0(sprintf("%.1f", round(x*100, ndigits)), "%")
}

theme_laura <- function() {
  theme_bw() + 
    theme(axis.text = element_text(size = 14),
          axis.title =  element_text(size = 16, face = "bold"),
          title =  element_text(size = 18, face = "bold"),
          strip.text = element_text(size=11)) 
}

theme_jointplot <- function() {
  theme_bw() +
    theme(
      axis.text = element_text(size = 16, color = 'black'),
      title =  element_text(size = 18, face = "bold", hjust = 0, color = 'black'),
      axis.title.y =  element_text(size = 20, face = "bold", color = 'black', hjust = 0.5, vjust = 1),
      axis.title.x =  element_text(size = 20, face = "bold", color = 'black', hjust = 0.5, vjust = -0.25),
      # axis.title.y = element_blank(), 
      # axis.line = element_blank(),
      # axis.ticks = element_blank()
      strip.text = element_text(size=13, face = 'bold'),
      legend.position = c(0.85, 0.85),
      legend.text = element_text(size = 13),
      strip.background = element_blank()
      #           panel.grid.minor.y = element_blank(),
      #           panel.grid.major.y = element_blank())
    )
}

theme_blankbox <- function() {
  theme_bw() +
    theme(
      axis.text = element_text(size = 16, color = 'white'),
      title =  element_text(size = 18, face = "bold", hjust = 0, color = 'white'),
      axis.title =  element_text(size = 20, face = "bold", color = 'white', hjust = 0.5, vjust = -0.25),
      # axis.title.y = element_blank(), 
      # axis.line = element_blank(),
      axis.ticks = element_blank(),
      strip.text = element_text(size=11),
      strip.background = element_blank(),
      legend.position="none",
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank()
      )
}

theme_blankLH<- function() {
  theme_classic() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(), 
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.position="none",
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank())
}