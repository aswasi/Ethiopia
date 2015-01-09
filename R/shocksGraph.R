# Make simple stacked bar graph for Ethiopia

# Clear the workspace
remove(list = ls())

# Load libraries & set working directory

libs <- c ("reshape", "ggplot2", "dplyr", "RColorBrewer", "grid", "scales")

# Load required libraries
lapply(libs, require, character.only=T)

# Set working directory for home or away
wd <- c("U:/Ethiopia/Excel/")
#wd <- c("C:/Users/t/Box Sync/Ethiopia/Export")
setwd(wd)

d <- read.csv("eth.shocks.region.csv", header = TRUE)

#Drop totals
d$Total <- NULL

# Graph Parameters: Set dots per inch for all graphic output; Set color palette
dpi.out = 500
clr = "YlOrRd"

# Lab RGB colors
redL   	<- c("#B71234")
dredL 	<- c("#822443")
dgrayL 	<- c("#565A5C")
lblueL 	<- c("#7090B7")
dblueL 	<- c("#003359")
lgrayL	<- c("#CECFCB")

# Melt data for plotting
df.melt <- melt(d, id = c("Region"))

# Make basic stacked bar graph
g <- ggplot(d.melt, aes(Region, value, fill = factor(d.melt$variable))) + geom_bar(stat = "identity")

g <- ggplot(df.melt, aes(x = reorder(factor(Region), value),
		y = value, fill = factor(df.melt$variable, 
		levels = rev(levels(df.melt$variable))))) + geom_bar(stat = "identity") + facet_wrap(~variable, ncol = 1)
#Toggle facet wrap depending on type of desired chart.

pp <- g + coord_flip()+labs(x ="", title = "Ethiopia: Major shocks by district", 
		y = "Percent of households reporting shock") + scale_fill_brewer(palette = clr ) +
		scale_y_continuous(labels=percent) +
		theme(legend.position = "top", legend.title=element_blank(), 
		panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
		axis.text.y  = element_text(hjust=1, size=10, colour = dgrayL ), axis.ticks.x=element_blank(),
		axis.text.x  = element_text(hjust=1, size=10, colour = dgrayL ),
		axis.title.x = element_text(colour=dgrayL , size=8),
		plot.title = element_text(lineheight=.8, colour = "black" )) + guides(fill = guide_legend(reverse=TRUE))
	print(pp)

# Change directory for writing .png to graphics directory
setwd("U:/Ethiopia/Graph/")
ggsave(pp, filename = paste("Shocks_district.pct", ".png"), width=14, height=10, dpi=dpi.out)
