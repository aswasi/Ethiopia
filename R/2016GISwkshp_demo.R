#______________________________________________________________________________
# Calculates the food consumption score for each household in 2014.  See
# Rmarkdown document for full details; methodology is based on the U.N.
# World Food Programme.
# 
# By Laura Hughes, USAID, lhughes@usaid.gov
# June 2014.
#______________________________________________________________________________

# Load in the data
source('~/GitHub/Ethiopia/R/2016GISwkshp_importData.R')



# Choose what to plot -----------------------------------------------------
scale = 'ethiopia'
relative_values = FALSE
var2sort = 'regionName'


# scale = 'regions'
# var2sort = 'dietDiv'
# relative_values = TRUE

if(scale == 'ethiopia') {
  data2plot = all_ethiopia
} else {
  data2plot = eth_adm1
}

if(relative_values == TRUE){
  var2plot = 'rel_mean'
  color_palette = PlBl
  color_limits = c(-0.55, 0.55)
} else{
  var2plot = 'avg'
  color_palette = brewer.pal(9, 'YlGnBu')
  color_limits = c(0, 1)
}

# Set plotting options ---------------------------------------------------
widthDDheat = 3.25
heightDDheat = 1.65
widthDDavg = 2.25



# Make plot ----------------------------------------------------------------

# Main heatmap
ggplot(data2plot) +
  geom_tile(aes_string(x = 'food', 
                       y = paste0('forcats::fct_reorder(regionName,', var2sort, ')'),
                       fill = var2plot), 
            color = 'white', size = 0.3) +
  scale_fill_gradientn(colours = color_palette, 
                       limits = color_limits) +
  # geom_text(aes(y = food, x = regionName, label = round(avg,1)), size = 4) +
  ggtitle('Dietary diversity, 2014') +
  theme_xylab()+
  theme(
    # axis.text = element_text(size = 6, color = softBlack),
    # title =  element_text(size = 1, face = "bold", hjust = 0, color = softBlack),
    legend.position = 'right',
    legend.text  = element_text(size = 6, color = softBlack),
    legend.key.width = unit(0.05, 'inch'),
    legend.key.height = unit(0.15, 'inch')
  )
