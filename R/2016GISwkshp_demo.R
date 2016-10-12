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

color_palette_dd = brewer.pal(9, 'YlGnBu')
color_limits_dd = c(4.5, 6.5)

# Choose what to plot -----------------------------------------------------
scale = 'ethiopia'
relative_values = FALSE
var2sort = 'regionName'
add_annotations = FALSE

# add_annotations = TRUE
# scale = 'regions'
# var2sort = 'dietDiv'
# relative_values = TRUE


# auto selectors ----------------------------------------------------------


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
  color_palette = brewer.pal(9, 'Blues')
  color_limits = c(0, 1)
}

# Set plotting options ---------------------------------------------------
widthDDheat = 3.25
heightDDheat = 1.65
widthDDavg = 2.25


# Plot foods by region ----------------------------------------------------

foods = ggplot(data2plot) +
  geom_tile(aes_string(x = 'food', 
                       y = paste0('forcats::fct_reorder(regionName,', var2sort, ')'),
                       fill = var2plot), 
            color = 'white', size = 0.3) +
  scale_fill_gradientn(colours = color_palette, 
                       limits = color_limits,
                       labels = scales::percent,
                       name = NULL) +
  scale_x_discrete(position = 'top') +
  ggtitle('Percent of households consuming a food, 2014') +
  theme_xylab()+
  theme(
    axis.text = element_text(colour = 'black'),
    legend.position = c(-0.1, 1.01),
    legend.direction = 'horizontal',
    legend.text  = element_text(size = 6, color = softBlack),
    legend.key.width = unit(0.15, 'inch'),
    legend.key.height = unit(0.15, 'inch')
  )


# plot dietary diversity score --------------------------------------------



dd = ggplot(data2plot) +
  geom_tile(aes_string(x = '1', 
                       y = paste0('forcats::fct_reorder(regionName,', var2sort, ')'),
                       fill = 'dietDiv'), 
            color = 'white', size = 0.3) +
  scale_fill_gradientn(colours = color_palette_dd, 
                       limits = color_limits_dd) +
  scale_x_continuous(position = 'top') +
  ggtitle('dietary diversity') +
  theme_xylab() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour = 'white'),
    legend.text  = element_text(size = 6, color = softBlack)
  )


# add annotations ---------------------------------------------------------


if (add_annotations == TRUE) {
  foods = foods + geom_text(aes_string(x = 'food', 
                                       y = paste0('forcats::fct_reorder(regionName,', var2sort, ')'),
                                       label = paste0('percent(', var2plot, ', 0)'),
                                       colour = paste0('abs(', var2plot, ')')),
                            family = 'Lato',
                            size = 4) +
    scale_color_text(data2plot[[var2plot]]) 
  
  dd = dd + 
    geom_text(aes_string(x = '1', 
                                 y = paste0('forcats::fct_reorder(regionName,', var2sort, ')'),
                                 label = 'round(dietDiv, 1)',
                                 colour = 'dietDiv'),
                      size = 4,
              family = 'Lato') +
    scale_color_text(data2plot[['dietDiv']]) 
} 


# plot both together ------------------------------------------------------


gridExtra::grid.arrange(foods, dd, ncol = 2, widths = c(0.8, 0.2))
