library(grid)
library(dplyr)
library(ggplot2)
library(stringr)
library(RColorBrewer)

setwd("~/GitHub/Ethiopia/")
source("R/setupFncns.r")
source("~/GitHub/Ethiopia/R/loadETHpanel.r")

# Colors and sizes ------------------------------------------------------------------
colors = brewer.pal(9, 'YlOrRd')

heightAvg = 3.4
widthAvg = 4.2


# Read in data ------------------------------------------------------------
shock_stats2012 <- read.table("~/GitHub/Ethiopia/Data/shock_stats_hh_2012.txt", 
                              header=TRUE, row.names = 1, sep ="")

shock_stats2012_avg <- read.table("~/GitHub/Ethiopia/Data/shock_stats_2012.txt", 
                                  header=TRUE, row.names = 1, sep ="")

shock_stats2012_FtF <- read.table("~/GitHub/Ethiopia/Data/shock_stats_FTF_2012.txt", 
                                  header=TRUE, row.names = 1, sep ="")

allShocks2012 = cbind(shock_stats2012, shock_stats2012_FtF, shock_stats2012_avg)

shock_stats2014 <- read.table("~/GitHub/Ethiopia/Data/shock_stats_hh_2014.txt", 
                              header=TRUE, row.names = 1, sep ="")

shock_stats2014_avg <- read.table("~/GitHub/Ethiopia/Data/shock_stats_2014.txt", 
                                  header=TRUE, row.names = 1, sep ="")

shock_stats2014_FtF <- read.table("~/GitHub/Ethiopia/Data/shock_stats_FTF_2014.txt", 
                                  header=TRUE, row.names = 1, sep ="")

allShocks2014 = cbind(shock_stats2014, shock_stats2014_FtF, shock_stats2014_avg)





# Arrangement order (total shocks) ----------------------------------------

# hhPanel  %>% group_by(regionName)  %>% summarise(shocked = mean(rptShock))  %>% arrange(desc(shocked))
allShocks2014 = allShocks2014 %>% 
  select(-contains('_subpop_1'), -contains('_subpop_2'))

p = allShocks2014 %>% 
  select(contains('price')) %>% 
  slice(-2)

p = t(p)
names = row.names(p)
names = str_replace(names, '^priceShk$', 'all Ethiopia')
names = str_replace(names, 'priceShk.', '')
names = str_replace(names, '_subpop_6', 'Benshagul Gumuz')
names = str_replace(names, '_subpop_1', 'non-Feed the Future zone')
names = str_replace(names, '_subpop_2', 'Feed the Future zone')





# assumes is in the format:
# [1] "Tigray"                   "Afar"                     "Amhara"                   "Oromia"                   "Somalie"                  "Benshagul Gumuz"         
# [7] "SNNP"                     "Gambelia"                 "Harari"                   "Diredwa"                  "non-Feed the Future zone" "Feed the Future zone"    
# [13] "all Ethiopia"



# price -------------------------------------------------------------------
# Arranged in descending order of total reported shocks, for 2012-2014.
p = allShocks2014 %>% 
  select(contains('price')) %>% 
  slice(-2)

p = t(p)

price14 = data.frame(p) %>% 
  mutate(name = names, x = X1, lb = X2, ub = X3, nObs = X4) %>% 
  arrange(desc(x)) %>% 
  mutate (order = c(2:6, 1, 7:11)) %>% 
  arrange(desc(order)) %>% 
  mutate(ymin = c(seq(10, 110, by = 10)),
         ymax = c(seq(10, 110, by = 10)))





# health ------------------------------------------------------------------
h = allShocks2014 %>% 
  select(contains('health')) %>% 
  slice(-2)

h = t(h)

health14 = data.frame(h) %>% 
  mutate(name = names, x = X1, lb = X2, ub = X3, nObs = X4) %>% 
  arrange(desc(x)) %>% 
  mutate (order = c(2:4, 1, 5:11)) %>% 
  arrange(desc(order)) %>% 
  mutate(
         ymin = c(seq(10, 110, by = 10)),
         ymax = c(seq(10, 110, by = 10)))



# hazards -----------------------------------------------------------------

w = allShocks2014 %>% 
  select(contains('hazard')) %>% 
  slice(-2)

w= t(w)

hazard14 = data.frame(w) %>% 
  mutate(name = names, x = X1, lb = X2, ub = X3, nObs = X4) %>% 
  arrange(desc(x)) %>% 
  mutate (order = c(2:6, 1, 7:11)) %>% 
  arrange(desc(order)) %>% 
  mutate(ymin = c(seq(10, 110, by = 10)),
         ymax = c(seq(10, 110, by = 10)))



# Plotting function definition --------------------------------------------

pairGrid = function (vals, title, xLab = "percent of households",
                     year = "2012",
                     sizeLine = 0.9, colorLine = 'grey',
                     xLim = NA, 
                     lineOverride = FALSE, lineAdj = 0.02,
                     annotAdj = 0.02,
                     sizeAnnot = 3, sizePct = 3,
                     sizeDot = 3, borderDot = 1,
                     colorDot = brewer.pal(9, 'YlOrRd'),
                     rangeColors = c(0,0.65),
                     # Controlling average point:
                     lineAvgAdj = 2.75, sizeAvg = 0.1,
                     xLabAdj = 0.0135,
                     colorNObs = c("#f2f2f2", "#4d4d4d")) {
  
  # -- Limits for the graph overall --
  if (is.na(xLim)) {
    xLim = c(min(min(vals$x)) - 6, max(max(vals$x)) + 3)
  }
  
  if(lineOverride){
    xMin = min(vals$x) - lineAdj
    xMax = max(vals$x) + lineAdj
  } else {
    xMin = 0
    xMax = 0.4
  }
  
  xAvg = vals %>% filter(name == "all Ethiopia") %>% select(x)
  
  vals = vals %>% 
    filter(name != "all Ethiopia")
  
  # -- Limits for the line underneath the points. --
  vals = vals %>% 
    mutate(xMin = xMin,
           xMax = xMax, 
           yAvgMin = ymin - lineAvgAdj,
           yAvgMax = ymax + lineAvgAdj)
  
  # -- Set up the base plot --
  ggplot(data = vals) + 
    theme(legend.position="none",
          plot.background = element_blank(),
          panel.background = element_blank(),
          axis.text = element_text(size = 9, color = 'black'),
          title =  element_text(size = 18, face = "bold", hjust = 0, color = 'black'),
          axis.title =  element_text(size =11, face = "bold", color = 'black', hjust = 0.5, vjust = 0),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y= element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y= element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(size = 0.5, color = 'black'),
          axis.ticks.x = element_line(color = 'black'),
          plot.margin = rep(unit(0, units = 'points'),4),
          panel.margin = rep(unit(0, units = 'points'),4),
          panel.border = element_blank(),
          # axis.text.x  = element_blank(), axis.title.x  = element_blank(),
          # axis.ticks = element_blank(), axis.line = element_blank(),
          axis.ticks.y = element_blank(), axis.line.y = element_blank(),
          axis.text.y = element_blank(), axis.title.y = element_blank(),
          aspect.ratio = heightAvg / widthAvg) +
    
    # -- axis limits --
    # coord_cartesian(ylim = c(-5, nrow(vals)*10 + 10), xlim = xLim) +
    coord_cartesian(ylim = c(-5, max(vals$ymin)+ 8), xlim = xLim) + 
    scale_x_continuous(labels = percent, expand = c(0,0)) +
    
    # -- labels --
    # ggtitle(title) +
    #     annotate("text", x = 0, y = max(vals$ymin) + 9.5, 
    #              size = 6.5, label = year, 
    #              color = vals$colors[7], hjust = 0) +
    #     annotate("text", x = 0, y = max(vals$ymin) + 15, 
    #              size = 8, label = title,
    #              fontface = "bold", hjust = 0) 
    xlab(xLab) +
    
    # -- Plot the line underneath all the points --
    #     geom_segment(aes(x = xMin, xend = xMax, y = ymin, yend = ymax),
    #                  color = colorLine, size = sizeLine) +
    
    # - as separate lines -
    #     geom_segment(aes(x = xAvg, xend = xAvg, y = yAvgMin, yend = yAvgMax),  
    #                  color = colorAvg, size = sizeAvg) +
    
    # -- Plot country average --
    geom_vline(xint = xAvg[1,1], linetype = 1, color = colorDot[5], size = sizeAvg) +
    
    # -- Add in S.E. --
    geom_rect(aes(xmin = lb, xmax = ub, ymin = ymin - 0.5, ymax = ymax + 0.5, fill = 'grey'), 
              alpha = 0.3) +
    scale_fill_identity()+
    
    # -- Overlay the points --
    # geom_point(aes(x = x, y = ymin), size = (sizeDot + borderDot), color = 'black') + # border
    geom_point(aes(x = x, y = ymin, colour = x), size = sizeDot) +
    scale_colour_gradientn(colours = colorDot,   limits = rangeColors) +
    
    # -- Add in circles containing the number of samples per segment. --
    #     geom_rect(aes(xmax = -0.01, xmin = -0.05, 
    #                   ymin = ymin - 3, ymax = ymin + 2, fill = nObs)) +
    #     geom_point(aes(x = -0.043,
    #                    y = ymin,  color = nObs), size = sizeDot * 2) +
    #     geom_text(aes(x = -0.043, y = ymin, label = nObs), size = 4.5, fontface = 'bold') + 
    # scale_color_gradientn(colours = colorDot) +
    
    # -- Add in names on the left --
    annotate("text", x = vals$xMin - annotAdj, y = vals$ymin, 
             size = sizeAnnot, label= vals$name, hjust = 1) +
    
    # -- Annotate percents over the numbers --
    annotate("text", x = vals$x + xLabAdj, y = vals$ymin + 4, 
             size = sizePct, label= percent(vals$x,0), hjust = 0.5) 
  
  
  
  # -- blocks for the labels --
  # annotate("rect", xmin = -0.35, xmax = -0.32, ymin = 0, ymax = vals$ymin[2] + 5, fill = vals$colors[3], alpha = 0.3) +
  # annotate("rect", xmin = -0.35, xmax = -0.32, ymin = vals$ymin[3] - 5, ymax = vals$ymin[12] + 5, fill = vals$colors[8], alpha = 0.3)
  
}



pairGridnObs = function (vals, title, xLab = "percent of households",
                         year = "2012",
                         sizeLine = 0.9, colorLine = 'grey',
                         xLim = NA, 
                         lineOverride = FALSE, lineAdj = 0.02,
                         annotAdj = -0.12,
                         sizeAnnot = 3, sizePct = 3,
                         sizeDot = 3, borderDot = 1,
                         colorDot = brewer.pal(9, 'YlOrRd'),
                         rangeColors = c(0,0.65),
                         # Controlling average point:
                         lineAvgAdj = 2.75, sizeAvg = 0.1,
                         xLabAdj = 0.0135,
                         colorNObs = c("#d7d7d7", "#5b5b5b"),
                         nObsAdj = -0.02, sizeNObsText = 2) {
  
  # -- Limits for the graph overall --
  if (is.na(xLim)) {
    xLim = c(min(min(vals$x)) - 6, max(max(vals$x)) + 3)
  }
  
  if(lineOverride){
    xMin = min(vals$x) - lineAdj
    xMax = max(vals$x) + lineAdj
  } else {
    xMin = 0
    xMax = 0.4
  }
  
  xAvg = vals %>% filter(name == "all Ethiopia") %>% select(x)
  
  vals = vals %>% 
    filter(name != "all Ethiopia")
  
  # -- Limits for the line underneath the points. --
  vals = vals %>% 
    mutate(xMin = xMin,
           xMax = xMax, 
           yAvgMin = ymin - lineAvgAdj,
           yAvgMax = ymax + lineAvgAdj)
  
  # -- Set up the base plot --
  ggplot(data = vals) + 
    theme(legend.position="none",
          plot.background = element_blank(),
          panel.background = element_blank(),
          axis.text = element_text(size = 9, color = 'black'),
          title =  element_text(size = 18, face = "bold", hjust = 0, color = 'black'),
          axis.title =  element_text(size =11, face = "bold", color = 'black', hjust = 0.5, vjust = 0),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y= element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y= element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(size = 0.5, color = 'black'),
          axis.ticks.x = element_line(color = 'black'),
          plot.margin = rep(unit(0, units = 'points'),4),
          panel.margin = rep(unit(0, units = 'points'),4),
          panel.border = element_blank(),
          # axis.text.x  = element_blank(), axis.title.x  = element_blank(),
          # axis.ticks = element_blank(), axis.line = element_blank(),
          axis.ticks.y = element_blank(), axis.line.y = element_blank(),
          axis.text.y = element_blank(), axis.title.y = element_blank(),
          aspect.ratio = heightAvg / widthAvg) +
    
    # -- axis limits --
    # coord_cartesian(ylim = c(-5, nrow(vals)*10 + 10), xlim = xLim) +
    coord_cartesian(ylim = c(-5, max(vals$ymin)+ 8), xlim = xLim) + 
    scale_x_continuous(labels = percent, expand = c(0,0)) +
    
    # Add in circles containing the number of samples per segment.
    geom_point(aes(x = -0.2,
                   y = ymin,  color = nObs), size = sizeDot * 2.25) +
    geom_text(aes(x = -0.2, y = ymin, label = nObs), size = sizeNObsText, fontface = 'bold') + 
    scale_color_gradientn(colours = colorNObs) +
    
    # Add in names on the left
    annotate("text", x = vals$xMin - annotAdj, y = vals$ymin, 
             size = sizeAnnot, label= vals$name, hjust = 1)
}

# Plot all shock summaries ----------------------------------------------------------------
# -- Price --
# << ETH_priceShk14_avg.pdf >>
pairGrid(price14, 'Price shocks', year = '2014', xLim = c(-0.3, 0.60), 
         sizeLine = 0)

ggsave("~/GitHub/Ethiopia/R/plots/ETH_priceShk14_avg.pdf", 
       width = widthAvg, height = heightAvg,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# << ETH_price14Nobs_avg.pdf >>
pairGridnObs(price14, 'Price shocks', year = '2014', xLim = c(-0.3, 0.60), 
         sizeLine = 0, xLabAdj = 0.0135, lineAvgAdj = 10)

ggsave("~/GitHub/Ethiopia/R/plots/ETH_price14Nobs_avg.pdf", 
       width = widthAvg, height = heightAvg,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# -- Health --
# << ETH_healthShk14_avg.pdf >>
pairGrid(health14, 'Health shocks', year = '2014', xLim = c(-0.3, 0.30), 
         sizeLine = 0)

ggsave("~/GitHub/Ethiopia/R/plots/ETH_healthShk14_avg.pdf", 
       width = widthAvg, height = heightAvg,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# << ETH_health14Nobs_avg.pdf >>
pairGridnObs(health14, 'Health shocks', year = '2014', xLim = c(-0.3, 0.30), 
             sizeLine = 0, xLabAdj = 0.0135, lineAvgAdj = 10)

ggsave("~/GitHub/Ethiopia/R/plots/ETH_health14Nobs_avg.pdf", 
       width = widthAvg, height = heightAvg,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# -- Hazard --
# << ETH_hazardShk14_avg.pdf >>
pairGrid(hazard14, 'Hazard shocks', year = '2014', xLim = c(-0.3, 0.50), 
         sizeLine = 0)

ggsave("~/GitHub/Ethiopia/R/plots/ETH_hazardShk14_avg.pdf", 
       width = widthAvg, height = heightAvg,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# << ETH_haz14Nobs_avg.pdf >>
pairGridnObs(hazard14, 'Hazard shocks', year = '2014', xLim = c(-0.3, 0.50), 
             sizeLine = 0, xLabAdj = 0.0135, lineAvgAdj = 10)

ggsave("~/GitHub/Ethiopia/R/plots/ETH_haz14Nobs_avg.pdf", 
       width = widthAvg, height = heightAvg,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# Colors for infographic --------------------------------------------------
avgs = shock_stats2014_avg %>% select(priceShk, healthShk, hazardShk) %>% slice(1)

avgs = data.frame(t(avgs))

avgs = avgs %>% mutate(x = 1:3, y = 1)

ggplot(avgs, aes(x = x, y = y, fill = `t.avgs.`)) + 
  geom_tile() +
  scale_fill_gradientn(colours = colors, 
                       values = seq(0, 0.5, by = 0.01), 
                       rescaler = function(x, ...) x, oob = identity)

# Comparision between shocks ----------------------------------------------


# Relative heatmap --------------------------------------------------------
# <<ETH_allShocks_heatmapVert_draft.pdf>>

s14 = data.frame(regions = unlist(regions), shocks = unlist(shocks), regShocks)

s14 = s14 %>% 
  filter(shocks != 'asset')

s14$shocks = factor(s14$shocks, c('price', 'hazard', 'health'))
s14$regions = factor(s14$regions, rev(c('Somalie', 'Diredwa',
                                        'Harari', 'Afar', 'Oromia',
                                        'Tigray', 'SNNP', 'Amhara',
                                        'Benshagul Gumuz', 'Gambelia')))


avgHealth = shock_stats2014_avg$healthShk[1]
avgPrice = shock_stats2014_avg$priceShk[1]
avgHazard = shock_stats2014_avg$hazardShk[1]

s14_rel = s14 %>% 
  mutate(rel_mean = ifelse(
    shocks == 'health', mean - avgHealth, 
    ifelse(
      shocks == 'price', mean - avgPrice,
      mean - avgHazard
    )
  ))

# vertical: regions on the y-axis, shocks on x.
ggplot(s14_rel) +
  geom_tile(aes(x = shocks, y = regions, fill = rel_mean), color = 'white', size = 1) +
  scale_fill_gradientn(colours = rev(PlBl), limits = c(-0.302, 0.302)) +
  geom_text(aes(x = shocks, y = regions, label = sprintf('%.1f', round(rel_mean * 100,1))), size = 4) +
  ggtitle('Household shocks per region, 2014') +
  theme_classic() +
  theme(
    axis.text = element_text(size = 16, color = 'black'),
    title =  element_text(size = 18, face = "bold", hjust = 0, color = 'black'),
    axis.title =  element_text(size = 20, face = "bold", color = 'black', hjust = 0.5, vjust = -0.25),
    axis.title.y = element_blank(), 
    axis.line = element_blank(),
    axis.ticks = element_blank()
    # axis.ticks.length = unit(0.15, "inch")
  )

# horizontal: regions on the x-axis, shocks on y.
# <<ETH_allShocks_heatmapHoriz_draft.pdf>>

s14_rel$regions = factor(s14$regions, c('Somalie', 'Diredwa',
                                        'Harari', 'Afar', 'Oromia',
                                        'Tigray', 'SNNP', 'Amhara',
                                        'Benshagul Gumuz', 'Gambelia'))

s14_rel$shocks = factor(s14$shocks, rev(c('price', 'hazard', 'health')))

ggplot(s14_rel) +
  geom_tile(aes(y = shocks, x = regions, fill = rel_mean), color = 'white', size = 1) +
  scale_fill_gradientn(colours = rev(PlBl), limits = c(-0.302, 0.302)) +
  geom_text(aes(y = shocks, x = regions, label = sprintf('%.1f', round(rel_mean * 100,1))), size = 4) +
  ggtitle('Household shocks per region, 2014') +
  theme_classic() +
  theme(
    axis.text = element_text(size = 16, color = 'black'),
    title =  element_text(size = 18, face = "bold", hjust = 0, color = 'black'),
    axis.title =  element_text(size = 20, face = "bold", color = 'black', hjust = 0.5, vjust = -0.25),
    axis.title.y = element_blank(), 
    axis.line = element_blank(),
    axis.ticks = element_blank()
    # axis.ticks.length = unit(0.15, "inch")
  )


# Ftf diff-diffs ----------------------------------------------------------
nonColor = ftfBlue
ftfColor = ftfOrange


ftfDiffs = data %>% 
  filter(ftfzone_5km != 99)


bumpChart = function(data,
                     var = 'healthShk', 
                     year1 = 2012,
                     year2 = 2014,
                     sizeIfeq = 0.7,
                     sizeLine = 2,
                     sizeDot = 6,
                     sizeDotIfeq = sizeDot/2,
                     sizeLab = 6,
                     xAdjLab = 0.15,
                     ymax = NA, 
                     numAsPct = TRUE,
                     title = ""){
  
  # Filter out NA values.
  data = data %>% 
    filter_(paste0('!is.na(',var,')'))
  
  
  # Calculate what things would look like if non-ftf change was applied.
  avgShk = data %>% 
    group_by(year, ftfzone_5km) %>% 
    summarise_(avg = paste0('mean(', var, ')'))
  
  # Calculate averages
  ctrl12 = avgShk %>% filter(year == year1, ftfzone_5km == 0) %>% select(avg)
  ctrl12 = ctrl12$avg
  ctrl14 = avgShk %>% filter(year == year2, ftfzone_5km == 0) %>% select(avg)
  ctrl14 = ctrl14$avg
  
  ftf12 = avgShk %>% filter(year == year1, ftfzone_5km == 1) %>% select(avg)
  ftf12 = ftf12$avg
  ftf14 = avgShk %>% filter(year == year2, ftfzone_5km == 1) %>% select(avg)
  ftf14 = ftf14$avg
  
  if (numAsPct == TRUE){
    ctrl12Lab = percent(ctrl12)
    ctrl14Lab = percent(ctrl14)
    ftf12Lab = percent(ftf12)
    ftf14Lab = percent(ftf14)
  } else {
    ctrl12Lab = round(ctrl12, 1)
    ctrl14Lab = round(ctrl14, 1)
    ftf12Lab = round(ftf12, 1)
    ftf14Lab = round(ftf14, 1)
  }
  
  
  # Labels for the percentages
  avg4Labels = data.frame(year1 = year1 - xAdjLab, year2 = year2 + xAdjLab, 
                          ctrl12, ctrl14, ftf12, ftf14,
                          ctrl12Lab, ctrl14Lab, ftf12Lab, ftf14Lab)
  
  # Calculate slope if the FtF data were the same as the non-FtF data.
  slopeIfeq = (ctrl14 - ctrl12) / (year2-year1)
  y1 = avgShk %>% filter(year == year1, ftfzone_5km == 1)
  
  y1 = y1$avg
  
  intercept = y1 - slopeIfeq * year1
  
  y2 = slopeIfeq * year2 + intercept
  
  Ifeq = data.frame(year1 = year1, year2 = year2, y1 = y1, y2 = y2)
  
  # -- set y-lim --
  if (is.na(ymax)) {
    ymax = max(ctrl12, ctrl14, ftf12, ftf14) + 0.02
  }
  
  # -- Plot! --
  ggplot(data, aes_string(x = 'year', y = var, colour = 'factor(ftfzone_5km)')) +
    
    # -- Line if the treatment had the same change as the control --
    geom_segment(aes(x = year1, xend = year2, y = y1, yend = y2), data = Ifeq,
                 colour = 'grey', size = sizeIfeq) +
    geom_point(aes(x =  year2, y = y2), data = Ifeq,
               colour = 'grey', size = sizeDotIfeq) +
    
    # -- Dot and line for bump chart. --
    stat_summary(fun.y=mean,  geom = 'line', size = sizeLine)+
    stat_summary(fun.y=mean,  geom = 'point', size = sizeDot)+
    
    # -- Left/right number labels for the bumps.
    geom_text(aes(x = year1, y = ctrl12, label  = ctrl12Lab), 
              data = avg4Labels, size = sizeLab, color = nonColor) +
    geom_text(aes(x = year2, y = ctrl14, label  = ctrl14Lab), 
              data = avg4Labels, size = sizeLab, color = nonColor) +
    geom_text(aes(x = year1, y = ftf12, label  = ftf12Lab), 
              data = avg4Labels, size = sizeLab, color =ftfColor) +
    geom_text(aes(x = year2, y = ftf14, label  = ftf14Lab), 
              data = avg4Labels, size = sizeLab, color = ftfColor) +

    scale_color_manual(values = c('0' = nonColor, '1' = ftfColor)) +
    
    # -- Limits --
    coord_cartesian(ylim = c(0, ymax)) +
    scale_x_continuous(breaks = c(year1, year2)) +
    
    ggtitle(title) +
    
    # -- Set theme --
    theme(title = element_text(size = 12, color = 'black'),
          axis.line = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 16, color = 'black'),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), 
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position="none",
          panel.background = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(size=13, face = 'bold'),
          strip.background = element_blank()
    )
  
}

bumpChart(ftfDiffs, xAdjLab = 0.15)

ymax = 0.30


# Bump charts: food security ----------------------------------------------
widthFTF = 4.5
heightFTF = 5.5

# << ETH_Q1_bumpplot.pdf >>
q1 = bumpChart(ftfDiffs, var = 'worryLackFood', title = 'Did you worry you would not have enough food?', ymax = ymax, sizeLab = 5.5, xAdjLab = 0.3)
ggsave("~/GitHub/Ethiopia/R/plots/ETH_Q1_bumpplot.pdf", plot = q1,
       width = widthFTF, height = heightFTF,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# << ETH_Q2_bumpplot.pdf >>
q2 = bumpChart(ftfDiffs, var = 'daysEatBadFoodBin', title = 'Did you rely on less preferred foods?', ymax = ymax, sizeLab = 5.5, xAdjLab = 0.3)

ggsave("~/GitHub/Ethiopia/R/plots/ETH_Q2_bumpplot.pdf", plot = q2,
       width = widthFTF, height = heightFTF,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

q3 = bumpChart(ftfDiffs, var = 'daysLimitVarietyBin', title = 'Did you limit the variety of foods eaten?', ymax = ymax, sizeLab = 4, xAdjLab = 0.3)
q4 = bumpChart(ftfDiffs, var = 'daysRedAmtBin', title = 'Did you limit portion size at mealtimes?', ymax = ymax, sizeLab = 4, xAdjLab = 0.3)
q5 = bumpChart(ftfDiffs, var = 'daysRedNumMealsBin', title = 'Did you reduce the number of meals?', ymax = ymax, sizeLab = 4, xAdjLab = 0.3)
q6 = bumpChart(ftfDiffs, var = 'daysRedAdultIntakeBin', title = 'Did you reduce consumption by adults to feed small children?', ymax = ymax, sizeLab = 4, xAdjLab = 0.3)
q7 = bumpChart(ftfDiffs, var = 'daysBorrowFoodBin', title = 'Did you borrow food from friends or relatives?', ymax = ymax, sizeLab = 4, xAdjLab = 0.3)
q8 = bumpChart(ftfDiffs, var = 'daysNoFoodSupplBin', title = 'Did you have no food of any kind in your household?', ymax = ymax, sizeLab = 4, xAdjLab = 0.3)
q9 = bumpChart(ftfDiffs, var = 'daysFastBin', title = 'Did you go a whole day without eating?', ymax = ymax, sizeLab = 4, xAdjLab = 0.3)


# << ETH_monthsFoodShort_bumpplot.pdf >>
bumpChart(ftfDiffs, var = 'numMonthFoodShort', 
          title = 'How many months in the past year did you have a situation where you did not have enough food to feed the household?', 
          ymax = 1.5, sizeLab = 5.5, xAdjLab = 0.3, numAsPct = F)

ggsave("~/GitHub/Ethiopia/R/plots/ETH_monthsFoodShort_bumpplot.pdf",
       width = widthFTF, height = heightFTF,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# << ETH_illnessShk_bumpplot.pdf >>
bumpChart(ftfDiffs, var = 'illnessShk', 
          title = 'Did anyone in your household have a major illness in the past year?', 
          ymax = ymax, sizeLab = 5.5, xAdjLab = 0.3)

ggsave("~/GitHub/Ethiopia/R/plots/ETH_illnessShk_bumpplot.pdf", 
       width = widthFTF, height = heightFTF,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

multiplot(q1, q2, q3, q4, q5, q6, q7, q8, q9, cols = 3)
multiplot(q2, q3, q4, q5, q1,  cols = 5)
