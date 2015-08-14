library(grid)
library(dplyr)
library(ggplot2)
library(stringr)
library(RColorBrewer)
source("R/setupFncns.r")

# Colors ------------------------------------------------------------------
colorsH = c(colorRampPalette(brewer.pal(9, "PuRd"))(12), "grey")
colorsW = c(colorRampPalette(brewer.pal(9, "YlOrBr"))(12), "grey")
colorsP = c(colorRampPalette(c('#dce3ed', '#3c567d', '#3c567d'))(11))
  # c(colorRampPalette(brewer.pal(9, "Greens"))(12), "grey")
colorsA = c(colorRampPalette(brewer.pal(9, "PuBu"))(12), "grey")


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
  mutate(colors = colorsP,
         ymin = c(seq(15, 115, by = 10)),
          ymax = c(seq(15, 115, by = 10)))





# assets ------------------------------------------------------------------
a = allShocks2012 %>% 
  select(contains('asset')) %>% 
  slice(-2)

a = t(a)

names = row.names(a)
names = str_replace(names, '^assetShk$', 'all Ethiopia')
names = str_replace(names, 'assetShk.', '')
names = str_replace(names, '_subpop_6', 'Benshagul Gumuz')
names = str_replace(names, '_subpop_1', 'non-Feed the Future zone')
names = str_replace(names, '_subpop_2', 'Feed the Future zone')

a = data.frame(a) %>% 
  mutate(name = names, x = X1, lb = X2, ub = X3, nObs = X4)



assets12 = full_join(a,coordsData) %>%
  mutate(colors = colorsA) %>% 
  arrange(ymin)

#---
a = allShocks2014 %>% 
  select(contains('asset')) %>% 
  slice(-2)

a = t(a)

a = data.frame(a) %>% 
  mutate(name = names, x = X1, lb = X2, ub = X3, nObs = X4)



assets14 = full_join(a,coordsData) %>%
  mutate(colors = colorsA) %>% 
  arrange(ymin)





# health ------------------------------------------------------------------
h = allShocks2012 %>% 
  select(contains('health')) %>% 
  slice(-2)

h = t(h)

names = row.names(h)
names = str_replace(names, '^healthShk$', 'all Ethiopia')
names = str_replace(names, 'healthShk.', '')
names = str_replace(names, '_subpop_6', 'Benshagul Gumuz')
names = str_replace(names, '_subpop_1', 'non-Feed the Future zone')
names = str_replace(names, '_subpop_2', 'Feed the Future zone')

h = data.frame(h) %>% 
  mutate(name = names, x = X1, lb = X2, ub = X3, nObs = X4)



health12 = full_join(h,coordsData) %>%
  mutate(colors = colorsH) %>% 
  arrange(ymin)

#---
h = allShocks2014 %>% 
  select(contains('health')) %>% 
  slice(-2)

h = t(h)

h = data.frame(h) %>% 
  mutate(name = names, x = X1, lb = X2, ub = X3, nObs = X4)



health14 = full_join(h,coordsData) %>%
  mutate(colors = colorsH) %>% 
  arrange(ymin)




# hazards -----------------------------------------------------------------
w = allShocks2012 %>% 
  select(contains('hazard')) %>% 
  slice(-2)

w = t(w)

names = row.names(w)
names = str_replace(names, '^hazardShk$', 'all Ethiopia')
names = str_replace(names, 'hazardShk.', '')
names = str_replace(names, '_subpop_6', 'Benshagul Gumuz')
names = str_replace(names, '_subpop_1', 'non-Feed the Future zone')
names = str_replace(names, '_subpop_2', 'Feed the Future zone')

w = data.frame(w) %>% 
  mutate(name = names, x = X1, lb = X2, ub = X3, nObs = X4)



hazard12 = full_join(w,coordsData) %>%
  mutate(colors = colorsW) %>% 
  arrange(ymin)

#---
w = allShocks2014 %>% 
  select(contains('hazard')) %>% 
  slice(-2)

w = t(w)

w = data.frame(w) %>% 
  mutate(name = names, x = X1, lb = X2, ub = X3, nObs = X4)



hazard14 = full_join(w,coordsData) %>%
  mutate(colors = colorsW) %>% 
  arrange(ymin)



# Plotting function definition --------------------------------------------


pairGrid = function (vals, title, xLab = "percent of households",
                     year = "2012",
                     sizeLine = 0.9, colorLine = 'grey',
                     xLim = NA, 
                     lineOverride = FALSE, lineAdj = 0.02,
                     annotAdj = 0.07,
                     sizeAnnot = 7, sizePct = 5.5,
                     pctAdj = 0.01,
                     sizeDot = 7, borderDot = 1,
                     colorDot = "dodgerblue",
                     # Controlling average point:
                     lineAvgAdj = 2.75, sizeAvg = 0.4,
                     colorNObs = c("#f2f2f2", "#4d4d4d")) {
  
  # Limits for the graph overall
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
  
  # Limits for the line underneath the points.
  vals = vals %>% 
    mutate(xMin = xMin,
           xMax = xMax, 
           yAvgMin = ymin - lineAvgAdj,
           yAvgMax = ymax + lineAvgAdj)
  
  # Set up the base plot
  ggplot(data = vals) + 
    theme(legend.position="none",
          axis.text = element_text(size = 16, color = 'black'),
          title =  element_text(size = 18, face = "bold", hjust = 0, color = 'black'),
          axis.title =  element_text(size = 20, face = "bold", color = 'black', hjust = 0.5, vjust = -0.25),
          panel.grid.major.x = element_line(size = 0.3, color = '#DDDDDD'),
          panel.grid.major.y= element_blank(),
          panel.grid.minor.x = element_line(size = 0.15, color = '#DDDDDD'),
          panel.grid.minor.y= element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(size = 0.8, color = 'black'),
          axis.ticks.x = element_line(color = 'black'),
          # axis.text.x  = element_blank(), axis.title.x  = element_blank(),
          # axis.ticks = element_blank(), axis.line = element_blank(),
          axis.ticks.y = element_blank(), axis.line.y = element_blank(),
          axis.text.y = element_blank(), axis.title.y = element_blank()) +
    
    # coord_cartesian(ylim = c(-5, nrow(vals)*10 + 10), xlim = xLim) +
    coord_cartesian(ylim = c(-5, max(vals$ymin)+ 8), xlim = xLim) +    
    
    # ggtitle(title) +
    xlab(xLab) +
    
    #     # Plot the line underneath all the points
    #     geom_segment(aes(x = xMin, xend = xMax, y = ymin, yend = ymax),
    #                  color = colorLine, size = sizeLine) +
    
    # as separate lines
    #     geom_segment(aes(x = xAvg, xend = xAvg, y = yAvgMin, yend = yAvgMax),  
    #                  color = colorAvg, size = sizeAvg) +
    # as a single line
    geom_vline(xint = xAvg[1,1], linetype = 1, color = colorDot[5]) +
    
    # Add in S.E.
    geom_rect(aes(xmin = lb, xmax = ub, ymin = ymin - 0.8, ymax = ymax + 0.8, fill = 'grey'), alpha = 0.2) +
    scale_fill_identity()+
    
    # Overlay the points
    geom_point(aes(x = x, y = ymin), size = (sizeDot + borderDot), color = 'black') +
    geom_point(aes(x = x, y = ymin), size = sizeDot, colour = colorDot) +
    
    # Add in rectangles containing the number of samples per segment.
#     geom_rect(aes(xmax = -0.01, xmin = -0.05, 
#                   ymin = ymin - 3, ymax = ymin + 2, fill = nObs)) +
    geom_point(aes(x = -0.043,
                  y = ymin,  color = nObs), size = sizeDot * 2) +
    geom_text(aes(x = -0.043, y = ymin, label = nObs), size = 4.5, fontface = 'bold') + 
    scale_color_gradientn(colours = colorNObs) +
    
    # Add in names on the left
    annotate("text", x = vals$xMin - annotAdj, y = vals$ymin, 
             size = sizeAnnot, label= vals$name, hjust = 1) +
    
    # Annotate percents over the numbers
    annotate("text", x = vals$x, y = vals$ymin + 4, 
             size = sizePct, label= percent(vals$x,0), hjust = 0.5) 
    
#     # Add in title
#     annotate("text", x = 0, y = max(vals$ymin) + 9.5, 
#              size = 6.5, label = year, 
#              color = vals$colors[7], hjust = 0) +
#     annotate("text", x = 0, y = max(vals$ymin) + 15, 
#              size = 8, label = title,
#              fontface = "bold", hjust = 0) 
    
    # blocks for the labels
    # annotate("rect", xmin = -0.35, xmax = -0.32, ymin = 0, ymax = vals$ymin[2] + 5, fill = vals$colors[3], alpha = 0.3) +
    # annotate("rect", xmin = -0.35, xmax = -0.32, ymin = vals$ymin[3] - 5, ymax = vals$ymin[12] + 5, fill = vals$colors[8], alpha = 0.3)
  
}


# Plot all ----------------------------------------------------------------

h12 = pairGrid(health12, 'Health shocks', year = '2012', xLim = c(-0.35, 0.60), colorDot = health12$colors[1:12],
         sizeLine = 0, pctAdj = -.15, lineAvgAdj = 10)

h14 = pairGrid(health14, 'Health shocks', year = '2014', xLim = c(-0.35, 0.60), colorDot = health14$colors[1:12],
         sizeLine = 0, pctAdj = -.15, lineAvgAdj = 10)
multiplot(h12, h14, cols = 2)



p12 = pairGrid(price12, 'Price shocks', year = '2012', xLim = c(-0.35, 0.60), colorDot = price12$colors[1:12],
               sizeLine = 0, pctAdj = -.15, lineAvgAdj = 10)

pairGrid(price14, 'Price shocks', year = '2014', xLim = c(-0.3, 0.60), colorDot = price14$colors[1:10],
               sizeLine = 0, pctAdj = -.15, lineAvgAdj = 10)

a14 = pairGrid(assets14, 'Asset shocks', year = '2014', xLim = c(-0.5, 0.60), colorDot = assets14$colors[1:12],
               sizeLine = 0, pctAdj = -.15, lineAvgAdj = 10)
multiplot(a12, a14, cols = 2)


w12 = pairGrid(hazard12, 'Hazard shocks', year = '2012', xLim = c(-0.35, 0.60), colorDot = hazard12$colors[1:12],
               sizeLine = 0, pctAdj = -.15, lineAvgAdj = 10)

w14 = pairGrid(hazard14, 'Hazard shocks', year = '2014', xLim = c(-0.35, 0.60), colorDot = hazard14$colors[1:12],
               sizeLine = 0, pctAdj = -.15, lineAvgAdj = 10)
multiplot(w12, w14, cols = 2)




# Comparision between shocks ----------------------------------------------
regShocks = t(shock_stats2012)

names = row.names(regShocks)
names = str_replace(names, 'Shk', '')
names = str_replace(names, '_subpop_6', 'Benshagul Gumuz')
names = str_split(names, "[.]")

shocks = lapply(names, '[[', 1)


regions = lapply(names, '[[', 2)

s = data.frame(regions = unlist(regions), shocks = unlist(shocks), regShocks)

s$shocks = factor(s$shocks, c('price', 'hazard', 'health', 'asset'))


# Parallel coordinates
ggplot(s) +
  # geom_point(aes(x = shocks, y = mean, group = regions, color = regions), size = 3) +
  geom_line(aes(x = shocks, y = mean, group = regions, color = regions), size = 1.25) +
  geom_vline(xint = c(1:4), size = 2)+
  geom_text(aes(x = 0.9, y = mean, group = regions, color = regions, 
                label = regions), data = s %>% filter(shocks =='price'), hjust= 1, size = 6.5)+
  geom_ribbon(aes(x = shocks, ymin = lowerB, ymax = upperB, group = regions, fill = regions), alpha = 0.3) +
  scale_fill_brewer(palette = 'Paired') +
  scale_color_brewer(palette = 'Paired') +
  theme(legend.position="none",
        axis.text = element_text(size = 16, color = 'black'),
        title =  element_text(size = 18, face = "bold", hjust = 0, color = 'black'),
        axis.title =  element_text(size = 20, face = "bold", color = 'black', hjust = 0.5, vjust = -0.25),
        # panel.grid.major.y = element_line(size = 0.3, color = '#DDDDDD'),
        panel.grid.major.x= element_blank(),
        # panel.grid.minor.y = element_line(size = 0.15, color = '#DDDDDD'),
        panel.grid.minor.x= element_blank(),
        panel.background = element_blank(),
        # axis.line = element_line(size = 0.8, color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.15, "inch"),
        # axis.text.x  = element_blank(), axis.title.x  = element_blank(),
        # axis.ticks = element_blank(), axis.line = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.line.y = element_blank(),
        axis.line.x = element_blank()
        ) 


s$regions = factor(s$regions, coordsData$name[3:12])

# Line graph
ggplot(s) +
  geom_line(aes(y = mean, x = regions, group = shocks, color = shocks), size = 1.25) +
  geom_text(aes(x = 11, y = mean, group = shocks, color = shocks, 
                label = shocks), data = s %>% filter(regions == 'Somalie'), hjust= 1, size = 6.5)+
  # geom_ribbon(aes(ymin = lowerB, ymax = upperB, x = regions, group = shocks, fill = shocks), alpha = 0.3) +
  scale_fill_manual(values = c('asset' = colorsA[8], 'price' = colorsP[8], 'hazard' = colorsW[8], 'health' = colorsH[8])) +
  scale_color_manual(values = c('asset' = colorsA[8], 'price' = colorsP[8], 'hazard' = colorsW[8], 'health' = colorsH[8])) +
  theme(legend.position="none",
        axis.text = element_text(size = 16, color = 'black'),
        title =  element_text(size = 18, face = "bold", hjust = 0, color = 'black'),
        axis.title =  element_text(size = 20, face = "bold", color = 'black', hjust = 0.5, vjust = -0.25),
        panel.grid.major.y = element_line(size = 0.3, color = '#DDDDDD'),
        panel.grid.major.x = element_line(size = 0.3, color = '#DDDDDD'),
        panel.grid.minor.y = element_line(size = 0.15, color = '#DDDDDD'),

        panel.background = element_blank(),
        # axis.line = element_line(size = 0.8, color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.15, "inch"),
        # axis.text.x  = element_blank(), axis.title.x  = element_blank(),
        # axis.ticks = element_blank(), axis.line = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.line.y = element_blank(),
        axis.line.x = element_blank()
  ) 


library(treemap)

treemap(s %>% filter(regions == 'Tigray'), index = 'shocks', vSize = 'mean', fontsize.title = 22, fontsize.labels = 18, title = 'Tigray')

s12 = s
ggplot(s12) +
  geom_tile(aes(x = shocks, y = regions, fill = mean), color = 'white', size = 2) +
  scale_fill_gradientn(colours = brewer.pal(9, 'BuPu'), limits = c(0,0.5)) +
  geom_text(aes(x = shocks, y = regions, label = sprintf('%.1f', round(mean * 100,1))), size = 7) +
  ggtitle('Household shocks per region, 2012') +
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


regShocks = t(shock_stats2014)

names = row.names(regShocks)
names = str_replace(names, 'Shk', '')
names = str_replace(names, '_subpop_6', 'Benshagul Gumuz')
names = str_split(names, "[.]")

shocks = lapply(names, '[[', 1)


regions = lapply(names, '[[', 2)

s14 = data.frame(regions = unlist(regions), shocks = unlist(shocks), regShocks)

s14$shocks = factor(s14$shocks, c('price', 'hazard', 'health', 'asset'))
s14$regions = factor(s14$regions, coordsData$name[3:12])

ggplot(s14) +
  geom_tile(aes(x = shocks, y = regions, fill = mean), color = 'white', size = 2) +
  scale_fill_gradientn(colours = brewer.pal(9, 'BuPu'), limits = c(0,0.5)) +
  geom_text(aes(x = shocks, y = regions, label = sprintf('%.1f', round(mean * 100,1))), size = 7) +
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

# --- Relative

avgHealth = shock_stats2012_avg$healthShk[1]
avgPrice = shock_stats2012_avg$priceShk[1]
avgAsset = shock_stats2012_avg$assetShk[1]
avgHazard = shock_stats2012_avg$hazardShk[1]

s12_rel = s12 %>% 
  mutate(rel_mean = ifelse(
    shocks == 'health', mean - avgHealth, 
    ifelse(
      shocks == 'asset', mean - avgAsset,
      ifelse(
        shocks == 'price', mean - avgPrice,
        mean - avgHazard
      )
    )
  ))

ggplot(s12_rel) +
  geom_tile(aes(x = shocks, y = regions, fill = rel_mean), color = 'white', size = 2) +
  scale_fill_gradientn(colours = rev(brewer.pal(9, 'BrBG')), limits = c(-0.32, 0.32)) +
  geom_text(aes(x = shocks, y = regions, label = sprintf('%.1f', round(rel_mean * 100,1))), size = 7) +
  ggtitle('Household shocks per region, 2012') +
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

# --- 2014

avgHealth = shock_stats2014_avg$healthShk[1]
avgPrice = shock_stats2014_avg$priceShk[1]
avgAsset = shock_stats2014_avg$assetShk[1]
avgHazard = shock_stats2014_avg$hazardShk[1]

s14_rel = s14 %>% 
  mutate(rel_mean = ifelse(
    shocks == 'health', mean - avgHealth, 
    ifelse(
      shocks == 'asset', mean - avgAsset,
      ifelse(
        shocks == 'price', mean - avgPrice,
        mean - avgHazard
      )
    )
  ))

ggplot(s14_rel) +
  geom_tile(aes(x = shocks, y = regions, fill = rel_mean), color = 'white', size = 2) +
  scale_fill_gradientn(colours = rev(brewer.pal(9, 'BrBG')), limits = c(-0.32, 0.32)) +
  geom_text(aes(x = shocks, y = regions, label = sprintf('%.1f', round(rel_mean * 100,1))), size = 7) +
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


s14_change = cbind(s14, change = s14 - s12)

ggplot(s14_change) +
  geom_tile(aes(x = shocks, y = regions, fill = change.mean), color = 'white', size = 2) +
  scale_fill_gradientn(colours = rev(brewer.pal(9, 'BrBG')), limits = c(-0.32, 0.32)) +
  geom_text(aes(x = shocks, y = regions, label = sprintf('%.1f', round(change.mean * 100,1))), size = 7) +
  ggtitle('change in household shocks per region, (2014 - 2012)') +
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