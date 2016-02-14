library(extrafont)
loadfonts()
# labels for top of marginal effects graph --------------------------------


target_dist = data.frame(t = 1:11, 
                         c = rep(25, 11), dist = letters[1:11])

tCol = brewer.pal(10, 'Paired')[2]
cCol = brewer.pal(10, 'Paired')[1]

ggplot(target_dist, aes(x = 1, y = 1)) +
  geom_point(aes(size = c*100), colour = cCol) +
  geom_point(aes(size = t*100), colour = tCol) +
  facet_wrap(~dist, ncol = 11) +
  theme_blankLH()

ggsave("~/GitHub/Ethiopia/R/plots/ETH_distBuffer.pdf", 
       width = 4.5, height = 2.75,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# Import results ----------------------------------------------------------


df <- read.csv("~/GitHub/Ethiopia/R/ETH.FtF.results.summary.csv", header=FALSE)



# methods comp ------------------------------------------------------------

methods_numMon = df %>% 
  slice(seq(17,25, by = 2)) %>% 
  select(-V1, -V13)


methods_numMon = data.frame(t(methods_numMon)) %>% 
  mutate(dist = 0:10) 

colnames(methods_numMon) = c('dnd', 'dnd-buff',
                             'dnd-wt', 'dnd-buff-wt', 'psm',
                             'dist')

methods_numMon = methods_numMon %>% 
  gather(question, tScore, -dist) %>% 
  mutate(tScore = -1 * as.double(tScore),
         signif = ifelse(tScore > 1.96, grey90K, grey15K))

ggplot(methods_numMon, aes(x = dist, y = tScore, 
                           group = question, 
                           colour = question,
                           fill = signif)) +
  # contextual background
  geom_hline(yintercept = 1.96, colour = '#ff7f00') +
  annotate(x = 1, y = 2.5, label = 'significance cutoff',
           colour = '#ff7f00', geom = 'text',
           family = "Segoe UI Semilight", size = 4) +

  # points
  geom_smooth(fill= NA) +
  geom_point(size = 4, shape = 21, colour = grey90K) +   
  scale_fill_identity() +
  scale_colour_brewer(palette = "Paired") +
  
  # labels
  ggtitle('Results disagree once weights are added') +
  xlab('treatment buffer (km)') +
  ylab('t-score') +
  
  scale_x_continuous(breaks = seq(0, 11, by = 2)) +
  
  # theme
  theme_bw() +
  theme(
    text = element_text(family = 'Segoe UI Light', colour = grey60K),
    rect = element_blank(),
    plot.background = element_blank(),
    axis.text = element_text(size = 12,  color = grey60K),
    title =  element_text(size = 13, family = "Segoe UI", hjust = 0, color = grey90K),
    axis.title =  element_text(size = 14, family = "Segoe UI Semilight", color = grey60K, hjust = 0.5, vjust = -0.25),
    strip.text = element_text(size=14, face = 'bold', hjust = 0.05, vjust = -2.5, color = '#4D525A'),
    legend.position = 'none',
    strip.background = element_blank(),
    axis.ticks = element_blank(),
    panel.margin = unit(3, 'lines'),
    panel.grid.major.y = element_line(size = 0.2, color = '#bababa'),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank())

  
# Marginal effects graph --------------------------------------------------

me_dnd = df %>%  slice(c(16,30, 111, 124))

me_dnd = data.frame(t(me_dnd)) %>% 
  slice(c(-1, -13)) %>% 
  mutate(dist = 0:10) 

colnames(me_dnd) = c('numberMon', 'Q1', 'Q8', 'Q9', 'dist')

me_dnd = me_dnd %>% 
  gather(question, ME, -dist) %>% 
  mutate(me = as.double(ME))


plotFTFdecay = function(df,
                        plotTitle,
                        yLim = c(-0.2, 0.2),
                        xLim = c(-.1, 10.1),
                        xLabAdj = 0.25,
                        colorBetter = ftfBlue,
                        colorWorse = ftfOrange,
                        smoothSpan = 0.6){
  
  ggplot(df, aes(x = dist, y = -1 * me)) +
    
    # contextual background
    annotate('rect', xmin = xLim[1], xmax = xLim[2], ymin = 0, ymax = yLim[2],
             fill = colorWorse, alpha = 0.25) +
    annotate('text', x = xLim[1] + xLabAdj, y = yLim[2] - 0.01, hjust = 0,
             colour = colorWorse, size = 6, label = 'worse') +
    annotate('rect', xmin = xLim[1], xmax = xLim[2], ymin = yLim[1], ymax = 0,
             fill = colorBetter, alpha = 0.25) +
    annotate('text', x = xLim[1] + xLabAdj, y = yLim[1] + 0.01, hjust = 0,
             colour = colorBetter, size = 6, label = 'better') +
    
    # points
    geom_smooth(fill= NA, colour = grey60K) +
    geom_point(size = 4, colour = grey90K) +    
    
    # labels
    ggtitle(plotTitle) +
    xlab('Treatment buffer (km)') +
    
    scale_x_continuous(breaks = seq(0, 11, by = 2)) +
    
    # theme
    theme_bw() +
    theme(
      text = element_text(family = 'Segoe UI Light', colour = grey60K),
      rect = element_blank(),
      plot.background = element_blank(),
      axis.text = element_text(size = 12,  color = grey60K),
      title =  element_text(size = 15, family = "Segoe UI", hjust = 0, color = grey90K),
      axis.title.x =  element_text(size = 14, family = "Segoe UI Semilight", color = grey60K, hjust = 0.5, vjust = -0.25),
      axis.title.y = element_blank(), 
      strip.text = element_text(size=14, face = 'bold', hjust = 0.05, vjust = -2.5, color = '#4D525A'),
      legend.position = 'none',
      strip.background = element_blank(),
      axis.ticks = element_blank(),
      panel.margin = unit(3, 'lines'),
      panel.grid.major.y = element_line(size = 0.2, color = '#bababa'),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(size = 0.1, color = '#bababa'))
}

plotFTFdecay(me_dnd %>% filter(question == 'numberMon'),
             'Improvement in average number of months',
             yLim = c(.7, -.1))

ggsave("~/GitHub/Ethiopia/R/plots/ETH_FtFdistME_numMon.pdf", 
       width = 4.5, height = 2.75,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       scale = 1.165,
       dpi = 300)


plotFTFdecay(me_dnd %>% filter(question == 'Q1'),
             'Improvement in worries about food security',
             yLim = c(.15, -.15))

ggsave("~/GitHub/Ethiopia/R/plots/ETH_FtFdistME_Q1.pdf", 
       width = 4.5, height = 2.75,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       scale = 1.165,
       dpi = 300)

plotFTFdecay(me_dnd %>% filter(question == 'Q8'),
             'Increase in days without food in house',
             yLim = c(.04, -.04))

ggsave("~/GitHub/Ethiopia/R/plots/ETH_FtFdistME_Q8.pdf", 
       width = 4.5, height = 2.75,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       scale = 1.165,
       dpi = 300)

plotFTFdecay(me_dnd %>% filter(question == 'Q9'),
             'Increase in days without any food',
             yLim = c(.04, -.04))

ggsave("~/GitHub/Ethiopia/R/plots/ETH_FtFdistME_Q9.pdf", 
       width = 4.5, height = 2.75,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       scale = 1.165,
       dpi = 300)



# DND plots ---------------------------------------------------------------
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

q8 = bumpChart(ftfDiffs, var = 'daysNoFoodSupplBin', title = 'Did you have no food of any kind in your household?', ymax = ymax, sizeLab = 4, xAdjLab = 0.3)
ggsave("~/GitHub/Ethiopia/R/plots/ETH_Q8_bumpplot.pdf", plot = q8,
       width = widthFTF, height = heightFTF,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

q9 = bumpChart(ftfDiffs, var = 'daysFastBin', title = 'Did you go a whole day without eating?', ymax = ymax, sizeLab = 4, xAdjLab = 0.3)
ggsave("~/GitHub/Ethiopia/R/plots/ETH_Q9_bumpplot.pdf", plot = q9,
       width = widthFTF, height = heightFTF,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# DND barcharts -----------------------------------------------------------
ftf = data %>% 
  filter(!is.na(ftfzone_5km), !is.na(worryLackFood)) %>% 
  group_by(ftfzone_5km, worryLackFood, year) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  group_by(ftfzone_5km, year) %>% 
  mutate(pct = num/sum(num))


ggplot(ftf, aes(x = worryLackFood, y = num, 
                 fill = factor(ftfzone_5km))) +
  geom_bar(stat = 'identity') + 
  facet_wrap(~ftfzone_5km + year, ncol = 2) +
  coord_flip() +
  theme(title = element_text(size = 32, color = grey90K),
        axis.ticks.x = element_line(color = grey60K, size = 0.5),
        axis.ticks.y = element_blank(),
        legend.position="none",
        panel.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = rep(unit(0, units = 'points'),4),
        panel.grid = element_blank(),
        panel.background = element_blank(), 
        strip.text = element_text(size=13 , color = grey60K, family = 'Segoe UI Semilight'),
        strip.background = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text= element_blank()) +
  scale_fill_manual(values = c('0' = ftfBlue, '1' = ftfOrange)) +
  geom_text(aes(label = num), family = 'Segoe UI Semibold', 
            colour = 'white', size = 2) +
  annotate('text', label = 'yes', x = 1, y = .35,
           family = 'Segoe UI Semilight',
           size = 2, colour = ftfBlue) +
  annotate('text', label = 'no', x = 1, y = .3,
           family = 'Segoe UI Semilight',
           size = 2, colour = ftfBlue) +
  scale_x_discrete(labels = c('verbal', 'physical'))

ggsave("~/GitHub/Ethiopia/R/plots/ETH_Q1_bar.pdf",
       width = 2.5, height = 3,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



ftf = data %>% 
  filter(!is.na(ftfzone_5km), !is.na(numMonthFoodShort)) %>% 
  group_by(ftfzone_5km, x = numMonthFoodShort > 0, year) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  group_by(ftfzone_5km, year) %>% 
  mutate(pct = num/sum(num))


ggplot(ftf, aes(x = x, y = num, 
                fill = factor(ftfzone_5km))) +
  geom_bar(stat = 'identity') + 
  facet_wrap(~ftfzone_5km + year, ncol = 2) +
  coord_flip() +
  theme(title = element_text(size = 32, color = grey90K),
        axis.ticks.x = element_line(color = grey60K, size = 0.5),
        axis.ticks.y = element_blank(),
        legend.position="none",
        panel.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = rep(unit(0, units = 'points'),4),
        panel.grid = element_blank(),
        panel.background = element_blank(), 
        strip.text = element_text(size=13 , color = grey60K, family = 'Segoe UI Semilight'),
        strip.background = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text= element_blank()) +
  scale_fill_manual(values = c('0' = ftfBlue, '1' = ftfOrange)) +
  geom_text(aes(label = num), family = 'Segoe UI Semibold', 
            colour = 'white', size = 2) +
  annotate('text', label = 'yes', x = 1, y = .35,
           family = 'Segoe UI Semilight',
           size = 2, colour = ftfBlue) +
  annotate('text', label = 'no', x = 1, y = .3,
           family = 'Segoe UI Semilight',
           size = 2, colour = ftfBlue) +
  scale_x_discrete(labels = c('verbal', 'physical'))

ggsave("~/GitHub/Ethiopia/R/plots/ETH_numMon_bar.pdf",
       width = 2.5, height = 3,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)




ftf = data %>% 
  filter(!is.na(ftfzone_5km), !is.na(daysNoFoodSupplBin)) %>% 
  group_by(ftfzone_5km, daysNoFoodSupplBin, year) %>% 
  summarise(num = n()) 


ggplot(ftf, aes(x = daysNoFoodSupplBin, y = num, 
                fill = factor(ftfzone_5km))) +
  geom_bar(stat = 'identity') + 
  facet_wrap(~ftfzone_5km + year, ncol = 2) +
  coord_flip() +
  theme(title = element_text(size = 32, color = grey90K),
        axis.ticks.x = element_line(color = grey60K, size = 0.5),
        axis.ticks.y = element_blank(),
        legend.position="none",
        panel.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = rep(unit(0, units = 'points'),4),
        panel.grid = element_blank(),
        panel.background = element_blank(), 
        strip.text = element_text(size=13 , color = grey60K, family = 'Segoe UI Semilight'),
        strip.background = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text= element_blank()) +
  scale_fill_manual(values = c('0' = ftfBlue, '1' = ftfOrange)) +
  geom_text(aes(label = num), family = 'Segoe UI Semibold', 
            colour = 'white', size = 2) +
  annotate('text', label = 'yes', x = 1, y = .35,
           family = 'Segoe UI Semilight',
           size = 2, colour = ftfBlue) +
  annotate('text', label = 'no', x = 1, y = .3,
           family = 'Segoe UI Semilight',
           size = 2, colour = ftfBlue) +
  scale_x_discrete(labels = c('verbal', 'physical'))

ggsave("~/GitHub/Ethiopia/R/plots/ETH_Q8_bar.pdf",
       width = 2.5, height = 3,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



ftf = data %>% 
  filter(!is.na(ftfzone_5km), !is.na(daysFastBin)) %>% 
  group_by(ftfzone_5km, daysFastBin, year) %>% 
  summarise(num = n()) 


ggplot(ftf, aes(x = daysFastBin, y = num, 
                fill = factor(ftfzone_5km))) +
  geom_bar(stat = 'identity') + 
  facet_wrap(~ftfzone_5km + year, ncol = 2) +
  coord_flip() +
  theme(title = element_text(size = 32, color = grey90K),
        axis.ticks.x = element_line(color = grey60K, size = 0.5),
        axis.ticks.y = element_blank(),
        legend.position="none",
        panel.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = rep(unit(0, units = 'points'),4),
        panel.grid = element_blank(),
        panel.background = element_blank(), 
        strip.text = element_text(size=13 , color = grey60K, family = 'Segoe UI Semilight'),
        strip.background = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text= element_blank()) +
  scale_fill_manual(values = c('0' = ftfBlue, '1' = ftfOrange)) +
  geom_text(aes(label = num), family = 'Segoe UI Semibold', 
            colour = 'white', size = 2) +
  annotate('text', label = 'yes', x = 1, y = .35,
           family = 'Segoe UI Semilight',
           size = 2, colour = ftfBlue) +
  annotate('text', label = 'no', x = 1, y = .3,
           family = 'Segoe UI Semilight',
           size = 2, colour = ftfBlue)

ggsave("~/GitHub/Ethiopia/R/plots/ETH_Q9_bar.pdf",
       width = 2.5, height = 3,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)# Exploration: why is wealth so different in FtF zones at baseline in Ethiopia?
# Laura Hughes, lhughes@usaid.gov, September 2015.

# Load data ---------------------------------------------------------------
setwd("~/GitHub/Ethiopia/")
source("R/setupFncns.r")


# price = read.csv('Analysis/priceShkWide.txt', skip = 1 , sep = '\t')

source("~/GitHub/Ethiopia/R/loadETHpanel.r")

vars2test =c('ax', 'bed', 'bike','blanket', 'car', 'cart', 'clothing', 'dungFuel', 'dvd',
                'elecLight', 'fireLight', 'flushToilet', 'indoorKitchen', 'jewel', 'metalRoof',
'mitad', 'mobile', 'moto', 'mudFloor', 'mudHome', 'noKitchen', 'hasToilet',
'ownHouse', 'phone', 'plough', 'protWaterDry', 'protWaterRainy', 'pump', 'radio', 
'refrig', 'sat', 'sew', 'shelf', 'sickle', 'sofa', 'stoneHome', 'stove', 'thatchRoof',
'tv', 'watch', 'weave', 'well', 'wasteFert', 'wasteThrow', 'roomsPC')

tests = NA

for (varName in vars2test){
  
  print(varName)
  
  ctrl = data %>% 
    filter(year == 2012, ftfzone_5km == 0) %>% 
    select_(varName)
  
  ctrl = ctrl[,1]
  
  ftf = data %>% 
    filter(year == 2012, ftfzone_5km == 1) %>% 
    select_(varName)
  
  ftf = ftf[,1]
  
  tests= rbind(tests,broom::tidy(t.test(ctrl, ftf)))
}


tests = tests %>% 
  mutate(signif = ifelse(p.value < 0.05, TRUE, FALSE),
         variable = c('NA', vars2test),
  sign = ifelse(estimate2 < estimate1, 'lower', 'higher'),
  diff = estimate2 - estimate1,
  pct = (estimate2-estimate1)/estimate1)

tests =tests  %>% 
  select(variable, sign, signif, statistic, p.value, estimate1, estimate2, diff, pct)

write.csv(tests, 'ETH_FtF_baseline_wealthDiffs.csv')


# other baseline vals -----------------------------------------------------

data %>% 
  filter(year == 2012) %>% 
  group_by(ftfzone_5km) %>% 
  summarise(mean(agehead, na.rm = T))

data %>% 
  filter(year == 2012) %>% 
  group_by(ftfzone_5km) %>% 
  summarise(mean(femhead, na.rm = T))

data %>% 
  filter(year == 2012) %>% 
  group_by(ftfzone_5km, eduMcat) %>% 
  summarise(num = n()) %>% 
  mutate(pct = num/sum(num))


data %>% 
  filter(year == 2012) %>% 
  group_by(ftfzone_5km) %>% 
  summarise(sum(wlthSmooth, na.rm = T), n())
setwd("~/GitHub/Ethiopia/")
source("R/setupFncns.r")

setwd("~/Documents/USAID/Ethiopia/Dataout/")

dataRaw = read_dta("ETH_201508_analysis_panel.dta")

data = removeAttributes(dataRaw)

childRaw = read_dta("ETH_201508_Child_Analysis.dta")

child = removeAttributes(childRaw)

warning('filtering out non-panel households and children.')

data = data %>% 
  filter(ptrack == 2) %>% 
  mutate(religion = ifelse(religHoh == 1, 
                           "Orthodox",
                           ifelse(religHoh == 3,
                                  "Protestant",
                                  ifelse(religHoh == 4,
                                         "Muslim",
                                         ifelse(religHoh == 7 | religHoh == 2 | religHoh == 6 | religHoh == 5,
                                                "other", NA)))),
         regionName = ifelse(
           saq01 == 2 , "Afar",
           ifelse(saq01 == 5, "Somalie", 
                  ifelse(saq01 == 6, "Benshagul Gumuz",
                         ifelse(saq01 == 12, "Gambella",
                                ifelse(saq01 == 13, "Harari",
                                       ifelse(saq01 == 15, "Diredawa",
                                              ifelse(saq01 == 1, "Tigray",
                                                     ifelse(saq01 == 3, "Amhara",
                                                            ifelse(saq01 == 4, "Oromia",
                                                                   ifelse(saq01 == 7, "SNNP", 
                                                                          ifelse(saq01 == 14, "Addis Ababa", "unknown")
                                                                   )))))))))),
         eduFcat = ifelse(educAdultF == 0, 'no education',
                          ifelse((educAdultF == 1 | educAdultF == 2 | educAdultF == 3), 'primary',
                                 ifelse((educAdultF == 4 | educAdultF == 5), 'secondary',
                                        ifelse(educAdultF == 6, 'tertiary', NA)))),
         eduMcat = ifelse(educAdultM == 0, 'no education',
                          ifelse((educAdultM == 1 | educAdultM == 2 | educAdultM == 3), 'primary',
                                 ifelse((educAdultM == 4 | educAdultM == 5), 'secondary',
                                        ifelse(educAdultM == 6, 'tertiary', NA)))),
         ageLinear = cut(agehead, c(8, 25, 35, 45, 55, 100))
         # dist_FTFzone_smooth = ntile(dist_FTFzone, 6)
         ) 
# %>% 
#   group_by(household_id)  %>% 
#   mutate(TLU_lagged = lag(TLUtotal, order_by = year))


child = child %>% 
  filter(ptrack == 2) %>% 
  mutate(religion = ifelse(religHoh == 1, 
                           "Orthodox",
                           ifelse(religHoh == 3,
                                  "Protestant",
                                  ifelse(religHoh == 4,
                                         "Muslim",
                                         ifelse(religHoh == 7 | religHoh == 2 | religHoh == 6 | religHoh == 5,
                                                "other", NA)))),
         regionName = ifelse(
           saq01 == 2 , "Afar",
           ifelse(saq01 == 5, "Somalie", 
                  ifelse(saq01 == 6, "Benshagul Gumuz",
                         ifelse(saq01 == 12, "Gambella",
                                ifelse(saq01 == 13, "Harari",
                                       ifelse(saq01 == 15, "Diredawa",
                                              ifelse(saq01 == 1, "Tigray",
                                                     ifelse(saq01 == 3, "Amhara",
                                                            ifelse(saq01 == 4, "Oromia",
                                                                   ifelse(saq01 == 7, "SNNP", 
                                                                          ifelse(saq01 == 14, "Addis Ababa", "unknown")
                                                                   )))))))))),
         eduFcat = ifelse(educAdultF == 0, 'no education',
                          ifelse((educAdultF == 1 | educAdultF == 2 | educAdultF == 3), 'primary',
                                 ifelse((educAdultF == 4 | educAdultF == 5), 'secondary',
                                        ifelse(educAdultF == 6, 'tertiary', NA)))),
         eduMcat = ifelse(educAdultM == 0, 'no education',
                          ifelse((educAdultM == 1 | educAdultM == 2 | educAdultM == 3), 'primary',
                                 ifelse((educAdultM == 4 | educAdultM == 5), 'secondary',
                                        ifelse(educAdultM == 6, 'tertiary', NA))))
  )
# Merge various modules from LS/MS data on Ethiopia together at the household level.
# 
# Variables:
# 
#                         - hhID2012: household ID from 2012 surveys
#                         - hhID2014: household ID from 2014 surveys
#                         - year: year of survey (2012 or 2014)
#                         - regionComb: string containing the regions in the 
#                                       survey, grouped by the 7 largest.

# Nutrition:
#                         - dd2012B:  dietary diversity from 2012, calculated
#                                     using survey module 5B (7 day recall) 
#                         - dd2014B:  dietary diversity from 2014, calculated
#                                     using survey module 5B (7 day recall)
#                         - fcs2012Min:  food consumption score from 2012, 
#                                        calculated using survey module 5B 
#                                        using the MOST conservative estimate 
#                                        of consumption.
#                         - fcs2012Max:  food consumption score from 2012, 
#                                        calculated using survey module 5B 
#                                        using the LEAST conservative estimate 
#                                        of consumption.
#                         - fcs2012MinCat: factor characterization of FCS score.
#                         - fcs2012MaxCat: factor characterization of FCS score.
#                         - fcs2014Min:  food consumption score from 2014, 
#                                        calculated using survey module 5B 
#                                        using the MOST conservative estimate 
#                                        of consumption.
#                         - fcs2014Max:  food consumption score from 2014, 
#                                        calculated using survey module 5B 
#                                        using the LEAST conservative estimate 
#                                        of consumption.
#                         - fcs2014MinCat: factor characterization of FCS score.
#                         - fcs2014MaxCat: factor characterization of FCS score.

# Food Security:
#                         - worryLackFood: binary value if the hh worried they would lack food in the last 7 days
#                         - daysEatBadFood: days in the past week the hh ate less preferred foods 
#                         - daysLimitVariety: days in the past week the hh limited the variety of food they ate
#                         - daysRedAmt: days in the past week the hh reduced the amount of food consumed
#                         - daysRedNumMeals: days in the past week the hh reduced the number of meals conusmed
#                         - daysRedAdultIntake: days in the past week the hh reduced adult food intake to provide more food for kids    
#                         - daysBorrowFood: days in the past week the hh borrowed food from friends/neighbors
#                         - daysNoFoodSuppl: days in the past week the hh had no food in the house
#                         - daysFast: days in the past week the hh went 24 h without food
#                         - avgNumMealsAdults: average number of daily meals for adults in the hh
#                         - avgNumMealsKids: average number of daily meals for kids in the hh
#                         - hhSameDiet: does the hh all eat the same diet?            
#                         - menDietDiv: categorical value: do men eat more (+1), less (-1), or the same (0) diverse diets?           
#                         - womenDietDiv: categorical value: do women eat more (+1), less (-1), or the same (0) diverse diets?          
#                         - kidsDietDiv: categorical value: do children eat more (+1), less (-1), or the same (0) diverse diets?           
#                         - foodShortSit: binary value: did the hh experience a food shortage situation in the past year?
#                         - janFoodShort: binary value: did the hh experience a food shortage situation in January?  
#                         - febFoodShort: binary value: did the hh experience a food shortage situation in February?
#                         - marFoodShort: binary value: did the hh experience a food shortage situation in March?
#                         - aprFoodShort: binary value: did the hh experience a food shortage situation in April?          
#                         - mayFoodShort: binary value: did the hh experience a food shortage situation in May?         
#                         - juneFoodShort: binary value: did the hh experience a food shortage situation in June?
#                         - julyFoodShort: binary value: did the hh experience a food shortage situation in July?
#                         - augFoodShort: binary value: did the hh experience a food shortage situation in August?
#                         - septFoodShort: binary value: did the hh experience a food shortage situation in September
#                         - octFoodShort: binary value: did the hh experience a food shortage situation in October?
#                         - novFoodShort: binary value: did the hh experience a food shortage situation in November?
#                         - decFoodShort: binary value: did the hh experience a food shortage situation in December?
#                         - causeShort1: primary cause for the food shortages over the past year
#                         - causeShort1other: primary cause for the food shortages over the past year, if "other" (2014 only)    
#                         - causeShort2: secondary cause for the food shortages over the past year
#                         - causeShort2other: secondary cause for the food shortages over the past year, if "other" (2014 only)      
#                         - causeShort3: tertiary cause for the food shortages over the past year
#                         - causeShort3other: tertiary cause for the food shortages over the past year, if "other" (2014 only)     
#                         - daysEatBadFoodBin: binary value if the hh ate less preferred foods in the past week
#                         - daysLimitVarietyBin: binary value if the hh ate less varied foods in the past week
#                         - daysRedAmtBin: binary value if the hh reduced the amount of foods consumed in the past week
#                         - daysRedNumMealsBin: binary value if the hh reduced the number of meals in the past week
#                         - daysRedAdultIntakeBin: binary value if the hh reduced adult consumption in the past week
#                         - daysBorrowFoodBin: binary value if the hh borrowed food in the past week
#                         - daysNoFoodSupplBin: binary value if the hh lacked food in the house in the past week
#                         - daysFastBin: binary value if the hh fasted in the past week
#                         - menDietDivCat: categorical data if the men in the hh eat more, less, or same diverse diets
#                         - womenDietDivCat: categorical data if the women in the hh eat more, less, or same diverse diets       
#                         - kidsDietDivCat: categorical data if the kids in the hh eat more, less, or same diverse diets        
#                         - causeShort1cat: categorical data for causeShort1 (see LS/MS documentation for codes)        
#                         - causeShort2cat: categorical data for causeShort2 (see LS/MS documentation for codes)        
#                         - causeShort3cat: categorical data for causeShort3 (see LS/MS documentation for codes)       
#                         - numMonthFoodShort: number of months over the past year with at least one food shortage situation      
#                         - maxDaysLimit: maximum number of days in the past week the hh changed their food consumption (from "days..." vars)  

# Agriculture:            -"certificate": number of parcels containing a certificate of ownership.

                          
#                         - "right2Sell": number of parcels where the hh has the right to sell them (only 2014)
#                         - "tenured": number of parcels containing tenured land--  inherited, rented, or granted by local leaders
#                         - "numParcels": number of parcels owned by the hh
#                         - "pctCert": percent of parcels with a certificate
#                         - "pctRight2Sell": percent of parcels with the right to sell.
#                         - "pctTenured": percent of parcels that are tenured
#                         - "avgField": average field size across all the parcels.
#                         - "areaField": sum of all the parcels sizes, in m^2
#                         - "irrig": number of parcels with irrigation 
#                         - "fertilizer": number of parcels using any type of fertilizer
#                         - "urea": number of parcels using urea          
#                         - "DAP": number of parcels using DAP
#                         - "inorgFert": number of parcels using any other inorganic fertilizer (2014 only)
#                         - "manure": number of parcels using manure       
#                         - "compost": number of parcels using compost      
#                         - "orgFert": number of parcels using other organic fertilizer       
#                         - "extPgrm": number of parcels participating in the extension program (2014 only)       
#                         - "numFields": number of fields per household
#                         - "anyInorg": binary value if the household is using any inorganic fertilizer (urea, DAP, other inorganic fertilizer) on any of their parcels      
#                         - "anyOrg": binary value if the household is using any organic fertilizer (manure, compost, other organic fertilizer) on any of their parcels 
#                         - "fertPct": percent of parcels using any fertilizer
#                         - "ureaPct": percent of parcels using urea
#                         - "DAPpct": percent of parcels using DAP
#                         - "manurePct": percent of parcels using manure

# Housing:
#                         - hhExist: did the hh exist 12 months ago? (2012 only)
#                         - "yrsInDwelling": number of years the hh lived in the dwelling
#                         - "moInDwelling": months the hh lived in the dwelling
#                         - "rentOrOwn": categorical data: does the hh rent, own, live free of rent, or other in the dwelling
#                         - "numRooms": number of rooms in the dwelling
#                         - "wallMaterial": categorical data for what the walls of the house are made of
#                         - "roofMaterial": categorical data for what the roof of the house is made of    
#                         - "floorMaterial": categorical data for what the floors are made of
#                         - "typeKitchen":  categorical data for what type of kitchen is in the house
#                         - "ovenType": oven type         
#                         - "toilet": toilet type           
#                         - "bathType": bath type         
#                         - "solidWaste": type of solid waste facilities       
#                         - "sourceWaterRainy": source of water in the rainy season
#                         - "time2WaterRainy": amount of time it takes to get water in the rainy season (2014 only) 
#                         - "sourceWaterDry": source of water in the dry season
#                         - "time2WaterDry": amount of time it takes to get water in the dry season (2014 only)    
#                         - "costWater": average cost of water (2014 only)        
#                         - "boilWater": does the hh typically boil water before drinking?
#                         - "chlorinateWater": does the hh typicallly use chemical means to purify drinking water?  
#                         - "ownOtherHome": does the hh own another home?
#                         - "numHomes": number of homes owned by hh         
#                         - "sourceLight": source of light in the hh     
#                         - "sourceElec": source of electricity (2014 only)      
#                         - "costElec": cost of electricity  (2014 only)        
#                         - "timeElecFail": frequency of electrical disruption in the past week     
#                         - "ElecFailLastWeek": hours without electricity last week (2014 only)
#                         - "cookingFuel": type of cooking fuel used      
#                         - "ownCellPhone": does anyone own a cell phone? (2014 only)    
#                         - "costPhone": cost of cell phone / landline per month (2014 only)        
#                         - "toiletCat": categorical values for toilet type       
#                         - "waterCatDry": categorical value for water source in dry season      
#                         - "waterCatRainy": categorical value for water source in the rainy season     
#                         - "anyWaterTreat": binary value if the hh boils and/or chemically treates water  


# Set working directory to where the Ethiopia data is located.
setwd("~/Documents/USAID/Ethiopia/")
source("R/setupFncns.r")
masterDir = getwd()


# Base --------------------------------------------------------------------

base = read_dta("Dataout/hh_base.dta")

base = removeAttributes(base)

baseIDs = base %>% select(hhID2012 = household_id, hhID2014 = household_id2, year)

newbase = read_dta("Datain/wave2014/Pub_ETH_HouseholdGeovars_Y2.dta")


# Agriculture (land holdings and tenure; Mods 1-3 post-planting) ----------------------------------
source("R/agriculture module/agMod.r")

# Add in necessary columns to merge
hhAg2012 = hhAg2012 %>% 
  ungroup() %>% 
  mutate(year = 2012, right2Sell = NA, pctRight2Sell = NA, inorgFert = NA)


hhAg2014 = hhAg2014 %>%
  ungroup() %>% 
  select(-hhID2012) %>% 
  mutate(year = 2014) 

# Merge with base to get hhID2012 and hhID2014 for all.
agr12 = full_join(baseIDs, hhAg2012, by = c("hhID2012", "year")) %>% 
  filter(year == 2012)
agr14 = full_join(baseIDs, hhAg2014, by = c("hhID2014", "year")) %>% 
  filter(year == 2014)

agr = rbind(agr12, agr14)



# Food Security (Mod 7) ---------------------------------------------------
setwd(masterDir)

source("R/food security module/foodSecMod.r")

foodSec2012 = foodSec2012 %>% 
  select(-eaID2012,-region, -rural, -hhWeight, -ea, -hhIDTrunc, 
         -zone, -woreda, -town, -subcity, -kebele, -regionComb) %>% 
  mutate(year = 2012, causeShort1other = NA, causeShort2other = NA, causeShort3other = NA) 
foodSec2014 = foodSec2014 %>% 
  select(-hhID2012, -eaID2012, -eaID2014, -region, -rural, -hhWeight, 
                                     -ea, -hhIDTrunc,  -zone, -woreda, -town, -subcity, -kebele, -regionComb) %>% 
  mutate(year = 2014)



foodSec2012 = full_join(baseIDs, foodSec2012, by = c("hhID2012", "year")) %>% 
                          filter(year == 2012)

foodSec2014 = full_join(baseIDs, foodSec2014, by = c("hhID2014", "year")) %>% 
  filter(year == 2014)

foodSec = rbind(foodSec2012, foodSec2014)


# Housing (Mod 9) ---------------------------------------------------------

setwd(masterDir)
source("R/housing module/housingMod.r")

housing2012 = housing2012 %>% 
  select(-hhWeight, -eaID2012, -rural, -hhTrunc, -rural, 
         -zone, -woreda, -town, -subcity, -kebele, -ea, -regionComb) %>% 
  mutate(year = 2012, time2WaterRainy = NA, time2WaterDry = NA, costWater = NA,
         sourceElec = NA, costElec = NA, ElecFailLastWeek = NA, ownCellPhone = NA, costPhone = NA)

housing2014 = housing2014 %>% select(-hhID2012, -eaID2012, -rural, -eaID2014, 
                                     -hhWeight, -hhTrunc, -rural, -zone, -woreda, -town, -subcity, -kebele, -ea, -regionComb) %>% 
  mutate(year = 2014, hhExist = NA)


housing2012 = full_join(baseIDs, housing2012, by = c("hhID2012", "year")) %>% 
                          filter(year == 2012)
                        
housing2014 = full_join(baseIDs, housing2014, by = c("hhID2014", "year")) %>% 
                          filter(year == 2014)

housing = rbind(housing2012, housing2014)


# Nutrition (Mod 5B) ---------------------------------------------------------------


source("R/nutrition analysis/calcFCSEthiopia.R")

code = "~/Documents/USAID/Ethiopia/R/nutrition analysis/"
w1 = "~/Documents/USAID/Ethiopia/Datain/wave2012"
w2 = "~/Documents/USAID/Ethiopia/Datain/wave2014"

calcFCSEthiopia(code, w1, w2)

dd2012B = dd2012B %>% 
  mutate(year = 2012)

dd2014B = dd2014B %>% 
  mutate(year = 2014)

dd2012B = full_join(baseIDs, dd2012B, by = c("hhID2012", "year")) %>% 
  mutate(dd = dd2012B) %>% 
  select(-dd2012B) %>% 
  filter(year == 2012)

dd2014B = full_join(baseIDs, dd2014B, by = c("hhID2014", "year")) %>% 
  mutate(dd = dd2014B) %>% 
  select(-dd2014B) %>% 
  filter(year == 2014)

fcs2012 = hhAggr2012 %>% 
  mutate(fcsMin = fcsMin2012, fcsMax = fcsMax2012, fcsCatMin = fcsCatMin2012, fcsCatMax = fcsCatMax2012) %>% 
  select(-fcsMin2012, -fcsCatMin2012, -fcsMax2012, -fcsCatMax2012)

fcs2014 = hhAggr2014%>% 
  mutate(fcsMin = fcsMin2014, fcsMax = fcsMax2014, fcsCatMin = fcsCatMin2014, fcsCatMax = fcsCatMax2014) %>% 
  select(-fcsMin2014, -fcsCatMin2014, -fcsMax2014, -fcsCatMax2014)
  

fcsDD12 = full_join(dd2012B, fcs2012, c("hhID2012", "Beans, lentils, nuts", "Beef, sheep, goat, or other re", "Eggs", 
                                        "Enjera (teff)", "Fish", "Fruits", "Kocho/Bula", "Milk/yogurt/cheese/other dairy", 
                                        "Oils/fats/butter", "Other cereal", "Other condiments", "Pasta, Macaroni and Biscuits", "Potatoes", "Poulty", 
                                        "Sugar or sugar products", "Vegetables", "pulses", "veg", "fruit", "milk", "sugar", "oils" = "oil"))

fcsDD14 = full_join(dd2014B, fcs2014, c("hhID2014", "Beans, lentils, nuts", "Beef, sheep, goat, or other re", "Eggs", 
                                        "Enjera (teff)", "Fish", "Fruits", "Kocho/Bula", "Milk/yogurt/cheese/other dairy", 
                                        "Oils/fats/butter", "Other cereal", "Other condiments", "Pasta, Macaroni and Biscuits", "Potatoes", "Poulty", 
                                        "Sugar or sugar products", "Vegetables", "pulses", "veg", "fruit", "milk", "sugar", "oils" = "oil"))

fcsDD = rbind(fcsDD12, fcsDD14)


# Geo ---------------------------------------------------------------------


# # Read in geo data.
# # ftfZones = read.csv("~/GitHub/Ethiopia/Data/ETH_FTF_LSMS_join.csv")
ftfZones = fread("~/GitHub/Ethiopia/Data/ETH_FTF_LSMS_join.csv")

geoData = read_dta("Dataout/geovars.dta")
geoData = removeAttributes(geoData)


# Merge in the geoData with the FtF status.

geoFtF = full_join(geoData, ftfZones, by = c("latitude" = "latitude", "longitude" = "longitude"))



# Merge -------------------------------------------------------------------

hh_laura = full_join(baseIDs, fcsDD, by = c("hhID2012", "hhID2014", "year"))
hh_laura = full_join(hh_laura, foodSec, by = c("hhID2012", "hhID2014", "year"))
hh_laura = full_join(hh_laura, agr, by = c("hhID2012", "hhID2014", "year"))
hh_laura = full_join(hh_laura, housing, c("hhID2012", "hhID2014", "year"))

hh_laura = hh_laura %>% 
  mutate(household_id = as.character(hhID2012), household_id2 = as.character(hhID2014)) %>% 
  select(-hhID2014, -hhID2012)


write_dta(hh_laura, "hh_laura.dta")

write.csv(hh_laura, "hh_laura.csv")
# Ethiopia LS/MS analysis
#
# Function to import data on child level stunting and merge with the household-level data.
#
# Laura Hughes, lhughes@usaid.gov, July 2015


# Import data -------------------------------------------------------------
setwd("~/GitHub/Ethiopia/")

source("R/food security module/setupFncns.r")

# Load inidividual education data.
setwd("~/GitHub/Ethiopia/")
source("R/educationMod.R")

indivEd = indivEd %>% 
  mutate(household_id = hhID2012, household_id2 = hhID2014, individual_id = indivID2012, individual_id2 = indivID2014) %>% 
  select(-hhID2012, -hhID2014, -hhWeight, -hhIDTrunc, -indivID2012, -indivID2014,
         -rural, -region, -regionComb)

setwd("~/GitHub/Ethiopia/")
hhRaw = read_dta("Data/ETH_201507_LSMS_All.dta") 

hh =  removeAttributes(hhRaw) %>% 
  mutate(stuntingHH = stunting, underweightHH = underweight, wastingHH = wasting, BMIhh = BMI, numChildHH = childTag) %>% 
  select(-stunting, -underweight, -wasting, -BMI, -X_merge, -childTag)

childRaw = read_dta("Data/ETH_201506_cHealth.dta")

childHealth = removeAttributes(childRaw)

childHealth = childHealth %>% 
#   mutate(household_id = as.character(household_id), household_id2 = as.character(household_id2),
#          ea_id = as.character(ea_id), ea_id2 = as.character(ea_id2)) %>% 
  select(-X_merge, -hid)



# Merge data --------------------------------------------------------------

# Merge education / individual data with child health data.
indiv_ed_health = full_join(childHealth, indivEd, 
                            by = c("household_id", "household_id2", "individual_id", "individual_id2", "year"))


# Merge with hh data.
indiv = full_join(hh, indiv_ed_health, by = c("household_id",
                                          "year"))
#                                           , "pw", "saq02", "saq03", "saq04", "saq05", "saq06", "saq07", 
#                                           "saq08", "ptrack", "pw2", "illness", 
#                                           "totIllness", "malariaHH", "diarrheaHH", "respInfection", "chDiarrhea"))

child = indiv %>% 
  filter(childTag == 1)



# Exploratory plots -------------------------------------------------------

colors = brewer.pal(11, "Spectral")

ggplot(child, aes(x = BMI, y = stunting)) +
  theme_laura() + 
  geom_point(size = 4, alpha = 0.3, color = colors[sample(11,1)])

ggplot(child, aes(x = underweight, y = stunting)) +
  theme_laura() + 
  geom_point(size = 4, alpha = 0.3, color = colors[sample(11,1)])


ggplot(child, aes(x = numChildHH, y = stunting)) +
  theme_laura() + 
  geom_point(size = 4, alpha = 0.3, color = colors[sample(11,1)])

ggplot(child, aes(x = fcsMin, y = stunting)) +
  theme_laura() + 
  facet_wrap(~year) +
  geom_point(size = 4, alpha = 0.3, color = colors[sample(11,1)])

ggplot(child, aes(x = noToilet, y = stunting)) +
  theme_laura() + 
  facet_wrap(~year) +
  geom_jitter(size = 4, alpha = 0.3, color = colors[sample(11,1)])


ggplot(childHealth, aes(x = ageMonths, y = stunting)) +
  theme_laura() + 
  facet_wrap(~year) +
  geom_point(size = 4, alpha = 0.3, color = colors[sample(11,1)])

ggplot(childHealth, aes(x = ageMonths, y = stunting)) +
  theme_laura() + 
  facet_wrap(~ year + gender) +
  geom_smooth()

ggplot(childHealth, aes(x = ageMonths, y = stunting)) +
  theme_laura() + 
  facet_wrap(~ year + gender) +
  geom_smooth()

ggplot(childHealth, aes(x = ageMonths, y = stunted, fill = factor(year))) +
  theme_laura() +
  facet_wrap(~ region) +
  geom_smooth()

ggplot(childHealth, aes(x = ageMonths, y = stunted, fill = factor(gender))) +
  theme_laura() +
  facet_wrap(~ region) +
  geom_smooth()
  
ggplot(childHealth, aes(x = ageMonths, y = stunting, color = year)) +
  theme_laura() 
  


code2Cat = function(df, dict, oldVar, newVarName){
  # Assumes that the dictionary will be a 2-column data frame with key in first column and value in the second.
  # Third column is an optional column explaining the relationship.  Should be some sort of logical operator, 
  # e.g. '==', '<', '<=', ...
  
  if(ncol(dict) == 2) {
    # Assumes all relationships are equalities
    dict = dict %>% mutate(rel = '==')
  } else if (ncol(dict) != 3) {
    stop('Not enough columns in dictionary.  Dictionary should be 2 or 3 columns.')
  }
  
  nestedCondit = ""
  
  for (i in 1:nrow(dict)) {
    nestedCondit = paste0(nestedCondit, 
                          'ifelse(', oldVar, dict[i,3], ' "', dict[i,1], '" , "', dict[i,2], '" , ')
  }
  
  closure = paste(rep(')', nrow(dict)), collapse = '')
  
  nestedCondit = paste0(nestedCondit, "NA", 
                        closure)
  
  newDF = mutate_(df, .dots= setNames(list(nestedCondit), newVarName))
  
  return(newDF)
}

     
# Introduction ------------------------------------------------------------
# Within the Ethiopia LSMS panel data between 2011/2012 and 2013/2014, 
# we noticed inconsistencies in children's ages and sexes between years.
# Children reported as being male in the first panel would be reported as 
# female in the second year, and their ages would change by an amount much
# smaller or larger than 24 months.
#
# This script is an attempt to document and, if possible, correct those data.
#
# Laura Hughes, November 2015, lhughes@usaid.gov
# https://github.com/flaneuse
# 
# Data source: http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/EXTLSMS/0,,contentMDK:23406371~pagePK:64168445~piPK:64168309~theSitePK:3358997,00.html




# Import cleaned data (to harvest ptrack, etc.) ------------------------------
source("~/GitHub/Ethiopia/R/setupFncns.r") 
source("~/GitHub/Ethiopia/R/loadETHpanel.r")


# 2012 --------------------------------------------------------------------

setwd("~/Documents/USAID/Ethiopia/Datain/wave2012/")

#_________________________________________________________________________________
# READ in raw data: cover
#_________________________________________________________________________________

rawCover2012 = read_dta('sect_cover_hh_w1.dta')

coverQuest2012 <- pullAttributes(rawCover2012)

cover2012 = removeAttributes(rawCover2012)


#_________________________________________________________________________________
# READ in data: health (--> stunting)
#_________________________________________________________________________________

rawHealth2012 = read_dta('sect3_hh_w1.dta')

healthQuest2012 <- pullAttributes(rawHealth2012)

health2012 = removeAttributes(rawHealth2012)


#_________________________________________________________________________________
# READ in data: stunting
#_________________________________________________________________________________

rawRoster2012 = read_dta('sect1_hh_w1.dta')

rosterQuest2012 <- pullAttributes(rawRoster2012)

roster2012 = removeAttributes(rawRoster2012)


# 2014 --------------------------------------------------------------------

setwd("~/Documents/USAID/Ethiopia/Datain/wave2014/")

#_________________________________________________________________________________
# READ in data: cover
#_________________________________________________________________________________

rawCover2014 = read_dta('sect_cover_hh_w2.dta')

coverQuest2014 <- pullAttributes(rawCover2014)

cover2014 = removeAttributes(rawCover2014)


#_________________________________________________________________________________
# READ in data: health (--> stunting)
#_________________________________________________________________________________

rawHealth2014 = read_dta('sect3_hh_w2.dta')

healthQuest2014 <- pullAttributes(rawHealth2014)

health2014 = removeAttributes(rawHealth2014)


#_________________________________________________________________________________
# READ in data: roster
#_________________________________________________________________________________

rawRoster2014 = read_dta('sect1_hh_w2.dta')

rosterQuest2014 <- pullAttributes(rawRoster2014)

roster2014 = removeAttributes(rawRoster2014)



# Pull out relevant vars --------------------------------------------------
health2012 = health2012 %>% 
  mutate(household_id = as.character(household_id), individual_id = as.character(individual_id)) %>% 
  select(household_id, individual_id,
         ea_id = ea_id, hhID_health = hh_s3q00,
         under5 = hh_s3q20, birthDay_health = hh_s3q21_a, 
         birthMonth_health = hh_s3q21_b, birthYear_health = hh_s3q21_c,
         weight = hh_s3q22, height = hh_s3q23, resultMeas = hh_s3q24) %>% 
  mutate(year = 2012) %>% 
  filter(!is.na(weight)) # Filter out only those children who have a reported weight.

health2014 = health2014 %>% 
  mutate(household_id = as.character(household_id), individual_id = as.character(individual_id), 
         household_id2 = as.character(household_id2), individual_id2 = as.character(individual_id2)) %>% 
  select(household_id, household_id2, individual_id, individual_id2,
         ea_id = ea_id, ea_id2 = ea_id2, hhID2_health = hh_s3q00, 
         under5 = hh_s3q20, birthDay_health = hh_s3q21_a, 
         birthMonth_health = hh_s3q21_b, birthYear_health = hh_s3q21_c,
         weight = hh_s3q22, height = hh_s3q23, resultMeas = hh_s3q24) %>% 
  mutate(year = 2014) %>% 
  filter(!is.na(weight)) # Filter out only those children who have a reported weight.

cover2012 = cover2012 %>% 
  mutate(household_id = as.character(household_id)) %>% 
select(household_id,
           ea_id = ea_id,  hhID_cover = saq08,
         hhSize = hh_saq09, 
         interview1Day = hh_saq13_a, interview1Month = hh_saq13_b, interview1Year = hh_saq13_c,
         interview2Day = hh_saq17_a, interview2Month = hh_saq17_b, interview2Year = hh_saq17_c,
         interview3Day = hh_saq21_a, interview3Month = hh_saq21_b, interview3Year = hh_saq21_c,
         missing1Cover = hh_saq16_a, missing1Roster = hh_saq16_b, missing1Health = hh_saq16_d,
         missing2Cover = hh_saq20_a, missing2Roster = hh_saq20_b, missing2Health = hh_saq20_d,
         missing3Cover = hh_saq24_a, missing3Roster = hh_saq24_b, missing3Health = hh_saq24_d) %>% 
  mutate(year = 2012, 
         interviewYearHealth = ifelse(missing3Health == 'X', NA,
                                      ifelse(missing2Health == 'X', interview3Year,
                                             ifelse(missing1Health == 'X', interview2Year,
                                                    interview1Year))),
         interviewMonthHealth = ifelse(missing3Health == 'X', NA,
                                       ifelse(missing2Health == 'X', interview3Month,
                                              ifelse(missing1Health == 'X', interview2Month,
                                                     interview1Month)))
  ) 

cover2014 = cover2014 %>% 
  mutate(household_id = as.character(household_id), 
         household_id2 = as.character(household_id2)) %>% 
  select(household_id, household_id2,
         ea_id = ea_id, ea_id2 = ea_id2, hhID2_cover = saq08,
         hhSize = hh_saq09, 
         interview1Day = hh_saq13_a, interview1Month = hh_saq13_b, interview1Year = hh_saq13_c,
         interview2Day = hh_saq17_a, interview2Month = hh_saq17_b, interview2Year = hh_saq17_c,
         interview3Day = hh_saq21_a, interview3Month = hh_saq21_b, interview3Year = hh_saq21_c,
         missing1Cover = hh_saq16_a, missing1Roster = hh_saq16_b, missing1Health = hh_saq16_d,
         missing2Cover = hh_saq20_a, missing2Roster = hh_saq20_b, missing2Health = hh_saq20_d,
         missing3Cover = hh_saq24_a, missing3Roster = hh_saq24_b, missing3Health = hh_saq24_d) %>% 
  mutate(year = 2014,          
         interviewYearHealth = ifelse(missing3Health == 'X', NA,
                                      ifelse(missing2Health == 'X', interview3Year,
                                             ifelse(missing1Health == 'X', interview2Year,
                                                    interview1Year))),
         interviewMonthHealth = ifelse(missing3Health == 'X', NA,
                                       ifelse(missing2Health == 'X', interview3Month,
                                              ifelse(missing1Health == 'X', interview2Month,
                                                     interview1Month)))) 

roster2012= roster2012 %>% 
  mutate(household_id = as.character(household_id), individual_id = as.character(individual_id)) %>% 
  select(household_id, individual_id,
         ea_id = ea_id,  hhID_roster = saq08,
         hhMemberID_roster = hh_s1q00, rel2Head = hh_s1q02, sex = hh_s1q03,
         ageYrs_roster = hh_s1q04_a, ageMonths_roster = hh_s1q04_b) %>% 
  mutate(year = 2012) 


roster2014 = roster2014 %>% 
  mutate(household_id = as.character(household_id), individual_id = as.character(individual_id), 
         household_id2 = as.character(household_id2), individual_id2 = as.character(individual_id2)) %>% 
  select(household_id, household_id2, individual_id, individual_id2,
         ea_id = ea_id, ea_id2 = ea_id2, hhID2_roster = saq08,
         hhMemberID_roster = hh_s1q00, rel2Head = hh_s1q02, sex = hh_s1q03,
         ageYrs_roster = hh_s1q04_a, ageMonths_roster = hh_s1q04_b, 
         sexCorrect = hh_s1q04d, correctedSex = hh_s1q04e, ageCorrect = hh_s1q04f,
         birthDay_roster = hh_s1q04g_1,
         birthMonth_roster = hh_s1q04g_2, birthYear_roster = hh_s1q04g_3,
         correctedAge = hh_s1q04h) %>% 
  mutate(year = 2014) 



# Select out just the households in the panel -----------------------------
# panel  = data  %>% select(household_id, household_id2, ea_id, ea_id2, year) %>% 
#   mutate(ptrack = TRUE)
# 
# 
# cover2014 = full_join(panel, cover2014, by = c("household_id" = "household_id", 
#                                                "household_id2" = "household_id2",
#                                                "ea_id" = "ea_id",
#                                                "ea_id2" = "ea_id2", 
#                                                "year" = "year")) %>% 
#   filter(year == 2014, ptrack == TRUE)
# 
# cover2012 = full_join(panel, cover2012, by = c("household_id" = "household_id", 
#                                                "ea_id" = "ea_id",
#                                                "year" = "year")) %>% 
#   filter(year == 2012, ptrack == TRUE)
# 
# 
# 
# roster2014 = full_join(panel, roster2014, by = c("household_id" = "household_id", 
#                                                  "household_id2" = "household_id2",
#                                                  "ea_id" = "ea_id",
#                                                  "ea_id2" = "ea_id2", 
#                                                  "year" = "year")) %>% 
#   filter(year == 2014, ptrack == TRUE, ageYrs_roster < 10 | is.na(ageYrs_roster))
# 
# 
# roster2012 = full_join(panel, roster2012, by = c("household_id" = "household_id", 
#                                                  "ea_id" = "ea_id",
#                                                  "year" = "year")) %>% 
#   filter(year == 2012, ptrack == TRUE, ageYrs_roster < 10 | is.na(ageYrs_roster))
# 
# 
# 
# 
# health2014 = full_join(panel, health2014, by = c("household_id" = "household_id", 
#                                                  "household_id2" = "household_id2",
#                                                  "ea_id" = "ea_id",
#                                                  "ea_id2" = "ea_id2", 
#                                                  "year" = "year")) %>% 
#   filter(year == 2014, ptrack == TRUE)
# 
# 
# health2012 = full_join(panel, health2012, by = c("household_id" = "household_id", 
#                                                  "ea_id" = "ea_id",
#                                                  "year" = "year")) %>% 
#   filter(year == 2012, ptrack == TRUE)



# merge individual datasets together --------------------------------------

all2012 = left_join(health2012, cover2012, by = c("household_id" = "household_id","ea_id" = "ea_id",  "year"))

all2012 = left_join(all2012, roster2012, by = c("household_id" = "household_id", "ea_id" = "ea_id", "year",
                                                "individual_id" = "individual_id"))


all2014 = left_join(health2014, cover2014, by = c("household_id" = "household_id", "household_id2" = "household_id2",
                                                  "ea_id" = "ea_id", "ea_id2" = "ea_id2", "year"))

all2014 = left_join(all2014, roster2014, by = c("household_id" = "household_id", "household_id2" = "household_id2", 
                                                "ea_id" = "ea_id", "ea_id2" = "ea_id2", "year", 
                                                "individual_id" = "individual_id", "individual_id2"= "individual_id2"))

# Calculate a rough age, based on the health data. ------------------------
# Ethiopian Amharic calendars are annoyingly different than Gregorian.  As a first pass, assuming dates are roughly equivalent to 
# Gregorian ones.

# Also correct the heights: ~ 100ish a year are physically unreasonable.

all2014 = all2014 %>% 
  mutate(ageMonthsEst = interviewMonthHealth - birthMonth_health + 12*(interviewYearHealth - birthYear_health),
         ageMonth_roster_total = ifelse(is.na(ageYrs_roster), ageMonths_roster, # Convert age on the roster to a total number of months.
                                        ifelse(is.na(ageMonths_roster), ageYrs_roster *12,
                                               ageMonths_roster + 12* ageYrs_roster)),
         diffAge = ageMonthsEst - ageMonth_roster_total, # Difference in age b/w the estimate and what's reported on the roster.
         agesAgree = abs(diffAge) <= 2,
         diffCorr = abs(correctedAge * 12 - ageMonthsEst) <= 2,
         correctedHeight = ifelse(height < 1.4, height * 100,
                                  ifelse(height > 140, height / 10, height)), # Fix heights that make no sense. 
         correctedHeight = ifelse(correctedHeight < 140 & correctedHeight > 40, correctedHeight, NA) # Double check heights are within a reasonable range.
  ) %>% 
  select(household_id, household_id2, year, individual_id, individual_id2,
         sex, sexCorrect, correctedSex, birthMonth_roster, birthMonth_health,
         ageYrs_roster, ageMonths_roster, ageMonth_roster_total, ageMonthsEst, 
         ageCorrect, correctedAge, diffAge, agesAgree, diffCorr, weight, height, correctedHeight)


all2012 = all2012 %>% 
  mutate(ageMonthsEst = interviewMonthHealth - birthMonth_health + 12*(interviewYearHealth - birthYear_health),
         ageMonth_roster_total = ifelse(is.na(ageYrs_roster), ageMonths_roster,
                                        ifelse(is.na(ageMonths_roster), ageYrs_roster *12,
                                               ageMonths_roster + 12* ageYrs_roster)),
         diffAge = ageMonthsEst - ageMonth_roster_total,
         agesAgree = abs(diffAge) <= 2) %>% 
  select(household_id, year, individual_id, 
         sex, weight, height,
         ageYrs_roster, ageMonths_roster, ageMonth_roster_total, ageMonthsEst, 
         diffAge, agesAgree, birthMonth_health)


# Merge two years together ------------------------------------------------

children = full_join(all2012, all2014, 
                     by = c("household_id", "individual_id"))

children = children %>% 
  mutate(ageGap = ageMonth_roster_total.x - ageMonth_roster_total.y,
         estGap = ageMonthsEst.x - ageMonthsEst.y,
         largeGap = abs(ageGap) > 30 | abs(ageGap) < 18,
         largeEst = abs(estGap) > 30 | abs(estGap) < 18) %>% 
  # filter(largeGap == TRUE) %>% 
  select(household_id, household_id2, individual_id, individual_id2, 
         ageYrs_roster.x, ageMonths_roster.x, ageMonth_roster_total.x,
         ageMonthsEst.x, ageYrs_roster.y, ageMonths_roster.y, ageMonth_roster_total.y, ageMonthsEst.y,
         ageGap, estGap, largeEst, largeGap, agesAgree.x, agesAgree.y, sexCorrect) %>% 
  mutate(agesAgree = ifelse(agesAgree.x == FALSE | agesAgree.y == FALSE, FALSE, TRUE))

ggplot(children, aes(x = ageMonth_roster_total.x, y = ageMonth_roster_total.y,
                     colour = factor(agesAgree))) + 
  geom_point(size = 3,  alpha  = 0.2)+theme_box_ygrid()+ xlab('2012') + ylab('2014') + 
  theme(axis.title.y = element_text()) +
  coord_cartesian(ylim = c(0, 100))


# Okay.  These kids aren’t alright.  Let’s filter out just the kid --------
maybeOkay = children %>% 
  filter(largeGap == FALSE, sexCorrect != 2) %>% 
  select(household_id, household_id2, individual_id, individual_id2)

x = child %>% 
  filter(individual_id %in% maybeOkay$individual_id)




# Examining the child dataset ---------------------------------------------
stuntingDB = child  %>% 
  select(household_id, household_id2, 
         individual_id, individual_id2, year, ageMonths,
         stunting, stunted) %>% 
  spread(year, ageMonths)


#   group_by(household_id, household_id2, individual_id, individual_id2, year) %>% 
#   mutate(age_lagged = lag(ageMonths, order_by = year))
#   

#   mutate(TLU_lagged = lag(TLUtotal, order_by = year))



# Is height reasonable? ---------------------------------------------------
# Source for comparison: http://www.who.int/childgrowth/en/
# Using girls for lower bound: http://www.who.int/childgrowth/standards/sft_lhfa_girls_p_0_2.pdf?ua=1
# At birth, 3rd percentile  == 45.6 cm
# And boys for upper bound: http://www.who.int/childgrowth/standards/sft_lhfa_boys_p_2_5.pdf?ua=1, http://www.cdc.gov/growthcharts/html_charts/statage.htm
# 118.7 cm @ 5 y, 138.7 @ 8 y.
# Therefore: correcting if it's < 1.4 (presumably 1.4 m), or > 140 (presumably > 140 mm)
heightData = all2014 %>% 
  mutate(heightLab = ifelse(height < 45 | height > 140, '#2166ac', '#abd9e9'))

ggplot(heightData, aes(x = height, fill = heightLab)) + 
  geom_histogram(binwidth = 5) +
  scale_fill_identity() +
  theme_jointplot()



ggplot(heightData, aes(x = ageMonthsEst, y = height, colour = heightLab)) + 
  geom_point(size = 4, alpha = 0.2) +
  scale_color_identity() +
  scale_x_continuous(limits = c(0, 90)) +
  theme_jointplot() +
  coord_cartesian(ylim = c(0, 140))


ggplot(heightData, aes(x = ageMonthsEst, y = correctedHeight, colour = heightLab)) + 
  geom_point(size = 4, alpha = 0.2) +
  scale_color_identity() +
  scale_x_continuous(limits = c(0, 90)) +
  theme_jointplot() +
  coord_cartesian(ylim = c(0, 140))

ggplot(child, aes(x = ageM, y = cheight, colour = factor(stunted))) + 
  geom_point(size = 4, alpha = 0.2) +
  scale_x_continuous(limits = c(0, 90)) +
  theme_jointplot() +
  coord_cartesian(ylim = c(0, 140))


# Do the ages agree? ------------------------------------------------------

ggplot(x, aes(x = ageMonth_roster_total, y = ageMonthsEst)) + geom_point(size = 3, alpha = 0.3) + theme_jointplot() +
  coord_cartesian(ylim = c(0, 100))


# Purpose: Crack open LSMS data, offset GPS, and plot a bit and look at spat. corr.
# Author: Tim Essam (GeoCenter / OakStream Systems, LLC)
# Required packages: lots of spatial package
# Date: 1/9/2015

# Clear the workspace
remove(list=ls())

# load libraries for use in tinkering with data
libs <- c ("geoR", "akima", "leaflet", "dplyr", "lattice",
	"sp", "maptools", "raster", "rgdal", "maps", "mapdata",
	"RgoogleMaps", "mapproj", "RColorBrewer", "ape")

# Load required libraries
lapply(libs, require, character.only=T)

# Set working directory to Ethiopia project
wdw <- c("U:/Ethiopia/Export")
setwd(wdw)

# Read in data; subset GPS info and jitter for no overlap
d <- read.csv("shocks.exploratory.csv", header=TRUE)

#Drop records with missing lat/lon values
d <- na.omit(d)

gps <- subset(d, select = c(lon, lat))
jitgps <- jitter2d(gps, max=0.01)

# Subset data to be recobined with GIS info
data <- subset(d, select = c(ag, conflict, disaster, financial, health,
	other, priceup, pricedown, totShock, hhid))
geo <- cbind(jitgps, data)

# Export jittered data to GIS/export folder
write.csv(geo, "jittered.LSMS.csv")

# Convert data frame to geodata
dgeo <- as.geodata(geo, coords.col = 1:2, data.col = 3:12)

# Plot data
plot(dgeo)

# create a basic plot of totShocks (using sp package)
coordinates(geo) <- c("lon", "lat")

# For downloading a static webmap from server
latb <- c(min(geo$lat), max(geo$lat))
lonb <- c(min(geo$lon), max(geo$lat))
center <- c(mean(latb), mean(lonb))
zoom <- min(MaxZoom(range(geo$lat), range(geo$lon)))
GetMap(center=center, zoom=zoom, maptype= "terrain", destfile = "MyTile1.png")

# Plot total, priceup, ag, health, and disaster
map("worldHires", "Ethiopia") 
points(geo)

spplot(geo, "totShock") 
spplot(geo, "ag")
spplot(geo, "health")
spplot(geo, "disaster")
spplot(geo, "priceup")

# Convert to grid
rast <- raster(ncol = 50, nrow = 50)
extent(rast) <- extent(geo)
r <- rasterize(geo, rast, geo$totShock, fun = mean)
plot(r)


YlOrRd = colorNumeric("Reds", domain = geo$totShock)
m = leaflet(geo) %>% addTiles()
m = m %>% addCircleMarkers(~geo$lon, ~geo$lat, fill = FALSE, radius = ~geo$totShock*5, color = ~YlOrRd (geo$totShock)) 

# Interpolate the surface (http://www.biostat.umn.edu/~brad/8472/slidegeoR.pdf)
int.scp <- interp.new(geo$lon, geo$lat, geo$priceup)
image(int.scp, xlim = range(geo$lon), ylim = range(geo$lat))
contour(int.scp, add = T)

# Variogram fitting
shocks <- as.data.frame(cbind(geo$lon, geo$lat, geo$priceup))
shocks.geo <- as.geodata(shocks, coords.col=1:2, data.col=3) # Create geo-data
shocks.var <- variog(shocks.geo, estimator.type="classical")
plot(shocks.var)

# Generate a distance matrix(http://www.ats.ucla.edu/stat/r/faq/morans_i.htm)
shock.dist <- as.matrix(dist(cbind(geo$long, geo$lat)))

# Invert the matrix, replace diagonals with zero
shock.dist.inv <- 1/shock.dist
diag(shock.dist.inv) <- 0
shock.dist.inv[1:5, 1:5]

# Check for spatial autocorrelation
Moran.I(geo$ag, shock.dist.inv)
Moran.I(geo$disaster, shock.dist.inv)
Moran.I(geo$health, shock.dist.inv)
Moran.I(geo$priceup, shock.dist.inv)
Moran.I(geo$totShock, shock.dist.inv)
library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(RColorBrewer)
library(animation)
library(gridExtra)
library(grid)
library(stringr)
library(ggalt)
library(choroplethrAdmin1)
library(choroplethr)

# Custom color libraries
source("~/GitHub/Ethiopia/R/colorPalettes.R")

# mutate with if/else dictionary function
source("~/GitHub/Ethiopia/R/code2Cat.r")

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
  paste0(sprintf("%.f", round(x*100, ndigits)), "%")
}


# themes ------------------------------------------------------------------



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

theme_box_ygrid<- function() {
  theme_bw() +
    theme(
      rect = element_blank(),
      plot.background = element_blank(),
      panel.background = element_rect(fill = 'white'),
      axis.text = element_text(size = 10, color = '#353839'),
      title =  element_text(size = 14, face = "bold", hjust = 0, color = 'black'),
      axis.title.x =  element_text(size = 12, face = "bold", color = 'black', hjust = 0.5, vjust = -0.25),
      axis.title.y = element_blank(), 
      # axis.line = element_blank(),
      # axis.ticks = element_blank()
      strip.text = element_text(size=14, face = 'bold', hjust = 0.05, vjust = -2.5),
      legend.position = 'none',
      strip.background = element_blank(),
      axis.ticks.y = element_blank(),
      panel.margin = unit(3, 'lines'),
      panel.grid.major.y = element_line(size = 0.2, color = '#bababa'),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank())
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


theme_blankLH <- function() {
  theme(
    title = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.ticks.length = unit(0, units = 'points'),
  axis.ticks.margin = unit(0, units =  'points'),
  panel.border = element_blank(),
  plot.margin = rep(unit(0, units = 'points'),4),
  panel.grid = element_blank(),
  panel.background = element_blank(), 
  plot.background = element_blank(), 
  legend.position="none"
  )
}

theme_xOnly<- function() {
  theme(title = element_text(size = 32, color = 'black'),
        axis.line = element_line(color = 'black', size = 1),
        axis.ticks.x = element_line(color = 'black', size = 0.5),
        axis.text.x = element_text(size = 16, color = '#353839'),
        axis.title.x = element_text(size = 22, color = 'black'),
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
        panel.border = element_blank(),
        plot.margin = rep(unit(0, units = 'points'),4),
        panel.grid = element_blank(),
        panel.background = element_blank(), 
        strip.text = element_text(size=13, face = 'bold'),
        strip.background = element_blank()
  )
}


theme_bump<- function() {
  theme(title = element_text(size = 32, color = 'black'),
        axis.line = element_blank(),
        axis.ticks.length = unit(7, 'points'),
        axis.ticks.y = element_line(color = 'black', size = 0.5),
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.title.y = element_text(size = 22, color = 'black'),
        axis.text.x = element_text(size = 16, color = 'black'),
        axis.title.x = element_blank(), 
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
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


theme_classicLH<- function() {
  theme(title = element_text(size = 32, color = 'black'),
        axis.line = element_blank(),
        axis.ticks.x = element_line(color = 'black', size = 1),
        axis.text.x = element_text(size = 16, color = 'black'),
        axis.title.x = element_text(size = 22, color = 'black'),
        axis.ticks.y = element_line(color = 'black', size = 1),
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.title.y = element_text(size = 22, color = 'black'),
        legend.position="none",
        panel.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())
}

n = 21


# male / female -----------------------------------------------------------
colorMale = '#27aae1'
colorFemale = '#E37686'

brewerPink = brewer.pal(9, 'RdPu')

femaleGradient = c(brewerPink[1], '#FCCDC8', 
                   '#E98893', '#AF3A4B', '#631622')
                   # high =  '#FEF1DD')

# FtF Colors --------------------------------------------------------------

ftfBlue = '#4799B5'
ftfGreen = '#94A545'
ftfOrange = '#D37D28'

# Blacks/Grey --------------------------------------------------------------
softBlack = '#353839'
stdGrey = '#4D525A'
dkGrey = '#353839'
grey90K = '#414042'
grey60K = '#808285'
grey50K = '#939598'
grey30K = '#BCBEC0'
grey15K = '#DCDDDE'
grey10K = '#E6E7E8'

# blue-orange -------------------------------------------------------------


low1 = '#0F26C3'
low2  ='#00A1F6'
mid = '#f1f1f1'
high1 = '#F77920'
high2 = '#C34415'


BlOr = colorRampPalette(c(low1, low2,  mid, high1, high2))(n)



# blue-orange-brown -------------------------------------------------------

low1 = '#0B0B8C'
# low1 = '#0B0B70'
low2  ='#00A1F6'
mid = '#f1f1f1'
high1 = '#F77920'
# high2 = '#63290A'
high2 = '#592409'
# high2 = '#662304' #good
# high2 = '#7A2A04' #too red



BlOrBr = colorRampPalette(c(low1, low2,  mid, high1, high2))(n)

# blue-brown -------------------------------------------------------
low1 = '#0B0B8C'
low2  ='#00A1F6'
mid = '#bababa'
mid = '#ffefc7'
mid = '#e5d7b5'
mid = '#fff0a9'
# mid = '#ddd3ba'
# mid = '#f1f1f1'
high1 = '#663F31'
# high2 = '#58281B'
# high1 = '#672F17'
high2 = '#49221F'

low = c("#0B0B8C", "#0556C1", "#00A1F6", "#78C9F3")

# high = c("#AB9891", "#663F31", '#775549', "#573028", "#49221F")

# high = c("#957C73","#724F43", "#5A342C", "#49221F")
high = c("#AC9992", "#83655A", "#6C483D" , "#5A342C")
high = c("#AC9992", "#83655A", "#6C483D" , "#49221F") # straight grey-brown
high = c("#B8997D", "#A47B55", "#6C483D" , "#49221F") #yellowish tan-brown

BuBr = colorRampPalette(c(low, mid, high))(n)

# Green-brown -------------------------------------------------------------


# 
# gwr = colorRampPalette(brewer.pal(9,'BrBG'))(n)
# 
GrBr = colorRampPalette(c('#3B5E5E','#436E6C', '#4A807B', '#9EB8B7', '#C7D4D3',
                         '#F1F1F1', '#DFD3CC', '#CCB7A4', '#B8997D',
                         '#A47B55','#61483B'))(n)

p2=c("#88000E",
     "#C60019",
     "#EF5739",
     "#EF654F",
     "#EA7F25",
     "#BE9226",
     "#E3666B",
     "#F7441C",
     "#F2AF23")

low1 = '#37010E'
low2  ='#76031F'
mid = '#B60430'
high1 = '#F60641'


satPlum = colorRampPalette(c(low1, low2,  mid, high1))(n)

low1 = '#63021A'
low2  ='#76031F'
low3 = '#D13150'
mid = '#D1573C'
high1 = '#D37751'
high2 = '#D39671'


haz1 = colorRampPalette(rev(c(low1, low2, high2)))(n)

OrRd = colorRampPalette(brewer.pal(9, "OrRd"))(n)


# plum to blue ------------------------------------------------------------
PlBl = colorRampPalette((c("#942132",  '#BD403C', "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0",
                                "#92C5DE", "#4393C3", "#2166AC", "#053061"
                                 )))(n)

# plum-y ------------------------------------------------------------------
low = '#C32252'
low = '#7A1533'
# low = c('#9B2335', '#BC243C')
low =  c('#631622', '#9B2335')
low =  c('#59141F', '#9B2335')
# med = '#C35B63'
# med = '#9B4747'
med = '#E15D44'
# med = '#DD4124'
high = c('#FFD3BF')
high =  '#FEF1DD'
PlOrYl  = colorRampPalette(rev(c(low, med, high)))(n)


# purple pink yellow ------------------------------------------------------


low = c('#ece6c2', '#edd5b3', '#e5b7a0')
med = c('#b65974') # pink
high = c('#78497f',  '#432970') #purple
  
PuPiYl  = colorRampPalette(rev(c("#432970", "#5D3877", "#78497F" ,"#975179", 
                                 "#B65974", "#CD878A", "#E5B7A0", "#E9C6A9", "#EDD5B3")))(n)



# plot --------------------------------------------------------------------


df = data.frame(x = 1:n, y = 1,
                gwr = BuBr, haz1 = haz1, PuPiYl = PuPiYl,
                divPlum = PlOrYl)

ggplot(df) +
  # geom_rect(aes(xmin = x, xmax = x + 1, ymin = y - .5, ymax = y - 1.5, fill = haz1)) +
  # geom_rect(aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 1, fill = haz)) +
  geom_rect(aes(xmin = x, xmax = x + 1, ymin = y + 1.5, ymax = y + 2.5, fill = gwr)) +
  # geom_rect(aes(xmin = x, xmax = x + 1, ymin = y + 1.5, ymax = y + 2.5, fill = gwr2)) +
  # geom_rect(aes(xmin = x, xmax = x + 1, ymin = y + 3, ymax = y + 4, fill = haz1)) +
  # geom_rect(aes(xmin = x, xmax = x + 1, ymin = y + 4.5, ymax = y + 5.5, fill = PuPiYl)) +
  # geom_rect(aes(xmin = x, xmax = x + 1, ymin = y + 6, ymax = y + 7, fill = divPlum)) +
  scale_fill_identity()+
  theme_classic() + theme(panel.background = element_rect(fill = '#e9e9e9'))

library(colorspace)

# rgb = hex2RGB(gwr2)


# gwr_BuBr = rbind(0.043137255 0.04313725 0.5490196
#                  [2,] 0.031372549 0.16078431 0.6313725
#                  [3,] 0.023529412 0.27843137 0.7137255
#                  [4,] 0.011764706 0.39607843 0.7960784
#                  [5,] 0.003921569 0.51372549 0.8784314
#                  [6,] 0.000000000 0.63137255 0.9647059
#                  [7,] 0.188235294 0.69411765 0.9568627
#                  [8,] 0.376470588 0.75686275 0.9529412
#                  [9,] 0.549019608 0.79607843 0.9058824
#                  [10,] 0.705882353 0.81176471 0.8156863
#                  [11,] 0.866666667 0.82745098 0.7294118
#                  [12,] 0.807843137 0.73333333 0.6313725
#                  [13,] 0.749019608 0.64313725 0.5372549
#                  [14,] 0.701960784 0.57647059 0.4549020
#                  [15,] 0.670588235 0.52549020 0.3921569
#                  [16,] 0.643137255 0.48235294 0.3333333
#                  [17,] 0.552941176 0.40000000 0.2941176
#                  [18,] 0.466666667 0.32156863 0.2549020
#                  [19,] 0.396078431 0.25098039 0.2117647
#                  [20,] 0.337254902 0.19215686 0.1647059
#                  [21,] 0.286274510 0.13333333 0.1215686)

# gwr_BuBr = col2rgb(BuBr)
# 
# plums = col2rgb(hazards)

rm(low, low3, low2, low1, haz1, high, high1, high2, p2, n, mid, med, df)
# Module for pulling out the shock-level data, and merging to hh-level data.
# Ethiopia LS/MS analysis
# August 2015, Laura Hughes, lhughes@usaid.gov


# Colors ------------------------------------------------------------------
colorShock = '#bd0026'


# Load Data ---------------------------------------------------------------

# Read in the merged household level data and load packages / helper functions.
source("~/GitHub/Ethiopia/R/loadETHpanel.r")
# remove child data
rm(child, childRaw)


# Load in raw shock data from 2012 and 2014.
setwd("~/Documents/USAID/Ethiopia/")

shocks2012Raw = read_dta("Datain/wave2012/sect8_hh_w1.dta")
shocks2014Raw = read_dta("Datain/wave2014/sect8_hh_w2.dta")

attr2012 = pullAttributes(shocks2012Raw)
attr2014 = pullAttributes(shocks2014Raw)

shocks2012 = removeAttributes(shocks2012Raw)
shocks2014 = removeAttributes(shocks2014Raw)


# Rename and consolidate shock data ---------------------------------------


# Remove variables don't need; add in ones do.
shocks2012 = shocks2012 %>% 
  mutate(year = 2012,
         isShocked = ifelse(hh_s8q01 == 2, 0,
                            ifelse(hh_s8q01 == 1, 1, NA)),
         incomeChg = ifelse(hh_s8q03_a == 1, 1, 
                            ifelse(hh_s8q03_a == 2, -1, 
                                   ifelse(hh_s8q03_a == 3, 0, NA))),
         assetsChg = ifelse(hh_s8q03_b == 1, 1, 
                            ifelse(hh_s8q03_b == 2, -1, 
                                   ifelse(hh_s8q03_b == 3, 0, NA))),
         foodProdChg = ifelse(hh_s8q03_c == 1, 1, 
                              ifelse(hh_s8q03_c == 2, -1, 
                                     ifelse(hh_s8q03_c == 3, 0, NA))),
         foodStocksChg = ifelse(hh_s8q03_d == 1, 1, 
                                ifelse(hh_s8q03_d == 2, -1, 
                                       ifelse(hh_s8q03_d == 3, 0, NA))),
         foodPurchChg = ifelse(hh_s8q03_e == 1, 1, 
                               ifelse(hh_s8q03_e == 2, -1, 
                                      ifelse(hh_s8q03_e == 3, 0, NA)))
  ) %>% 
  select(household_id, year, regionCode = saq01,
         shockCode = hh_s8q00, shockDescrip = hh_s8q0a, 
         isShocked, shockSev = hh_s8q02,
         incomeChg, assetsChg, foodProdChg, foodStocksChg, foodPurchChg,
         cope1 = hh_s8q04_a, cope2 = hh_s8q04_b, cope3 = hh_s8q04_c,
         freqYr = hh_s8q05, freq5y = hh_s8q06)%>% 
  mutate(shockDescrip = ifelse(shockCode == 101, 'Death of household head',
                               ifelse(shockCode == 102, 'Illness of household member',
                                      ifelse(shockCode == 103, 'Loss of non-farm jobs',
                                             ifelse(shockCode == 104, 'Drought',
                                                    ifelse(shockCode == 105, 'Flood',
                                                           ifelse(shockCode == 106, 'Landslides / avalanches',
                                                                  ifelse(shockCode == 107, 'Heavy rains preventing work',
                                                                         ifelse(shockCode == 108, 'Crop damage',
                                                                                ifelse(shockCode == 109, 'Price fall of food items',
                                                                                       ifelse(shockCode == 110, 'Price rise of food items',
                                                                                              ifelse(shockCode == 111, 'Price increase of farm inputs',
                                                                                                     ifelse(shockCode == 112, 'Loss of livestock',
                                                                                                            ifelse(shockCode == 113, 'Fire',
                                                                                                                   ifelse(shockCode == 114, 'Theft, robbery, and violence',
                                                                                                                          ifelse(shockCode == 115, 'Involuntary loss of house / land',
                                                                                                                                 ifelse(shockCode == 116, 'Displacement from govt. dvpt.',
                                                                                                                                        ifelse(shockCode ==117, 'Local unrest / violence',
                                                                                                                                               ifelse(shockCode == 118, 'other', NA)
                                                                                                                                        ))))))))))))))))),
         
         shockScore = incomeChg + assetsChg + foodProdChg + foodStocksChg + foodPurchChg,
         
         assetShockBin = ifelse(isShocked == 1 & (shockCode == 108 | shockCode == 112 | 
                                                    shockCode == 115 | shockCode == 116), 1, 0),
         priceShockBin = ifelse(isShocked == 1 & (shockCode == 109 | shockCode == 110 | 
                                                    shockCode == 111), 1, 0),
         hazardShockBin = ifelse(isShocked == 1 & (shockCode == 104 | shockCode == 105 | 
                                                     shockCode == 106 | shockCode == 107 | shockCode == 113), 1, 0),
         healthShockBin = ifelse(isShocked == 1 & (shockCode == 101 | shockCode == 102), 1, 0),
         
         
         shockClass = ifelse(isShocked == 1 & (shockCode == 108 | shockCode == 112 | 
                                                 shockCode == 115 | shockCode == 116), 'asset',
                             ifelse(isShocked == 1 & (shockCode == 109 | shockCode == 110 | 
                                                        shockCode == 111), 'price',
                                    ifelse(isShocked == 1 & (shockCode == 104 | shockCode == 105 | 
                                                               shockCode == 106 | shockCode == 107 | shockCode == 113), 'hazard',
                                           ifelse(isShocked == 1 & (shockCode == 101 | shockCode == 102), 'health', NA)))),
         
         
         goodCope = ifelse(cope1 == 1 | cope1 == 2 | cope1 == 3 | cope1 == 4 | cope1 == 5 | cope1 == 6 | cope1 == 7 | cope1 == 8 | cope1 == 10 | cope1 == 12 | cope1 == 16, 1, 0),
         badCope = ifelse(cope1 == 5 | cope1 == 9 | cope1 == 11| cope1 == 13| cope1 == 14| cope1 == 15| cope1 == 17, 1,0),
         othCope = ifelse(cope1 == 18 | cope1 == 19 | cope1 == 20 | cope1 == 25 | cope1 == 60, 1, 0),
         
         
         cope1Cat = ifelse(cope1 == 1, 'used savings',
                           ifelse(cope1 == 2, 'help from family/friends',
                                  ifelse(cope1 == 3, 'help from govt',
                                         ifelse(cope1 == 4, 'help from NGO/relig org',
                                                ifelse(cope1 == 5, 'changed food consump',
                                                       ifelse(cope1 == 6, 'second job', 
                                                              ifelse(cope1 == 7, 'new job',
                                                                     ifelse(cope1 == 8, 'migrated',
                                                                            ifelse(cope1 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope1 == 10, 'got credit',
                                                                                          ifelse(cope1 == 11, 'sold ag assets',
                                                                                                 ifelse(cope1 == 12, 'sold durable assets',
                                                                                                        ifelse(cope1 == 13, 'sold land',
                                                                                                               ifelse(cope1 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope1 == 15, 'sold livestock',
                                                                                                                             ifelse(cope1 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope1 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope1 == 18, 'prayed',
                                                                                                                                                  ifelse(cope1 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope1 == 20, 'other', NA)))))))))))))))))))),
         cope2Cat = ifelse(cope2 == 1, 'used savings',
                           ifelse(cope2 == 2, 'help from family/friends',
                                  ifelse(cope2 == 3, 'help from govt',
                                         ifelse(cope2 == 4, 'help from NGO/relig org',
                                                ifelse(cope2 == 5, 'changed food consump',
                                                       ifelse(cope2 == 6, 'second job', 
                                                              ifelse(cope2 == 7, 'new job',
                                                                     ifelse(cope2 == 8, 'migrated',
                                                                            ifelse(cope2 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope2 == 10, 'got credit',
                                                                                          ifelse(cope2 == 11, 'sold ag assets',
                                                                                                 ifelse(cope2 == 12, 'sold durable assets',
                                                                                                        ifelse(cope2 == 13, 'sold land',
                                                                                                               ifelse(cope2 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope2 == 15, 'sold livestock',
                                                                                                                             ifelse(cope2 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope2 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope2 == 18, 'prayed',
                                                                                                                                                  ifelse(cope2 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope2 == 20, 'other', NA)))))))))))))))))))),
         cope3Cat = ifelse(cope3 == 1, 'used savings',
                           ifelse(cope3 == 2, 'help from family/friends',
                                  ifelse(cope3 == 3, 'help from govt',
                                         ifelse(cope3 == 4, 'help from NGO/relig org',
                                                ifelse(cope3 == 5, 'changed food consump',
                                                       ifelse(cope3 == 6, 'second job', 
                                                              ifelse(cope3 == 7, 'new job',
                                                                     ifelse(cope3 == 8, 'migrated',
                                                                            ifelse(cope3 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope3 == 10, 'got credit',
                                                                                          ifelse(cope3 == 11, 'sold ag assets',
                                                                                                 ifelse(cope3 == 12, 'sold durable assets',
                                                                                                        ifelse(cope3 == 13, 'sold land',
                                                                                                               ifelse(cope3 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope3 == 15, 'sold livestock',
                                                                                                                             ifelse(cope3 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope3 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope3 == 18, 'prayed',
                                                                                                                                                  ifelse(cope3 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope3 == 20, 'other', NA))))))))))))))))))))
         
  )


# Remove variables don't need; add in ones do.
shocks2014 = shocks2014 %>% 
  mutate(year = 2014,
         isShocked = ifelse(hh_s8q01 == 2, 0,
                            ifelse(hh_s8q01 == 1, 1, NA)),
         incomeChg = ifelse(hh_s8q03_a == 1, 1, 
                            ifelse(hh_s8q03_a == 2, -1, 
                                   ifelse(hh_s8q03_a == 3, 0, NA))),
         assetsChg = ifelse(hh_s8q03_b == 1, 1, 
                            ifelse(hh_s8q03_b == 2, -1, 
                                   ifelse(hh_s8q03_b == 3, 0, NA))),
         foodProdChg = ifelse(hh_s8q03_c == 1, 1, 
                              ifelse(hh_s8q03_c == 2, -1, 
                                     ifelse(hh_s8q03_c == 3, 0, NA))),
         foodStocksChg = ifelse(hh_s8q03_d == 1, 1, 
                                ifelse(hh_s8q03_d == 2, -1, 
                                       ifelse(hh_s8q03_d == 3, 0, NA))),
         foodPurchChg = ifelse(hh_s8q03_e == 1, 1, 
                               ifelse(hh_s8q03_e == 2, -1, 
                                      ifelse(hh_s8q03_e == 3, 0, NA)))
  ) %>% 
  select(household_id, household_id2, year, regionCode = saq01,
         shockCode = hh_s8q00, shockDescrip = hh_s8q0a, 
         isShocked, shockSev = hh_s8q02,
         incomeChg, assetsChg, foodProdChg, foodStocksChg, foodPurchChg,
         cope1 = hh_s8q04_a, cope2 = hh_s8q04_b, cope3 = hh_s8q04_c,
         freqYr = hh_s8q05, freq5y = hh_s8q06) %>% 
  
  # remove death of other hh member, to be consistent w/ 2012 data.
  filter(shockCode != 201) %>% 
  mutate(shockDescrip = ifelse(shockCode == 101, 'Death of household head',
                               ifelse(shockCode == 102, 'Illness of household member',
                                      ifelse(shockCode == 103, 'Loss of non-farm jobs',
                                             ifelse(shockCode == 104, 'Drought',
                                                    ifelse(shockCode == 105, 'Flood',
                                                           ifelse(shockCode == 106, 'Landslides / avalanches',
                                                                  ifelse(shockCode == 107, 'Heavy rains preventing work',
                                                                         ifelse(shockCode == 108, 'Crop damage',
                                                                                ifelse(shockCode == 109, 'Price fall of food items',
                                                                                       ifelse(shockCode == 110, 'Price rise of food items',
                                                                                              ifelse(shockCode == 111, 'Price increase of farm inputs',
                                                                                                     ifelse(shockCode == 112, 'Loss of livestock',
                                                                                                            ifelse(shockCode == 113, 'Fire',
                                                                                                                   ifelse(shockCode == 114, 'Theft, robbery, and violence',
                                                                                                                          ifelse(shockCode == 115, 'Involuntary loss of house / land',
                                                                                                                                 ifelse(shockCode == 116, 'Displacement from govt. dvpt.',
                                                                                                                                        ifelse(shockCode ==117, 'Local unrest / violence',
                                                                                                                                               ifelse(shockCode == 118, 'other', NA)
                                                                                                                                        ))))))))))))))))),
         
         shockScore = incomeChg + assetsChg + foodProdChg + foodStocksChg + foodPurchChg,
         
         assetShockBin = ifelse(isShocked == 1 & (shockCode == 108 | shockCode == 112 | 
                                                    shockCode == 115 | shockCode == 116), 1, 0),
         priceShockBin = ifelse(isShocked == 1 & (shockCode == 109 | shockCode == 110 | 
                                                    shockCode == 111), 1, 0),
         hazardShockBin = ifelse(isShocked == 1 & (shockCode == 104 | shockCode == 105 | 
                                                     shockCode == 106 | shockCode == 107 | shockCode == 113), 1, 0),
         healthShockBin = ifelse(isShocked == 1 & (shockCode == 101 | shockCode == 102), 1, 0),
         
         
         shockClass = ifelse(isShocked == 1 & (shockCode == 108 | shockCode == 112 | 
                                                 shockCode == 115 | shockCode == 116), 'asset',
                             ifelse(isShocked == 1 & (shockCode == 109 | shockCode == 110 | 
                                                        shockCode == 111), 'price',
                                    ifelse(isShocked == 1 & (shockCode == 104 | shockCode == 105 | 
                                                               shockCode == 106 | shockCode == 107 | shockCode == 113), 'hazard',
                                           ifelse(isShocked == 1 & (shockCode == 101 | shockCode == 102), 'health', NA)))),
         
         goodCope = ifelse(cope1 == 1 | cope1 == 2 | cope1 == 3 | cope1 == 4 | cope1 == 5 | cope1 == 6 | cope1 == 7 | cope1 == 8 | cope1 == 10 | cope1 == 12 | cope1 == 16, 1, 0),
         badCope = ifelse(cope1 == 5 | cope1 == 9 | cope1 == 11| cope1 == 13| cope1 == 14| cope1 == 15| cope1 == 17, 1,0),
         othCope = ifelse(cope1 == 18 | cope1 == 19 | cope1 == 20 | cope1 == 25 | cope1 == 60, 1, 0),
         
         
         cope1Cat = ifelse(cope1 == 1, 'used savings',
                           ifelse(cope1 == 2, 'help from family/friends',
                                  ifelse(cope1 == 3, 'help from govt',
                                         ifelse(cope1 == 4, 'help from NGO/relig org',
                                                ifelse(cope1 == 5, 'changed food consump',
                                                       ifelse(cope1 == 6, 'second job', 
                                                              ifelse(cope1 == 7, 'new job',
                                                                     ifelse(cope1 == 8, 'migrated',
                                                                            ifelse(cope1 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope1 == 10, 'got credit',
                                                                                          ifelse(cope1 == 11, 'sold ag assets',
                                                                                                 ifelse(cope1 == 12, 'sold durable assets',
                                                                                                        ifelse(cope1 == 13, 'sold land',
                                                                                                               ifelse(cope1 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope1 == 15, 'sold livestock',
                                                                                                                             ifelse(cope1 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope1 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope1 == 18, 'prayed',
                                                                                                                                                  ifelse(cope1 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope1 == 20, 'other', NA)))))))))))))))))))),
         cope2Cat = ifelse(cope2 == 1, 'used savings',
                           ifelse(cope2 == 2, 'help from family/friends',
                                  ifelse(cope2 == 3, 'help from govt',
                                         ifelse(cope2 == 4, 'help from NGO/relig org',
                                                ifelse(cope2 == 5, 'changed food consump',
                                                       ifelse(cope2 == 6, 'second job', 
                                                              ifelse(cope2 == 7, 'new job',
                                                                     ifelse(cope2 == 8, 'migrated',
                                                                            ifelse(cope2 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope2 == 10, 'got credit',
                                                                                          ifelse(cope2 == 11, 'sold ag assets',
                                                                                                 ifelse(cope2 == 12, 'sold durable assets',
                                                                                                        ifelse(cope2 == 13, 'sold land',
                                                                                                               ifelse(cope2 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope2 == 15, 'sold livestock',
                                                                                                                             ifelse(cope2 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope2 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope2 == 18, 'prayed',
                                                                                                                                                  ifelse(cope2 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope2 == 20, 'other', NA)))))))))))))))))))),
         cope3Cat = ifelse(cope3 == 1, 'used savings',
                           ifelse(cope3 == 2, 'help from family/friends',
                                  ifelse(cope3 == 3, 'help from govt',
                                         ifelse(cope3 == 4, 'help from NGO/relig org',
                                                ifelse(cope3 == 5, 'changed food consump',
                                                       ifelse(cope3 == 6, 'second job', 
                                                              ifelse(cope3 == 7, 'new job',
                                                                     ifelse(cope3 == 8, 'migrated',
                                                                            ifelse(cope3 == 9, 'decr health/edu spending',
                                                                                   ifelse(cope3 == 10, 'got credit',
                                                                                          ifelse(cope3 == 11, 'sold ag assets',
                                                                                                 ifelse(cope3 == 12, 'sold durable assets',
                                                                                                        ifelse(cope3 == 13, 'sold land',
                                                                                                               ifelse(cope3 == 14, 'sold crop stock',
                                                                                                                      ifelse(cope3 == 15, 'sold livestock',
                                                                                                                             ifelse(cope3 == 16, 'incr fishing',
                                                                                                                                    ifelse(cope3 == 17, 'sent kids away',
                                                                                                                                           ifelse(cope3 == 18, 'prayed',
                                                                                                                                                  ifelse(cope3 == 19, 'did nothing',
                                                                                                                                                         ifelse(cope3 == 20, 'other', NA))))))))))))))))))))
         
  )



# Merge w/ hh data --------------------------------------------------------
hh = data %>% 
  select(household_id, household_id2, year, ptrack,
         religHoh, agehead, ageLinear, saq01, region, 
         ftfzone, ftfzone_5km,
         literateHoh, literateSpouse, educAdultM, 
         educAdultF, educHoh, educSpouse, eduMcat, eduFcat,
         femhead,
         landQtile, landHectares, landQtile_lag, TLUtotal,
         wealthIndex, wlthSmooth, wealthPanel,
         contains('Shk'), contains('shock'), 
         -merge_shocks) %>% 
  mutate(religion = ifelse(religHoh == 1, 
                           "Orthodox",
                           ifelse(religHoh == 3,
                                  "Protestant",
                                  ifelse(religHoh == 4,
                                         "Muslim",
                                         ifelse(religHoh == 7 | religHoh == 2 | religHoh == 6 | religHoh == 5,
                                                "other", NA)))))


shocksPanel12 = inner_join(shocks2012, hh, by = c("household_id", "year"))

shocksPanel14 = inner_join(shocks2014, hh, by = c("household_id", "household_id2", "year"))

shocksPanel = rbind(shocksPanel12, shocksPanel14)

sh = shocksPanel


sh14 = sh %>% 
  filter(year == 2014)


# # Exploring differences ------------------------------------------------------------------
# View(sh  %>% filter(isShocked == 1, priceShockBin == 1, !is.na(religion), !is.na(cope1Cat)) %>%  
#        group_by(cope1Cat, religion)  %>% 
#        summarise(num = n()) %>% ungroup()  %>% 
#        group_by(religion) %>% 
#        mutate(pct=percent(num/sum(num)))  %>% 
#        ungroup() %>% 
#        arrange(cope1Cat))
# 
# View(sh  %>% filter(isShocked == 1, priceShockBin == 1, !is.na(cope1Cat)) %>%  
#        group_by(cope1Cat)  %>% 
#        summarise(num = n()) %>% 
#        mutate(pct=percent(num/sum(num))))


# severity ----------------------------------------------------------------
shSev = sh14 %>% 
  filter(!is.na(shockClass), shockClass != 'asset')

# Eliminating multiple shocks of the same type; allowed to have one type of each shock, over multiple years.
shSev = shSev %>% group_by(household_id, year, shockClass) %>% 
  filter(isShocked == 1) %>% 
  mutate(mostSev = min(shockSev)) %>% 
  summarise(shockSev = mean(mostSev), 
            wlthSmooth = mean(wlthSmooth),
            agehead = mean(agehead),
            ageLinear = mean(ageLinear),
            ftfzone = mean(ftfzone), 
            religion = min(religion),
            TLU = mean(TLUtotal),
            landQtile = mean(landQtile),
            landQtile_lag = mean(landQtile_lag),
            landHectares = mean(landHectares),
            educAdultM = mean(educAdultM),
            educAdultF = mean(educAdultF),
            literateHoh = mean(literateHoh),
            literateSpouse = mean(literateSpouse),
            priceShk = mean(priceShk)
            )


shSev$shockClass = factor(shSev$shockClass, 
                          c('price', 'hazard', 'health'))

# All shocks
# <<ETH_anyShock_shockSeverity_draft.pdf>>
ggplot(shSev, aes(x = shockSev)) +
  stat_bin(binwidth = 0.5, fill = colorShock) + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), 
           vjust = 1.75, hjust = 1.5, color = 'white', size = 4) +
  facet_wrap(~shockClass) +
  theme_xOnly() +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0.75, 3.75),
                     breaks = 1:3,
                     labels = c("most", "", "least")) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 950, by = 100)) +
  theme(panel.grid = element_line(),
        strip.text = element_text(size=20, face = 'bold'),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_line(colour = 'grey', size = 0.1),
        panel.margin = unit(4, "lines"),
        axis.text.x = element_text(hjust = 0)) +
  xlab('severity of shock')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_anyShock_shockSeverity_draft.pdf",
       width = 9, height = 4,
       bg = 'white',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



# Shock severity over different factors.

ggplot(shSev, aes(x = wlthSmooth, y = shockSev)) +
  geom_smooth(size  = 1.5) +
  facet_wrap(~shockClass) +
  theme_jointplot() +
  scale_y_reverse() +
  theme(strip.text = element_text(size=20, face = 'bold'),
        panel.margin = unit(2, "lines")) +
  ylab('average shock severity') +
  xlab('wealth decile')

ggplot(shSev, aes(x = wlthSmooth, y = shockSev)) +
  geom_smooth(size  = 1.5) +
  facet_wrap(~shockClass) +
  theme_jointplot() +
  scale_y_reverse() +
  theme(strip.text = element_text(size=20, face = 'bold'),
        panel.margin = unit(2, "lines")) +
  ylab('average shock severity') +
  xlab('wealth decile')

ggplot(shSev, aes(x = agehead, y = shockSev)) +
  geom_smooth(size  = 1.5, method = 'loess', span = 1) +
  facet_wrap(~shockClass) +
  theme_jointplot() +
  scale_y_reverse() +
  theme(strip.text = element_text(size=20, face = 'bold'),
        panel.margin = unit(2, "lines")) +
  ylab('average shock severity') +
  xlab('age of household head')


# Not as interesting-- other dimensions without tons of variation.

# ggplot(shSev, aes(x = TLU, y = shockSev)) +
#   geom_smooth(size  = 1.5, method = 'loess', span = 1) +
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   ylab('average shock severity') +
#   xlab('total tropical livestock units')


# ggplot(shSev, aes(x = landHectares, y = shockSev)) +
#   geom_smooth(size  = 1.5, method = 'loess', span = 1) +
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   ylab('average shock severity') +
#   xlab('land size (hectares)') +
#   coord_cartesian(xlim = c(0,50))
# 
# 
# 
# # worst to be other?
# ggplot(shSev %>% filter(!is.na(religion)), aes(x = religion, y = shockSev)) +
#   stat_summary(fun.y=mean, colour="red", geom = 'point', size = 6)+
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   ylab('average shock severity') +
#   xlab('religion')
# 
# 
# ggplot(shSev, aes(x = landQtile, y = shockSev)) +
#   stat_summary(fun.y=mean, colour="red", geom = 'point', size = 3)+
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   ylab('average shock severity') +
#   xlab('land quartile')
# 
# ggplot(shSev, aes(x = educAdultF, y = shockSev)) +
#   stat_summary(fun.y=mean, colour="red", geom = 'point', size = 3)+
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   xlab('education of adult female') +
#   ylab('average shock severity')
# 
# 
# ggplot(shSev, aes(x = educAdultM, y = shockSev)) +
#   stat_summary(fun.y=mean, colour="red", geom = 'point', size = 3)+
#   facet_wrap(~shockClass) +
#   theme_jointplot() +
#   scale_y_reverse() +
#   theme(strip.text = element_text(size=20, face = 'bold'),
#         panel.margin = unit(2, "lines")) +
#   xlab('education of adult male') +
#   ylab('average shock severity')


# shk frequency by severity, wealth/age ----------------------------------

sevLimits = c(1.1,1.85)
sizeHH = c(0, 250)
shkMax = 0.22

widthSev = 6.*1.32
heightSev = 3.2*1.32

# -- age, price
filterVar = 'price'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(ageLinear) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(ageLinear) %>% 
  summarise(avg = mean(priceShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(ageLinear))


ggplot(df, aes(x = ageLinear, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(4,20), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(labels=c("< 25","25 - 35", "35 - 45", "45 - 55", "> 55")) +
  ggtitle('price shocks \npercent of households reporting shock') +
  xlab('age of household head')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_priceShk_sev_age.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


ggsave("~/GitHub/Ethiopia/R/plots/ETH_scale_sev_age.pdf",
       width = widthSev, height = heightSev*2,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# -- age, hazard
filterVar = 'hazard'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(ageLinear) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(ageLinear) %>% 
  summarise(avg = mean(hazardShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(ageLinear))


ggplot(df, aes(x = ageLinear, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(3,20), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(labels=c("< 25","25 - 35", "35 - 45", "45 - 55", "> 55")) +
  ggtitle('hazard shocks \npercent of households reporting shock') +
  xlab('age of household head')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_hazardShk_sev_age.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# -- age, health
filterVar = 'health'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(ageLinear) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(ageLinear) %>% 
  summarise(avg = mean(healthShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(ageLinear))


ggplot(df, aes(x = ageLinear, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(3, 20), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(labels=c("< 25","25 - 35", "35 - 45", "45 - 55", "> 55")) +
  ggtitle('health shocks \npercent of households reporting shock') +
  xlab('age of household head')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_healthShk_sev_age.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



# -- wealth, health
filterVar = 'health'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(wlthSmooth) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(wlthSmooth) %>% 
  summarise(avg = mean(healthShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(wlthSmooth))


ggplot(df, aes(x = wlthSmooth, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(1, 10), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  ggtitle('health shocks \npercent of households reporting shock') +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_healthShk_sev_wlth.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# -- wealth, price
filterVar = 'price'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(wlthSmooth) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(wlthSmooth) %>% 
  summarise(avg = mean(priceShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(wlthSmooth))


ggplot(df, aes(x = wlthSmooth, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(1, 10), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  ggtitle('price shocks \npercent of households reporting shock') +
  xlab('wealth')


ggsave("~/GitHub/Ethiopia/R/plots/ETH_priceShk_sev_wlth.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# -- wealth, hazard
filterVar = 'hazard'

avgSev =  sh14 %>% 
  filter(shockClass == filterVar) %>% 
  group_by(wlthSmooth) %>% 
  summarise(sev = mean(shockSev, na.rm = T),
            num = n())

avgReport = data %>%
  filter(year == 2014) %>% 
  group_by(wlthSmooth) %>% 
  summarise(avg = mean(hazardShk)) 


df = full_join(avgSev, avgReport)

df = df %>% 
  filter(!is.na(wlthSmooth))


ggplot(df, aes(x = wlthSmooth, y = avg, colour = sev, size = num)) +
  geom_point() +
  theme_box_ygrid() +
  theme(legend.position = 'right',
        axis.ticks.x = element_blank()) +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'YlOrRd')),
                         breaks = sevLimits,
                         limits = sevLimits, name = 'shock severity', labels = c('more severe', 'less severe')) +
  scale_size_continuous(range = c(1, 10), 
                        limits = sizeHH,
                        name = 'number of households') +
  scale_y_continuous(labels = percent, limits = c(0, shkMax)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  ggtitle('hazard shocks \npercent of households reporting shock') +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_hazardShk_sev_wlth.pdf",
       width = widthSev, height = heightSev,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)





# shock effect ------------------------------------------------------------
effect = sh %>% 
  gather(change, changeEffect, 8:12) %>% 
  filter(!is.na(changeEffect))
# 
# effect = effect %>% 
#   gather(shockType, )


effect$change = factor(effect$change, 
                       c('incomeChg','foodProdChg', 'foodStocksChg',  'assetsChg',  'foodPurchChg' ))

effect$shockClass = factor(effect$shockClass,
                           c('price', 'health', 'hazard'))

ggplot(effect, aes(x = change, y = (..count..)/sum(..count..),
                   fill = factor(changeEffect))) +
  geom_bar() +
  facet_wrap(~wlthSmooth) +
  theme_laura()


plotStackedBar = function(data,
                          facetVar = 'wlthSmooth',
                          valueVar = 'changeEffect',
                          colorPos = '#516FB5',
                          colorNeg = '#9E2F49',
                          asPercent = TRUE) {
  
  
  # Filter data to break it into positives and negatives.
  pos = data %>% 
    filter(changeEffect > 0)
  
  neg = data %>% 
    filter(changeEffect < 0)
  
  ggplot(pos, aes(x = change, y = (..count..))) +
    geom_bar(fill = colorPos) +
    geom_bar(aes(x = change, y = -1*(..count..)),
             data = neg, fill = colorNeg) +
    facet_wrap(as.formula(paste0('~', facetVar)))+
    theme_box_ygrid() +
    theme(axis.ticks.x = element_blank()) +
    scale_y_continuous(breaks = seq(-800, 200, by = 100)) +
    scale_x_discrete(labels = c('income', 'food product', 'food stocks','assets','food purchases'))
}

plotStackedBar(effect %>% filter(shockClass != 'asset', !is.na(shockClass)), facetVar = 'shockClass')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_chgWlfare.pdf",
       width = widthCoping, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

x = effect %>% 
  filter(shockClass != "asset") %>% 
  group_by(educAdultF, shockClass, changeEffect) %>% 
  summarise(num = n()) %>% 
  mutate(pct = num/sum(num)) %>% 
  filter(!is.na(shockClass))

ggplot(x, 
       aes(x = educAdultF, y = pct, color = factor(changeEffect),
           group = factor(changeEffect))) +
  geom_path(size = 2) + 
  theme_laura() +
  facet_wrap(~shockClass) +
  coord_cartesian(ylim = c(0, 1))



# coping by wealth --------------------------------------------------------

colorCope = brewer.pal(9, 'PuBuGn')
orderCope = c ('sold livestock' , 'used savings' ,  
               'help from govt', 'help from family/friends')
ymax = 0.35

widthCoping = 19.5
heightCoping = 2.5

# price

priceCope = sh14  %>% filter(isShocked == 1, priceShockBin == 1, !is.na(wlthSmooth), !is.na(cope1Cat)) %>% 
  group_by(cope1Cat, wlthSmooth)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(wlthSmooth) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))


priceCope$cope1Cat = factor(priceCope$cope1Cat, orderCope)

ggplot(priceCope %>% filter(!is.na(cope1Cat)), aes(x = wlthSmooth, 
                                   y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(2, 11)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  theme(panel.margin = unit(1, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 4) +
  ggtitle('coping with price shocks') +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_priceShk_copingWlth.pdf",
       width = widthCoping, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# hazards
haz = sh14  %>% filter(isShocked == 1, hazardShockBin == 1, !is.na(wlthSmooth), !is.na(cope1Cat)) %>% 
  group_by(cope1Cat, wlthSmooth)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(wlthSmooth) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))



haz$cope1Cat = factor(haz$cope1Cat, orderCope)

ggplot(haz %>% filter(!is.na(cope1Cat)), aes(x = wlthSmooth, y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(2, 11)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  theme(panel.margin = unit(1, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 4) +
  ggtitle('coping with hazard shocks') +
  xlab('wealth')



ggsave("~/GitHub/Ethiopia/R/plots/ETH_hazShk_copingWlth.pdf",
       width = widthCoping, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# health
health = sh14  %>% filter(isShocked == 1, healthShockBin == 1, !is.na(wlthSmooth), !is.na(cope1Cat)) %>% 
  group_by(cope1Cat, wlthSmooth)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(wlthSmooth) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))



health$cope1Cat = factor(health$cope1Cat, orderCope)

ggplot(health %>% filter(!is.na(cope1Cat)), aes(x = wlthSmooth, y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(2, 11)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)),
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  theme(panel.margin = unit(1, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 4) +
  ggtitle('coping with health shocks') +
  xlab('wealth')


ggsave("~/GitHub/Ethiopia/R/plots/ETH_healthShk_copingWlth.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# -- land --
health = sh14  %>% filter(isShocked == 1, healthShockBin == 1, !is.na(landQtile_lag), !is.na(cope1Cat)) %>% 
  group_by(cope1Cat, landQtile_lag)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(landQtile_lag) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))



health$cope1Cat = factor(health$cope1Cat, c('used savings', 'sold livestock', 'did nothing', 'help from family/friends'))

ggplot(health %>% filter(!is.na(cope1Cat)), aes(x = landQtile_lag, y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(2, 11)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  scale_x_discrete(breaks = 1:4,
                   # limits = c(-0.5, 10.5),
                   labels=c("little land", "","", "lots of land")) +
  theme(panel.margin = unit(1, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 2) +
  ggtitle('coping with health shocks') +
  xlab('land quartile (lagged)')


ggsave("~/GitHub/Ethiopia/R/plots/ETH_healthShk_copingLand.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# -- educ : numbers VERY thin --
health = sh14  %>% filter(isShocked == 1, healthShockBin == 1, !is.na(eduMcat), !is.na(cope1Cat)) %>% 
  group_by(cope1Cat, eduMcat)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(eduMcat) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))



# health$cope1Cat = factor(health$cope1Cat, c('used savings', 'sold livestock', 'did nothing', 'help from family/friends'))

ggplot(health %>% filter(!is.na(cope1Cat)), aes(x = eduMcat, y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(5, 11)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
#   scale_x_discrete(breaks = 1:4,
#                    # limits = c(-0.5, 10.5),
#                    labels=c("little land", "","", "lots of land")) +
  theme(panel.margin = unit(1, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 4) +
  ggtitle('coping with health shocks') +
  xlab('education adult male')


ggsave("~/GitHub/Ethiopia/R/plots/ETH_healthShk_copingEduM.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# -- ftf  --
health = sh14  %>% filter(isShocked == 1, healthShockBin == 1, !is.na(ftfzone_5km), !is.na(cope1Cat)) %>% 
  group_by(cope1Cat, ftfzone_5km)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(ftfzone_5km) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))



health$cope1Cat = factor(health$cope1Cat, c('used savings', 'prayed', 'sold livestock', 'help from family/friends'))

ggplot(health %>% filter(!is.na(cope1Cat)), aes(x = ftfzone_5km, y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(5, 11)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  #   scale_x_discrete(breaks = 1:4,
  #                    # limits = c(-0.5, 10.5),
  #                    labels=c("little land", "","", "lots of land")) +
  theme(panel.margin = unit(1, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 2) +
  ggtitle('coping with health shocks') +
  xlab('ftf zone')


ggsave("~/GitHub/Ethiopia/R/plots/ETH_healthShk_copingFtF.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# any shock

# coping - sold cows - wealth ---------------------------------------------



allCope = sh14  %>% filter(isShocked == 1, !is.na(wlthSmooth)) %>% 
  mutate(soldCows = ifelse(is.na(cope1Cat), NA, 
                           ifelse(cope1Cat == 'sold livestock', 1, 0)),
         usedSavings = ifelse(is.na(cope1Cat), NA, 
                              ifelse(cope1Cat == 'used savings', 1, 0)),
         didNothing = ifelse(is.na(cope1Cat), NA, 
                           ifelse(cope1Cat == 'did nothing', 1, 0))
         )
 
ggplot(allCope, aes(x = wlthSmooth, y = soldCows))+
  geom_smooth(colour = '#01503f', fill = NA,
              size = 1.25, method = 'loess', span = 1)+ 
  theme_box_ygrid() +
  coord_cartesian(ylim = c(0, ymax)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  ggtitle('Wealthy households tend to hold onto valuable assets \n (cows)') +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_anyShk_cowsWlth.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)




# coping- saved - by male edu ---------------------------------------------


allCope = sh14  %>% filter(isShocked == 1, !is.na(eduMcat), !is.na(cope1Cat)) %>%  
  group_by(cope1Cat, eduMcat)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(eduMcat) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))


allCope$cope1Cat = factor(allCope$cope1Cat, 'used savings')

ggplot(allCope %>% filter(!is.na(cope1Cat)), aes(x = eduMcat, 
                                                 y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(5, 15)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  theme(panel.margin = unit(2, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 4) +
  ggtitle('Educated households tend to rely on savings') 




ggsave("~/GitHub/Ethiopia/R/plots/ETH_anyShk_savedEduM.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



# coping - did nothing - wealth  ------------------------------------------


ggplot(allCope, aes(x = wlthSmooth, y = didNothing))+
  geom_smooth(colour = '#057773', fill = NA,
              size = 1.25, method = 'loess', span = 1)+ 
  theme_box_ygrid() +
  coord_cartesian(ylim = c(0, ymax)) +
  scale_x_discrete(breaks = c(seq(0, 10, by = 3)), 
                   # limits = c(-0.5, 10.5),
                   labels=c("", "very poor","poor", "above average")) +
  ggtitle('Wealthy households tend to do nothing \n (nada)') +
  xlab('wealth')

ggsave("~/GitHub/Ethiopia/R/plots/ETH_anyShk_nadaWlth.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Any shock, by religion, coping ------------------------------------------

allCope = sh14  %>% filter(isShocked == 1, !is.na(religion), !is.na(cope1Cat)) %>%  
  group_by(cope1Cat, religion)  %>% 
  summarise(num = n()) %>% ungroup()  %>% 
  group_by(religion) %>% 
  mutate(pct=num/sum(num)) %>% 
  arrange(desc(pct))


allCope$cope1Cat = factor(allCope$cope1Cat, 'prayed')

ggplot(allCope %>% filter(!is.na(cope1Cat)), aes(x = religion, 
                                                 y = pct, size = num, colour = pct))+
  geom_point()+ 
  theme_box_ygrid() +
  scale_size_continuous(range = c(5, 15)) +
  scale_colour_gradientn(colours = colorCope, limits = c(0, ymax)) +
  scale_y_continuous(limits = c(0, ymax), 
                     labels = percent) +
  theme(panel.margin = unit(2, 'lines'),
        strip.text = element_text(vjust = 1)) +
  facet_wrap(~cope1Cat, ncol = 4) +
  ggtitle('Protestants cope through prayer more than \n other religions percent of households') 

ggsave("~/GitHub/Ethiopia/R/plots/ETH_anyShk_copingRelig.pdf",
       width = widthCoping/3.5, height = heightCoping,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



# infographic: all coping -------------------------------------------------

sh  %>% filter(isShocked == 1, year == 2014,
               !is.na(cope1Cat))  %>% 
  group_by(cope1Cat) %>% 
  summarise(num = n()) %>% 
  mutate(pct = percent(num/sum(num))) %>% 
  arrange(desc(num))

# decision tree -----------------------------------------------------------
# decision  %>% group_by(shockClass) %>% summarise(sum(nObs))
# 
# decision = sh %>% 
#   filter(shockClass != 'asset', !is.na(shockClass)) %>% 
#   group_by(shockClass, cope1Cat) %>% 
#   summarise(nObs = n()) %>% 
#   ungroup() %>% 
#   arrange(desc(nObs))
# 
# table = decision
# 
# nestedJSON = function(table){
#   parent = unique(table[,1])
#  
#   json =   '[
#   {'
#   
#   for (i in 1:length(parent)) {
#     temp = table  %>% 
#       filter(shockClass == parent[i,]) %>% 
#       summarise(sum(nObs))
#     
#     json = paste0(json, " 'name': '", parent[i,], "'")
#   }
#   
# 
#       "name": "experienced price shock",
#       "parent": null,
#       "w": 956,
#       "children": [
#         {
#           "name": "primary shock",
#           "parent": "yes",
#           "w": 418,
#           "children": 
# }
# 

#_________________________________________________________________________________
# Read in and cleanup Ethiopia LS/MS data on the education module (Section 2).
#
# When you run source('R/educationMod.r'), 6 variables are created:
# - education2012: table containing education module 2 from 2012.
# - education2014: table containing education module 2 from 2014.
# - educationQuest2012: survey questions from 2012 ('label' in Stata file)
# - educationQuest2014: survey questions from 2014 ('label' in Stata file)
# - raweducation2012: raw table from 2012 from Stata import (includes label and labels)
# - raweducation2014: raw table from 2014 from Stata import (includes label and labels)
#
# This function will also save two files in the "Dataout" directory:
# - education2012 --> education2012.csv
# - education2014 --> education2014.csv
#
# Note: column names for 2012/2014 are identical.
#
# Laura Hughes, lhughes@usaid.gov, July 2015
#_________________________________________________________________________________
setwd("~/Documents/USAID/Ethiopia/")
masterDir = getwd()

# Load libraries and helper functions.
source("R/setupFncns.r") 

library(foreign)

#_________________________________________________________________________________
# 2012
#_________________________________________________________________________________
setwd("Datain/wave2012/")

#_________________________________________________________________________________
# READ in data.
#_________________________________________________________________________________

raweducation2012 = read_dta('sect2_hh_w1.dta')

educationQuest2012 <- pullAttributes(raweducation2012)

education2012 = removeAttributes(raweducation2012)

eduCols = c("indivID2012", "hhID2012", "eaID2012", "rural", "hhWeight", "region", 
            "zone", "woreda", "town","subcity", "kebele", "ea", "hhIDTrunc", 
            "hhMemberID", "over5y", "literate",
            "attendedSchool", "whyGo2School", "highestGrade", "inSchool",
            "whyNotInSchool", "gradeInSchool", "schoolOrgType", "absentFromSchool",
            "whyAbsent", "howGet2School", "time2School", "scholarship",
            "amtScholarship", "amt2EduFees", "amt2EduSuppl", "goNextYr")

colnames(education2012) = eduCols

# Clean up dataset; combine regions into one.
education2012 = education2012 %>% 
  mutate(regionComb = ifelse(
    region == 2 | region == 5 | region == 6 |
      region == 12 | region == 13 | region == 15,
    "other", ifelse(region == 1, "Tigray",
                    ifelse(region == 3, "Amhara",
                           ifelse(region == 4, "Oromia",
                                  ifelse(region == 7, "SNNP", 
                                         ifelse(region == 14, "Addis Ababa", "unknown")))))),
    over5y = ifelse(over5y == "X", 1, ifelse (over5y == "",0,NA)),
    # Convert things to binary
    literate = ifelse(literate == 1, 1, ifelse(literate == 2, 0, NA)),
    attendedSchool = ifelse(attendedSchool == 1, 1, ifelse(attendedSchool == 2, 0, NA)),
    inSchool = ifelse(inSchool == 1, 1, ifelse(inSchool == 2, 0, NA)),
    absentFromSchool = ifelse(absentFromSchool == 1, 1,
                              ifelse(absentFromSchool == 2, 0, NA))
  )

setwd(masterDir)
write.csv(education2012, "Dataout/education2012.csv")
write.dta(education2012, "Dataout/education2012.dta", convert.factors = 'string')

#_________________________________________________________________________________
#_________________________________________________________________________________
# 2014
#_________________________________________________________________________________
setwd(masterDir)
setwd("Datain/wave2014/")

#_________________________________________________________________________________
# READ in data.
#_________________________________________________________________________________

raweducation2014 = read_dta('sect2_hh_w2.dta')

educationQuest2014 <- pullAttributes(raweducation2014)

education2014 = removeAttributes(raweducation2014)

eduCols = c("hhID2012", "hhID2014", "indivID2012", "indivID2014", 
            "eaID2012", "eaID2014","rural", "hhWeight", "region", 
            "zone", "woreda", "town","subcity", "kebele", "ea", "hhIDTrunc",
            "hhMemberID", "hhMemberName", "over5y", "literate",
            "attendedSchool", "whyGo2School", "highestGrade", "inSchool",
            "whyNotInSchool", "gradeInSchool", "schoolOrgType", "absentFromSchool",
            "whyAbsent", "howGet2School", "time2School", "scholarship",
            "amtScholarship", "amt2EduFees", "amt2EduSuppl", "goNextYr")

colnames(education2014) = eduCols

# Clean up dataset; combine regions into one.
education2014 = education2014 %>% 
  mutate(regionComb = ifelse(
    region == 2 | region == 5 | region == 6 |
      region == 12 | region == 13 | region == 15,
    "other", ifelse(region == 1, "Tigray",
                    ifelse(region == 3, "Amhara",
                           ifelse(region == 4, "Oromia",
                                  ifelse(region == 7, "SNNP", 
                                         ifelse(region == 14, "Addis Ababa", "unknown")))))),
    over5y = ifelse(over5y == "X", 1, ifelse (over5y == "",0,NA)),
    # Convert things to binary
    literate = ifelse(literate == 1, 1, ifelse(literate == 2, 0, NA)),
    attendedSchool = ifelse(attendedSchool == 1, 1, ifelse(attendedSchool == 2, 0, NA)),
    inSchool = ifelse(inSchool == 1, 1, ifelse(inSchool == 2, 0, NA)),
    absentFromSchool = ifelse(absentFromSchool == 1, 1,
                              ifelse(absentFromSchool == 2, 0, NA))
    # scholarship = ifelse(scholarship == 1, 1, ifelse(scholarship == 2, 0, NA)),
    # goNextYr = ifelse(goNextYr == 1, 1, ifelse(goNextYr == 2, 0, NA))
    )


# hh level aggregation
# highest education hh
# convert and merge grades.
# mean education
# % literate hh
# mother literate -- also in sec 1
# literacy by age
# school grade by age
# truancy by district

# 44% illiterate
education2014  %>% filter(over5y == 1)  %>% group_by(literate)  %>% summarise(num = n()) %>% mutate(pct = num/sum(num))
education2014  %>% filter(over5y == 1)  %>% group_by(regionComb, literate)  %>% summarise(num = n()) %>% mutate(pct = round(num/sum(num),2))

setwd(masterDir)

edu14 = education2014 %>% 
  select(-indivID2014) %>% 
  filter(indivID2012 != "")

write.csv(education2014, "Dataout/education2014.csv")
write.dta(edu14, "Dataout/education2014_noID14.dta", convert.factors = 'string')

# inidividual characteristics ---------------------------------------------

setwd("~/Documents/USAID/Ethiopia/Datain/wave2012/")
one12 = read_dta("sect1_hh_w1.dta"); 
indivChar12 = removeAttributes(one12)
indivChar12 = indivChar12  %>% select(indivID2012  = individual_id, ageY = hh_s1q04_a, ageM = hh_s1q04_b, 
                                      sex = hh_s1q03, relHH = hh_s1q02,
                                      relig = hh_s1q07, married = hh_s1q08, regionBorn = hh_s1q11,
                                      bioDadinHH = hh_s1q12, bioDadEduc = hh_s1q15, bioDadJob = hh_s1q20,
                                      bioMominHH = hh_s1q16, bioMomEduc = hh_s1q19, bioMomJob = hh_s1q21)
indivEd12 = full_join(indivChar12, education2012, by= "indivID2012")
indivEd12 = indivEd12 %>% 
  mutate(year = 2012) %>% 
  select(-eaID2012, -ea)

#----
setwd("~/Documents/USAID/Ethiopia/Datain/wave2014/")
one14 = read_dta("sect1_hh_w2.dta");
# oneQ=pullAttributes(one14)

indivChar14 = removeAttributes(one14)
indivChar14 = indivChar14  %>% select(indivID2014  = individual_id2, indivID2012  = individual_id, ageY = hh_s1q04_a, ageM = hh_s1q04_b, 
                                      sex = hh_s1q03, relHH = hh_s1q02,
                                      relig = hh_s1q07, married = hh_s1q08, regionBorn = hh_s1q11,
                                      bioDadinHH = hh_s1q12, bioDadEduc = hh_s1q15, bioDadJob = hh_s1q20,
                                      bioMominHH = hh_s1q16, bioMomEduc = hh_s1q19, bioMomJob = hh_s1q21)

indivEd14 = full_join(indivChar14, education2014, by= c("indivID2014", "indivID2012"))
indivEd14 = indivEd14 %>% 
  mutate(year = 2014) %>% 
  select(-eaID2012, -eaID2014, -ea,  -hhMemberName)


indiv14 = indivChar14 %>% 
  select(-indivID2014) %>% 
  filter(indivID2012 != "")


setwd(masterDir)
write.csv(indivChar14, "Dataout/indiv2014.csv")
write.csv(indivChar12, "Dataout/indiv2012.csv")
write.dta(indivChar12, "Dataout/indiv2012.dta", convert.factors = 'string')
write.dta(indiv14, "Dataout/indiv2014_noID14.dta", convert.factors = 'string')

# Combine Years -----------------------------------------------------------
ids12 = indivEd12 %>% 
  select(indivID2012, hhID2012)

ids14 = indivEd14 %>% 
  select(indivID2012, indivID2014, hhID2012, hhID2014)

base = full_join(ids12, ids14) # to get all indivIDs and hhIDs


indivEd12 = full_join(base, indivEd12, by = c("indivID2012", "hhID2012"))
indivEd14= full_join(base, indivEd14 %>% select(-hhID2012, -indivID2012), by = c("indivID2014", "hhID2014"))

indivEd = rbind(indivEd12, indivEd14)

setwd(masterDir)
write.csv(indivEd, "Dataout/indiv_education.csv")
write.dta(indivEd, "Dataout/indiv_education.dta", convert.factors = 'string')

# # plotting ----------------------------------------------------------------
# 
# ggplot(indivEd14, aes(x = ageY, y = highestGrade)) + geom_point()
# ggplot(indivEd14, aes(x = ageY, y = literate)) + geom_jitter(alpha = 0.1) + theme_bw()
# 
# litAge14 = indivEd14 %>%
#   filter(!is.na(literate)) %>% 
#   group_by(ageY) %>% 
#   summarise(totNum = n(), litPct = mean(literate))
# 
# 
# 

# 
# litAge12 = indivEd12 %>% 
#   filter(!is.na(literate)) %>% 
#   group_by(ageY) %>% 
#   summarise(totNum = n(), litPct = mean(literate))
# 
# ggplot(indivEd12, aes(x = ageY, y = highestGrade)) + geom_point()
# ggplot(indivEd12, aes(x = ageY, y = literate)) + geom_jitter(alpha = 0.1) + theme_bw()
# 
# ggplot(litAge, aes(x = ageY, y = litPct)) + 
#   geom_point(size = 3) +
#   theme_bw() +
#   coord_cartesian(x = c(4.5, 100)) +
#   ylab("percent literate") +
#   xlab("age") +
#   theme(axis.text = element_text(size = 14), 
#         axis.title = element_text(size = 16, face = "bold"), 
#         title = element_text(size = 18, face = "bold")) +
#   ggtitle("Literacy per age (2014)")
# 
# mergedAge = full_join(litAge12, litAge, by = "ageY") %>% 
#   filter(ageY > 4)
# 
# 
# ggplot(mergedAge, aes(x = ageY, y = litPct.x)) + 
#   geom_area(alpha = 0.5, colour = "blue") +
#   geom_area(aes(x = ageY, y = litPct.y), 
#             alpha = 0.5, colour = "orange") +
#   theme_bw() +
#   coord_cartesian(x = c(4.5, 100)) +
#   ylab("percent literate") +
#   xlab("age") +
#   theme(axis.text = element_text(size = 14), 
#         axis.title = element_text(size = 16, face = "bold"), 
#         title = element_text(size = 18, face = "bold")) +
#   ggtitle("Literacy per age") +
#   scale_color_manual(values = c("2012" = "blue", "2014" = "orange"))
# 
# 
# # posPrice = tim %>% filter(priceShk == 1)
# # negPrice = tim %>% filter(priceShk == 0)
# # 
# # leaflet() %>% addTiles()  %>% 
# #   # addCircles(negPrice$lon_dd_mod, negPrice$lat_dd_mod, radius = 5, fillColor = "blue",
# #                    # fillOpacity = 0.1, weight = 0) %>% setView(38, 8, zoom = 6) %>% 
# #   addCircleMarkers(posPrice$lon_dd_mod, posPrice$lat_dd_mod, radius = 5, fillColor = "red",
# #                    fillOpacity = 0.1, weight = 0) %>% setView(38, 8, zoom = 6)
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
# skankyPlot = function(data, alpha = 0.4, colorPalette= brewer.pal(10, 'paired'), 
# sizeLabels = 4, sizeWidth = 10)
# 
# Laura Hughes, lhughes@usaid.gov, USAID, September 2015.
# data should be a data frame.
# 
skankyPlot = function(data,
                      opacity = 0.4,
                      colorPalette= brewer.pal(10, 'Paired'),
                      sizeLabels = 4,
                      sizeFactor = 10,
                      sizeGap = 40){
  
  
  
  x = sh  %>% filter(shockClass != 'asset') %>%  
    group_by(shockClass, shockDescrip, cope1Cat) %>% 
    summarise(num= n()) %>% 
    filter(num > 200)
  
  
  
  # |shockClass |shockDescrip                |cope1Cat       | num|
  #   |:----------|:---------------------------|:--------------|---:|
  #   |hazard     |Drought                     |sold livestock | 215|
  #   |hazard     |Drought                     |used savings   | 203|
  #   |health     |Illness of household member |used savings   | 243|
  #   |price      |Price rise of food items    |sold livestock | 312|
  #   |price      |Price rise of food items    |used savings   | 349|
  
  nodeNames= colnames(x)[1:ncol(x)-1]
  numNodes = length(nodeNames)
  
  counter = 0
  xStep  = 1
  
  
  # generate the nodes --------------------------------------------------------------------
  
  nodes = NA
  
  
  for (i in 1:numNodes) {
    
    temp = x %>% 
      group_by_(nodeNames[i]) %>% 
      summarise(nObs = sum(num)) %>% 
      mutate(cs = cumsum(nObs))
    
    nRow = nrow(temp)
    
    
    temp =  
      temp %>% 
      mutate(offset = seq(from = 0, by = sizeGap, length.out = nRow),
             ymin = c(0, cs[1:nRow-1]) + offset,
             ymax = cs + offset,
             xmin = counter, 
             xmax = counter + xStep)
    
    colnames(temp)[1] = 'group' 
    
    nodes = rbind(nodes, temp)
    
    counter = counter + xStep*2
  }
  
  
  ggplot(data = nodes) +
    geom_rect(aes(xmin = xmin, xmax = xmax, 
                  ymin = ymin, ymax = ymax, fill = factor(ymin)),
              alpha = opacity) +
    geom_text(aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = group),
              size = 4) +
    theme_laura()

  
  
  


# rCharts -----------------------------------------------------------------

install.packages('rCharts')
install.packages('rjson')
library(rCharts)
library(rjson)


# x = read.csv('~/Desktop/test.csv')


x = sh  %>% filter(shockClass != 'asset', !is.na(shockClass), 
                   !is.na(shockScore),
                   !is.na(cope1Cat)) %>%  
  group_by(shockClass, shockScore) %>% 
  summarise(num= n())

colnames(x) <- c("source", "target", "value")

y = sh  %>% filter(shockClass != 'asset', !is.na(shockClass), 
                   !is.na(shockScore),
                   !is.na(cope1Cat)) %>%   
  mutate(cope1Cat = ifelse(cope1 < 5 | cope1 == 15 | cope1 == 18 | cope1 == 19,
                            cope1Cat,
                           ifelse(!is.na(cope1),
                           'other', NA))) %>% 
  group_by(shockScore, cope1Cat) %>% 
  summarise(num= n())

colnames(y) <- c("source", "target", "value")

x = rbind(x, y)

sankeyPlot <- rCharts$new()

sankeyPlot$set(
  data = x,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 500,
  height = 500,
  units = "TWh",
  title = "Sankey Diagram"
)

sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')

sankeyPlot
}setwd("~/Documents/USAID/Ethiopia/Datain/")
fivea=read_dta('sect5a_hh_w2.dta'); attrA <- pullAttributes(fivea)
fivea=read_dta('wave2014/sect5a_hh_w2.dta'); attrA <- pullAttributes(fivea)
fiveaTable = removeAttributes(fivea)


food2014  = fiveaTable %>% 
  select(household_id, household_id2,
         foodCode = hh_s5aq00, food = hh_s5aq0a,
         isConsumed = hh_s5aq01,
         totAmt = hh_s5aq02_a, totUnit = hh_s5aq02_b,
         purchAmt = hh_s5aq03_a, purchUnit = hh_s5aq03_b,
         prodAmt = hh_s5aq05_a, prodUnit = hh_s5aq05_b) %>% 
  mutate(year = 2014)

ptrack = data %>% 
  select(household_id, household_id2, year, ptrack, priceShk)

# select only panel
food2014 = right_join(food2014, ptrack) 

food2014 = food2014 %>% 
  mutate(pctPurch = ifelse(purchAmt == 0, 0, 
                           ifelse(
                             totUnit == purchUnit, purchAmt / totAmt, 
                             NA)),
         pctProd = ifelse(prodAmt == 0, 0, 
                          ifelse(totUnit == prodUnit, prodAmt / totAmt, NA)
         ),
         totPct = pctPurch + pctProd
         ) 

ggplot(food2014, aes(x = food, y = pctPurch, order = pctPurch)) + 
  # stat_summary(fun.y = mean,  geom = 'point', size = 5) +
  geom_jitter(size = 5, alpha = 0.2) + 
  theme_jointplot() + coord_cartesian(xlim = c(-0.1,1.1)) + coord_flip()


ggplot(data %>% filter(ftfzone != 99), aes(y = worryLackFood, x = maxDaysLimit, colour = factor(year))) + 
  stat_summary(fun.y = mean, geom = 'point', size =6) +
  facet_wrap(~ftfzone) +
  theme_jointplot() + theme(legend.position = 'left')

ggplot(data, aes(y = worryLackFood, x = maxDaysLimit, colour = factor(year))) + 
  stat_smooth()+
  theme_jointplot()


ggplot(food2014, aes(x = pctProd, y = priceShk)) + 
  # stat_summary(fun.y = mean,  geom = 'point', size = 5) +
  facet_wrap(~food) +
  geom_smooth()+
  # geom_jitter(size = 5, alpha = 0.2) + 
  theme_jointplot() + coord_cartesian(xlim = c(-0.1,1.1), ylim = c(-0.1, 1.1)) 

ggplot(food2014, aes(x = pctPurch, y = priceShk)) + 
  # stat_summary(fun.y = mean,  geom = 'point', size = 5) +
  facet_wrap(~food) +
  geom_smooth()+
  # geom_jitter(size = 5, alpha = 0.2) + 
  theme_jointplot() + coord_cartesian(xlim = c(-0.1,1.1), ylim = c(-0.1, 1.1)) 


x = fiveaTable %>%
filter(!is.na(hh_s5aq02_b)) %>%
group_by(tot = hh_s5aq02_b,
purch = hh_s5aq03_b,
own = hh_s5aq05_b) %>%
summarise(r=n()) %>%
arrange(desc(r))




data = data %>% 
  mutate()


