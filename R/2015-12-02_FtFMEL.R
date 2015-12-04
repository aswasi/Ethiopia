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
       dpi = 300)