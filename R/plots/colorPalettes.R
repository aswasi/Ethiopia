n = 111


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
mid = '#ddd3ba'
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


df = data.frame(x = 1:n, y = 1, haz = hazards, 
                gwr = BuBr, haz1 = haz1, PuPiYl = PuPiYl,
                divPlum = PlOrYl)

ggplot(df) +
  # geom_rect(aes(xmin = x, xmax = x + 1, ymin = y - .5, ymax = y - 1.5, fill = haz1)) +
  geom_rect(aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 1, fill = haz)) +
  # geom_rect(aes(xmin = x, xmax = x + 1, ymin = y + 1.5, ymax = y + 2.5, fill = gwr)) +
  geom_rect(aes(xmin = x, xmax = x + 1, ymin = y + 1.5, ymax = y + 2.5, fill = gwr2)) +
  geom_rect(aes(xmin = x, xmax = x + 1, ymin = y + 3, ymax = y + 4, fill = haz1)) +
  geom_rect(aes(xmin = x, xmax = x + 1, ymin = y + 4.5, ymax = y + 5.5, fill = PuPiYl)) +
  geom_rect(aes(xmin = x, xmax = x + 1, ymin = y + 6, ymax = y + 7, fill = divPlum)) +
  scale_fill_identity()+
  theme_blankLH() + theme(panel.background = element_rect(fill = '#e9e9e9'))

library(colorspace)

rgb = hex2RGB(gwr2)


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

gwr_BuBr = col2rgb(gwr2)

plums = col2rgb(hazards)
