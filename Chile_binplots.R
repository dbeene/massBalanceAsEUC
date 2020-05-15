##### Mass balance scatterplots
##### 28 April 2020

#### Load libraries
#library(openxlsx)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(lubridate)
#### Load data
chile<-read.csv("Chile_Arsenic_Data_Redux.csv")


#Drinking water Arsenic concentration was based on the 6-year average of arsenic in public water 
#supply averaged from EPA’s Six Year Review of Contaminant Occurrence database. 

chile = chile %>% group_by(SubjectID) %>% mutate(yrin = year(DateEnr)) %>% 
  mutate(WaterAS6yr = mean(Exp_WaterAS[(yrin - Exp_Year) %in% (0:5)]),
         WaterAS1yr = Exp_WaterAS[yrin == Exp_Year])

chile = chile %>% filter(!duplicated(SubjectID))

#### Create new var (WaterArsenic2) to replace 0 values with 0.1
chile$WaterAS1yr2<-ifelse(chile$WaterAS1yr==0,0.1,chile$WaterAS1yr)
chile$WaterAS6yr2<-ifelse(chile$WaterAS6yr==0,0.1,chile$WaterAS6yr)

chile = subset(chile,!is.na(UrinaryArsenic))

#quantile(chile$WaterAS1yr2,c(0,.25,.5,.75,1))
#0%  25%  50%  75% 100% 
#0.1 36.3 36.3 40.0 60.0 

#can't do quartiles

#group data via k means clustering
chile$clust.id = kmeans(chile$WaterAS1yr2,4,
                        nstart = 1000)$cluster

#summarize by bin

chile.bin = chile %>% group_by(clust.id) %>% 
  dplyr::summarize(xmean = mean(WaterAS1yr2),
                   xlo = quantile(WaterAS1yr2,0.025),
                   xhi = quantile(WaterAS1yr2,0.975),
                   xsd = sd(WaterAS1yr2),
                   ymean = mean(UrinaryArsenic),
                   ylo = quantile(UrinaryArsenic,0.025),
                   yhi = quantile(UrinaryArsenic,0.975),
                   ysd = sd(UrinaryArsenic),
                   minusmean = mean(total.minus),
                   minuslo = quantile(total.minus,0.025),
                   minushi = quantile(total.minus,0.975),
                   minussd = sd(total.minus))

#x shift for adjusted points / bars
off = 1

pdf(file = 'plot_pdfs/Chile_binplot_qtile.pdf', width = 10, height = 9)
ggplot(chile.bin, aes(x=xmean, y=ymean,
                      xmax = xhi, xmin = xlo,
                      ymax = yhi, ymin = ylo)) +
  geom_errorbar() + geom_errorbarh() + 
  geom_point(#color="#80cdc1", 
    size = 3) +
  scale_x_continuous(expand = c(0,0),limits = c(0, 500), breaks = c(0,100,200,300,400,500),labels=c("0","100","200","300","400",'500'))+
  scale_y_continuous(expand = c(0,0),limits = c(0, 400), breaks = c(0,100,200,300,400))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_bw(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank(),
                                   panel.grid.major = element_blank()) # adds a "one-to-one" line
dev.off()

pdf(file = 'plot_pdfs/Chile_binplot_1SD.pdf', width = 10, height = 9)
# 1 standard deviation goes below zero
ggplot(chile.bin, aes(x=xmean, y=ymean,
                      xmax = xmean + xsd, xmin = xmean - xsd,
                      ymax = ymean + ysd, ymin = ymean - ysd)) +
  geom_errorbar() + geom_errorbarh() + 
  geom_point(#color="#80cdc1", 
    size = 3) +
  scale_x_continuous(expand = c(0,0),limits = c(0, 500), breaks = c(0,100,200,300,400,500),labels=c("0","100","200","300","400",'500'))+
  scale_y_continuous(expand = c(0,0),limits = c(0, 400), breaks = c(0,100,200,300,400))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_bw(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank(),
                                   panel.grid.major = element_blank()) # adds a "one-to-one" line
dev.off()

pdf(file = 'plot_pdfs/Chile_binplot_qtile_subtract.pdf', width = 10, height = 9)
ggplot(chile.bin, aes(x=xmean, y=ymean,
                      xmax = xhi, xmin = xlo,
                      ymax = yhi, ymin = ylo)) +
  geom_errorbar(color = '#80cdc1') + #geom_errorbarh() + 
  geom_point(color="#80cdc1", 
    size = 3) +
  geom_errorbar(color = '#fdbf6f', aes(ymin = minuslo, ymax = minushi,x = xmean + off)) + 
  #geom_errorbarh(color = 'orange',aes(y = minusmean,x = xmean + 1)) + 
  geom_point(color = '#fdbf6f', size = 3, aes(y = minusmean,x = xmean + off)) + 
  scale_x_continuous(expand = c(0,0),limits = c(0, 65), breaks = c(0,15,30,45,60))+
  scale_y_continuous(expand = c(0,0),limits = c(0, 670), breaks = c(0,160,320,480,640))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_bw(base_size = 20)+
  #geom_abline(color="red") + 
  theme(panel.grid.minor = element_blank(),
                                   panel.grid.major = element_blank()) # adds a "one-to-one" line
dev.off()


pdf(file = 'plot_pdfs/Chile_binplot_qtile_subtract_11.pdf', width = 10, height = 9)
ggplot(chile.bin, aes(x=xmean, y=ymean,
                      xmax = xhi, xmin = xlo,
                      ymax = yhi, ymin = ylo)) +
  geom_abline(color="red") + 
  geom_errorbar(color = '#80cdc1') + #geom_errorbarh() + 
  geom_point(color="#80cdc1", 
             size = 3) +
  geom_errorbar(color = '#fdbf6f', aes(ymin = minuslo, ymax = minushi,x = xmean + off)) + 
  #geom_errorbarh(color = 'orange',aes(y = minusmean,x = xmean + 1)) + 
  geom_point(color = '#fdbf6f', size = 3, aes(y = minusmean,x = xmean + off)) + 
  scale_x_continuous(expand = c(0,0),limits = c(0, 65), breaks = c(0,15,30,45,60))+
  scale_y_continuous(expand = c(0,0),limits = c(0, 670), breaks = c(0,160,320,480,640))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) # adds a "one-to-one" line
dev.off()


