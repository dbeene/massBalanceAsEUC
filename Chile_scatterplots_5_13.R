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

ggplot(chile,aes(x = WaterAS1yr,y=WaterAS6yr)) + geom_point() + 
  stat_smooth(method = 'lm') + geom_abline(lty=2)

#### Create new var (WaterArsenic2) to replace 0 values with 0.1
chile$WaterAS1yr2<-ifelse(chile$WaterAS1yr==0,0.1,chile$WaterAS1yr)
chile$WaterAS6yr2<-ifelse(chile$WaterAS6yr==0,0.1,chile$WaterAS6yr)

## Plot 1: Full scale, all participants, 1 year average water arsenic
chile = subset(chile,!is.na(UrinaryArsenic))

minwater<-min(chile$WaterAS1yr2)
maxwater<-max(chile$WaterAS1yr2)
minurine<-min(chile$UrinaryArsenic)
maxurine<-max(chile$UrinaryArsenic)


#get outlier indices, those with z score > 3
zs = (chile$UrinaryArsenic - mean(chile$UrinaryArsenic))/sd(chile$UrinaryArsenic)
outs = which(zs>3)

healsminx = 0.1
healsmaxx = 864
healsminy = 1
healsmaxy = 2000

pdf(file = 'plot_pdfs/Chile_plot1A.pdf', width = 10, height = 9)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1", size = 3) + 
  #geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(healsminx, 900), breaks = c(0,200,400,600,800),labels=c("0","200","400","600","800"))+
  scale_y_continuous(limits = c(healsminy, healsmaxy), breaks = c(0,500,1000,1500,2000),labels=c("0","500","1000","1500","2000"))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank()) # adds a "one-to-one" line
dev.off()

pdf(file = 'plot_pdfs/Chile_plot1B.pdf', width = 10, height = 9)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1", size = 3) + 
  geom_smooth(method="lm",color="#318175", fullrange = T)+
  scale_x_continuous(limits = c(healsminx, 900), breaks = c(0,200,400,600,800),labels=c("0","200","400","600","800"))+
  scale_y_continuous(limits = c(healsminy, healsmaxy), breaks = c(0,500,1000,1500,2000),labels=c("0","500","1000","1500","2000"))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank()) # adds a "one-to-one" line
dev.off()

pdf(file = 'plot_pdfs/Chile_plot1B_noSE.pdf', width = 10, height = 9)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1", size = 3) + 
  geom_smooth(method="lm",color="#318175",
              se = F, fullrange = T)+
  scale_x_continuous(limits = c(healsminx, 900), breaks = c(0,200,400,600,800),labels=c("0","200","400","600","800"))+
  scale_y_continuous(limits = c(healsminy, healsmaxy), breaks = c(0,500,1000,1500,2000),labels=c("0","500","1000","1500","2000"))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank()) # adds a "one-to-one" line
dev.off()



### Recalibrating for arsenobetaine

## Recalibrate total urinary arsenic by arsenobetaine levels- see PMCID: PMC5065621 for details
# Recalibrated values = residuals + conditional mean 

fit <- glm(log(UrinaryArsenic)~log(UrinaryAsBet),
           data=chile)
b <- summary(fit)$coeff[1,1]  						# This is the conditional mean from the model for individuals with 0 log ab_cations (intercept B0, condition mean when arsenobetaine=1)
chile$ln.total <- b             							# Create a variable in the database with the value of the same baseline conditional mean for everybody- this gives everybody the urinary total arsenic value that corresponds to the conditional mean
residuals.total <- residuals(fit,type="response") 			# Save the residuals- this is the part that doesn't depend on AB and also gives you the variability between people
chile.lowab <- subset(chile, chile$UrinaryAsBet < 1)
fit.b0 <- glm(log(UrinaryArsenic)~offset(ln.total),    #offset = you are not obtaining a coefficient that is multiplying this variable (b1)- so ln.total is the same value for everyone (b)
              # offset tells you that "this variable is not a real coefficinet- it is just a stand in value- so instead give me another coefficient (alpha0) to calibrate
              data=chile.lowab) 		 	# Intercept from this model is the estimated constant factor to recalibrate the baseline conditional mean into the adjusted marginal mean
chile$ln.total.adj <- summary(fit.b0)$coeff[1,1]+b+residuals.total	# Add the conditional mean, the calibration factor (marginal mean of the log-transformed total arsenic) and the residuals

chile$total.adj <- exp(chile$ln.total.adj)

chile$total.minus = chile$UrinaryArsenic-chile$UrinaryAsBet

pdf(file = 'plot_pdfs/Chile_plot1C.pdf', width = 10, height = 9)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1", alpha = 0.8, size = 3) + 
  geom_smooth(method="lm",color="#318175", fullrange = T)+
  geom_point(aes(y = total.minus), color="#fdbf6f", alpha = 0.8, size = 3) + 
  geom_smooth(aes(y = total.minus), method="lm",color="#ff7f00", fullrange = T)+
  scale_x_continuous(limits = c(healsminx, 900), breaks = c(0,200,400,600,800),labels=c("0","200","400","600","800"))+
  scale_y_continuous(limits = c(healsminy, healsmaxy), breaks = c(0,500,1000,1500,2000),labels=c("0","500","1000","1500","2000"))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank()) # adds a "one-to-one" line
dev.off()


pdf(file = 'plot_pdfs/Chile_plot1C_noSE.pdf', width = 10, height = 9)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1", alpha = 0.8, size = 3) + 
  geom_smooth(method="lm",color="#318175", fullrange = T,se=F)+
  geom_point(aes(y = total.minus), color="#fdbf6f", alpha = 0.8, size = 3) + 
  geom_smooth(aes(y = total.minus), method="lm",color="#ff7f00", fullrange = T, se = F)+
  scale_x_continuous(limits = c(healsminx, 900), breaks = c(0,200,400,600,800),labels=c("0","200","400","600","800"))+
  scale_y_continuous(limits = c(healsminy, healsmaxy), breaks = c(0,500,1000,1500,2000),labels=c("0","500","1000","1500","2000"))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank()) # adds a "one-to-one" line
dev.off()         

pdf(file = 'plot_pdfs/Chile_plot1C_adj.pdf', width = 10, height = 9)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1", alpha = 0.8, size = 3) + 
  geom_smooth(method="lm",color="#318175", fullrange = T)+
  geom_point(aes(y = total.adj), color="#fdbf6f", alpha = 0.8, size = 3) + 
  geom_smooth(aes(y = total.adj), method="lm",color="#ff7f00", fullrange = T)+
  scale_x_continuous(limits = c(healsminx, 900), breaks = c(0,200,400,600,800),labels=c("0","200","400","600","800"))+
  scale_y_continuous(limits = c(healsminy, healsmaxy), breaks = c(0,500,1000,1500,2000),labels=c("0","500","1000","1500","2000"))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank()) # adds a "one-to-one" line
dev.off()


pdf(file = 'plot_pdfs/Chile_plot1C_adj_noSE.pdf', width = 10, height = 9)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1", alpha = 0.8, size = 3) + 
  geom_smooth(method="lm",color="#318175", fullrange = T,se=F)+
  geom_point(aes(y = total.adj), color="#fdbf6f", alpha = 0.8, size = 3) + 
  geom_smooth(aes(y = total.adj), method="lm",color="#ff7f00", fullrange = T, se = F)+
  scale_x_continuous(limits = c(healsminx, 900), breaks = c(0,200,400,600,800),labels=c("0","200","400","600","800"))+
  scale_y_continuous(limits = c(healsminy, healsmaxy), breaks = c(0,500,1000,1500,2000),labels=c("0","500","1000","1500","2000"))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank()) # adds a "one-to-one" line
dev.off()


#######
pdf(file = 'plot_pdfs/Chile_plot2A.pdf', width = 10, height = 9)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1", size = 3) + 
  #geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(0, 65), breaks = c(0,15,30,45,60),labels=c("0","15","30","45","60"))+
  scale_y_continuous(limits = c(0, 670), breaks = c(0,160,320,480,640),labels=c("0","160","320","480","640"))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank()) # adds a "one-to-one" line
dev.off()

pdf(file = 'plot_pdfs/Chile_plot2B.pdf', width = 10, height = 9)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1", size = 3) + 
  geom_smooth(method="lm",color="#318175", fullrange = T)+
  scale_x_continuous(limits = c(0, 65), breaks = c(0,15,30,45,60),labels=c("0","15","30","45","60"))+
  scale_y_continuous(limits = c(0, 670), breaks = c(0,160,320,480,640),labels=c("0","160","320","480","640"))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank()) # adds a "one-to-one" line
dev.off()

pdf(file = 'plot_pdfs/Chile_plot2C.pdf', width = 10, height = 9)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1", alpha = 0.8, size = 3) + 
  geom_smooth(method="lm",color="#318175", fullrange = T)+
  geom_point(aes(y = total.minus), color="#fdbf6f", alpha = 0.8, size = 3) + 
  geom_smooth(aes(y = total.minus), method="lm",color="#ff7f00", fullrange = T)+
  scale_x_continuous(limits = c(0, 65), breaks = c(0,15,30,45,60),labels=c("0","15","30","45","60"))+
  scale_y_continuous(limits = c(0, 670), breaks = c(0,160,320,480,640),labels=c("0","160","320","480","640"))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank()) # adds a "one-to-one" line
dev.off()

pdf(file = 'plot_pdfs/Chile_plot2C_adj.pdf', width = 10, height = 9)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1", alpha = 0.8, size = 3) + 
  geom_smooth(method="lm",color="#318175", fullrange = T)+
  geom_point(aes(y = total.adj), color="#fdbf6f", alpha = 0.8, size = 3) + 
  geom_smooth(aes(y = total.adj), method="lm",color="#ff7f00", fullrange = T)+
  scale_x_continuous(limits = c(0, 65), breaks = c(0,15,30,45,60),labels=c("0","15","30","45","60"))+
  scale_y_continuous(limits = c(0, 670), breaks = c(0,160,320,480,640),labels=c("0","160","320","480","640"))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank()) # adds a "one-to-one" line
dev.off()

#quantile(chile$WaterAS1yr2,c(0,.25,.5,.75,1))
#0%  25%  50%  75% 100% 
#0.1 36.3 36.3 40.0 60.0 

#can't do quartiles

chile$clust.id = kmeans(chile$WaterAS1yr2,4,
                        nstart = 1000)$cluster

chile.bin = chile %>% group_by(clust.id) %>% 
  dplyr::summarize(xmean = mean(WaterAS1yr2),
                   xlo = quantile(WaterAS1yr2,0.025),
                   xhi = quantile(WaterAS1yr2,0.975),
                   xsd = sd(WaterAS1yr2),
                   ymean = mean(UrinaryArsenic),
                   ylo = quantile(UrinaryArsenic,0.025),
                   yhi = quantile(UrinaryArsenic,0.975),
                   ysd = sd(UrinaryArsenic),
                   adjmean = mean(total.adj),
                   adjlo = quantile(total.adj,0.025),
                   adjhi = quantile(total.adj,0.975),
                   adjsd = sd(total.adj),
                   minusmean = mean(total.minus),
                   minuslo = quantile(total.minus,0.025),
                   minushi = quantile(total.minus,0.975),
                   minussd = sd(total.minus))




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


pdf(file = 'plot_pdfs/Chile_binplot_qtile_adj.pdf', width = 10, height = 9)
ggplot(chile.bin, aes(x=xmean, y=ymean,
                      xmax = xhi, xmin = xlo,
                      ymax = yhi, ymin = ylo)) +
  geom_errorbar() + geom_errorbarh() + 
  geom_point(#color="#80cdc1", 
    size = 3) +
  geom_errorbar(color = 'orange', aes(ymin = adjlo, ymax = adjhi,x = xmean + 1)) + 
  geom_errorbarh(color = 'orange',aes(y = adjmean,x = xmean + 1)) + 
  geom_point(color = 'orange', size = 3, aes(y = adjmean,x = xmean + 1)) + 
  scale_x_continuous(expand = c(0,0),limits = c(0, 500), breaks = c(0,100,200,300,400,500),labels=c("0","100","200","300","400",'500'))+
  scale_y_continuous(expand = c(0,0),limits = c(0, 400), breaks = c(0,100,200,300,400))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_bw(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank(),
                                   panel.grid.major = element_blank()) # adds a "one-to-one" line
dev.off()

pdf(file = 'plot_pdfs/Chile_binplot_1SD_adj.pdf', width = 10, height = 9)
# 1 standard deviation goes below zero
ggplot(chile.bin, aes(x=xmean, y=ymean,
                      xmax = xmean + xsd, xmin = xmean - xsd,
                      ymax = ymean + ysd, ymin = ymean - ysd)) +
  geom_errorbar() + geom_errorbarh() + 
  geom_point(#color="#80cdc1", 
    size = 3) +
  geom_errorbar(color = 'orange', aes(ymin = adjmean - adjsd, ymax = adjmean + adjsd,x = xmean + 1)) + 
  geom_errorbarh(color = 'orange',aes(y = adjmean,xmin = xmean - xsd + 1, xmax = xmean + xsd + 1)) + 
  geom_point(color = 'orange', size = 3, aes(y = adjmean,x = xmean + 1)) + 
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
  geom_errorbar(color = '#fdbf6f', aes(ymin = minuslo, ymax = minushi,x = xmean + 1)) + 
  #geom_errorbarh(color = 'orange',aes(y = minusmean,x = xmean + 1)) + 
  geom_point(color = '#fdbf6f', size = 3, aes(y = minusmean,x = xmean + 1)) + 
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
  geom_errorbar(color = '#fdbf6f', aes(ymin = minuslo, ymax = minushi,x = xmean + 1)) + 
  #geom_errorbarh(color = 'orange',aes(y = minusmean,x = xmean + 1)) + 
  geom_point(color = '#fdbf6f', size = 3, aes(y = minusmean,x = xmean + 1)) + 
  scale_x_continuous(expand = c(0,0),limits = c(0, 65), breaks = c(0,15,30,45,60))+
  scale_y_continuous(expand = c(0,0),limits = c(0, 670), breaks = c(0,160,320,480,640))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) # adds a "one-to-one" line
dev.off()


pdf(file = 'plot_pdfs/Chile_binplot_1SD_subtract.pdf', width = 10, height = 9)
# 1 standard deviation goes below zero
ggplot(chile.bin, aes(x=xmean, y=ymean,
                      xmax = xmean + xsd, xmin = xmean - xsd,
                      ymax = ymean + ysd, ymin = ymean - ysd)) +
  geom_errorbar() + geom_errorbarh() + 
  geom_point(#color="#80cdc1", 
    size = 3) +
  geom_errorbar(color = 'orange', aes(ymin = minusmean - minussd, ymax = minusmean + minussd,x = xmean + 1)) + 
  geom_errorbarh(color = 'orange',aes(y = minusmean,xmin = xmean - xsd + 1, xmax = xmean + xsd + 1)) + 
  geom_point(color = 'orange', size = 3, aes(y = minusmean,x = xmean + 1)) + 
  scale_x_continuous(expand = c(0,0),limits = c(0, 65), breaks = c(0,15,30,45,60))+
  scale_y_continuous(expand = c(0,0),limits = c(0, 670), breaks = c(0,160,320,480,640))+
  labs(title="Chile Case-Control Study", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_bw(base_size = 20)+
  geom_abline(color="red") + theme(panel.grid.minor = element_blank(),
                                   panel.grid.major = element_blank()) # adds a "one-to-one" line
dev.off()



