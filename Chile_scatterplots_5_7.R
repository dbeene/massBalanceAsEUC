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
minwater<-min(chile$WaterAS1yr2,na.rm=T)
maxwater<-max(chile$WaterAS1yr2,na.rm=T)
minurine<-min(chile$UrinaryArsenic,na.rm=T)
maxurine<-max(chile$UrinaryArsenic,na.rm=T)

chile = subset(chile,!is.na(UrinaryArsenic))

#get outlier indices, those with z score > 3
zs = (chile$UrinaryArsenic - mean(chile$UrinaryArsenic))/sd(chile$UrinaryArsenic)
outs = which(zs>3)

pdf(file = 'plot_pdfs/Chile_water_vs_total_all.pdf', width = 5, height = 5)
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(minwater, maxwater), expand = c(0,0),
                     breaks = seq(minwater, maxwater,length=5),labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), expand = c(0,0),
                     breaks = seq(minurine, maxurine,length=5),labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="Chile, all subjects", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(color="red") # adds a "one-to-one" line
dev.off()

pdf(file = 'plot_pdfs/Chile_water_vs_total_restricted.pdf', width = 5, height = 5)
ggplot(chile[-outs,], aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(minwater, maxwater), expand = c(0,0),
                     breaks = seq(minwater, maxwater,length=5),labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), expand = c(0,0),
                     breaks = seq(minurine, maxurine,length=5),labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="Chile, dropping 11 outliers", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(color="red") # adds a "one-to-one" line
dev.off()


##same with 6 year average
minwater<-min(chile$WaterAS6yr2,na.rm=T)
maxwater<-max(chile$WaterAS6yr2,na.rm=T)

pdf(file = 'plot_pdfs/Chile_water_vs_total_all_6yr.pdf', width = 5, height = 5)
ggplot(chile, aes(x=WaterAS6yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(minwater, maxwater), expand = c(0,0),
                     breaks = seq(minwater, maxwater,length=5),labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), expand = c(0,0),
                     breaks = seq(minurine, maxurine,length=5),labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="Chile, all subjects", x ="Water arsenic (µg/L, 6 year average)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(color="red") # adds a "one-to-one" line

dev.off()

pdf(file = 'plot_pdfs/Chile_water_vs_total_restricted_6yr.pdf', width = 5, height = 5)
ggplot(chile[-outs,], aes(x=WaterAS6yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(minwater, maxwater), expand = c(0,0),
                     breaks = seq(minwater, maxwater,length=5),labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), expand = c(0,0),
                     breaks = seq(minurine, maxurine,length=5),labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="Chile, dropping 11 outliers", x ="Water arsenic (µg/L, 6 year average)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(color="red") # adds a "one-to-one" line

dev.off()


## Plot 2: Nope.. No water arsenic levels were above 70 ug/L

### Plot 3: Recalibrating for arsenobetaine

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

#do same while dropping outliers (some observations have high leverage)

chile.sub = chile[-outs,]

fit.sub <- glm(log(UrinaryArsenic)~log(UrinaryAsBet),
           data=chile.sub)
b.sub <- summary(fit.sub)$coeff[1,1]  						# This is the conditional mean from the model for individuals with 0 log ab_cations (intercept B0, condition mean when arsenobetaine=1)
chile.sub$ln.total <- b.sub             							# Create a variable in the database with the value of the same baseline conditional mean for everybody- this gives everybody the urinary total arsenic value that corresponds to the conditional mean
residuals.total.sub <- residuals(fit.sub,type="response") 			# Save the residuals- this is the part that doesn't depend on AB and also gives you the variability between people
chile.lowab <- subset(chile.sub, UrinaryAsBet < 1)
fit.b0.sub <- glm(log(UrinaryArsenic)~offset(ln.total),    #offset = you are not obtaining a coefficient that is multiplying this variable (b1)- so ln.total is the same value for everyone (b)
              # offset tells you that "this variable is not a real coefficinet- it is just a stand in value- so instead give me another coefficient (alpha0) to calibrate
              data=chile.lowab) 		 	# Intercept from this model is the estimated constant factor to recalibrate the baseline conditional mean into the adjusted marginal mean
chile.sub$ln.total.adj <- summary(fit.b0.sub)$coeff[1,1]+b.sub+residuals.total.sub	# Add the conditional mean, the calibration factor (marginal mean of the log-transformed total arsenic) and the residuals


# All the three together are the log-transformed arsenobetaine-adjusted individual total arsenic levels)
# alpha is the first thing (fit.b0$coeff(1,1), b is the conditional mean, and the residuals are the most important part

describe(chile$ln.total.adj)
chile$total.adj <- exp(chile$ln.total.adj)
describe(chile$total.adj) # New var of total arsenic recalibrated for urinary arsenobetaine

recalibratedminwater<-min(chile$WaterAS1yr2,na.rm=T)
recalibratedmaxwater<-max(chile$WaterAS1yr2,na.rm=T)
recalibratedminurine<-min(chile$total.adj,na.rm=T)
recalibratedmaxurine<-max(chile$total.adj,na.rm=T)

### Plot
pdf(file = 'plot_pdfs/Chile_water_vs_recalibrated_total_all.pdf', width = 5, height = 5)

ggplot(chile, aes(x=WaterAS1yr2, y=total.adj)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(recalibratedminwater, recalibratedmaxwater), breaks = seq(recalibratedminwater, recalibratedmaxwater,length=5),
                     labels=round(seq(recalibratedminwater,recalibratedmaxwater,length=5)))+
  scale_y_continuous(limits = c(recalibratedminurine, recalibratedmaxurine), breaks = seq(recalibratedminurine, recalibratedmaxurine,length=5),
                     labels=round(seq(recalibratedminurine,recalibratedmaxurine,length=5)))+
  labs(title="Chile, all subjects, recalibrated for arsenobetaine", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic, recalibrated for arsenobetaine (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line

dev.off()

##6 yr average
recalibratedminwater<-min(chile$WaterAS6yr2,na.rm=T)
recalibratedmaxwater<-max(chile$WaterAS6yr2,na.rm=T)

pdf(file = 'plot_pdfs/Chile_water_vs_recalibrated_total_all_6yr.pdf', width = 5, height = 5)

ggplot(chile, aes(x=WaterAS6yr2, y=total.adj)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(recalibratedminwater, recalibratedmaxwater), breaks = seq(recalibratedminwater, recalibratedmaxwater,length=5),
                     labels=round(seq(recalibratedminwater,recalibratedmaxwater,length=5)))+
  scale_y_continuous(limits = c(recalibratedminurine, recalibratedmaxurine), breaks = seq(recalibratedminurine, recalibratedmaxurine,length=5),
                     labels=round(seq(recalibratedminurine,recalibratedmaxurine,length=5)))+
  labs(title="Chile, all subjects, recalibrated for arsenobetaine", x ="Water arsenic (µg/L, 6 year average)", y = "Urinary arsenic, recalibrated for arsenobetaine (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line

dev.off()

##version without outliers
describe(chile.sub$ln.total.adj)
chile.sub$total.adj <- exp(chile.sub$ln.total.adj)
describe(chile.sub$total.adj) # New var of total arsenic recalibrated for urinary arsenobetaine

recalibratedminwater<-min(chile.sub$WaterAS1yr2,na.rm=T)
recalibratedmaxwater<-max(chile.sub$WaterAS1yr2,na.rm=T)
recalibratedminurine<-min(chile.sub$total.adj,na.rm=T)
recalibratedmaxurine<-max(chile.sub$total.adj,na.rm=T)

### Plot
pdf(file = 'plot_pdfs/Chile_water_vs_recalibrated_total_restricted.pdf', width = 5, height = 5)

ggplot(chile.sub, aes(x=WaterAS1yr2, y=total.adj)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(recalibratedminwater, recalibratedmaxwater), breaks = seq(recalibratedminwater, recalibratedmaxwater,length=5),
                     labels=round(seq(recalibratedminwater,recalibratedmaxwater,length=5)))+
  scale_y_continuous(limits = c(recalibratedminurine, recalibratedmaxurine), breaks = seq(recalibratedminurine, recalibratedmaxurine,length=5),
                     labels=round(seq(recalibratedminurine,recalibratedmaxurine,length=5)))+
  labs(title="Chile, dropping 11 outliers\nrecalibrated for arsenobetaine", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic, recalibrated for arsenobetaine (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line

dev.off()

##6 yr average
recalibratedminwater<-min(chile.sub$WaterAS6yr2,na.rm=T)
recalibratedmaxwater<-max(chile.sub$WaterAS6yr2,na.rm=T)

pdf(file = 'plot_pdfs/Chile_water_vs_recalibrated_total_restricted_6yr.pdf', width = 5, height = 5)

ggplot(chile.sub, aes(x=WaterAS6yr2, y=total.adj)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(recalibratedminwater, recalibratedmaxwater), breaks = seq(recalibratedminwater, recalibratedmaxwater,length=5),
                     labels=round(seq(recalibratedminwater,recalibratedmaxwater,length=5)))+
  scale_y_continuous(limits = c(recalibratedminurine, recalibratedmaxurine), breaks = seq(recalibratedminurine, recalibratedmaxurine,length=5),
                     labels=round(seq(recalibratedminurine,recalibratedmaxurine,length=5)))+
  labs(title="Chile, dropping 11 outliers\nrecalibrated for arsenobetaine", x ="Water arsenic (µg/L, 6 year average)", y = "Urinary arsenic, recalibrated for arsenobetaine (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line

dev.off()
###Plotting against total inorganic arsenic, all data
minwater<-min(chile$WaterAS1yr2,na.rm=T)
maxwater<-max(chile$WaterAS1yr2,na.rm=T)
minurine<-min(chile$UrinaryInArsenic,na.rm=T)
maxurine<-max(chile$UrinaryInArsenic,na.rm=T)

pdf(file = 'plot_pdfs/Chile_water_vs_inorganic_all.pdf', width = 5, height = 5)

ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryInArsenic)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(minwater, maxwater), breaks = seq(minwater, maxwater,length=5),
                     labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), breaks = seq(minurine, maxurine,length=5),
                     labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="Chile, all subjectsin/ninorganic arsenic", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary inorganic arsenic (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line

dev.off()

minwater<-min(chile$WaterAS6yr2,na.rm=T)
maxwater<-max(chile$WaterAS6yr2,na.rm=T)

pdf(file = 'plot_pdfs/Chile_water_vs_inorganic_all_6yr.pdf', width = 5, height = 5)

ggplot(chile, aes(x=WaterAS6yr2, y=UrinaryInArsenic)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(minwater, maxwater), breaks = seq(minwater, maxwater,length=5),
                     labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), breaks = seq(minurine, maxurine,length=5),
                     labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="Chile, all subjects\ninorganic arsenic", x ="Water arsenic (µg/L, 6 year average)", y = "Urinary inorganic arsenic (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line
dev.off()

###Plotting against total inorganic arsenic, no outliers
minwater<-min(chile.sub$WaterAS1yr2,na.rm=T)
maxwater<-max(chile.sub$WaterAS1yr2,na.rm=T)
minurine<-min(chile.sub$UrinaryInArsenic,na.rm=T)
maxurine<-max(chile.sub$UrinaryInArsenic,na.rm=T)

pdf(file = 'plot_pdfs/Chile_water_vs_inorganic_restricted.pdf', width = 5, height = 5)

ggplot(chile.sub, aes(x=WaterAS1yr2, y=UrinaryInArsenic)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(minwater, maxwater), breaks = seq(minwater, maxwater,length=5),
                     labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), breaks = seq(minurine, maxurine,length=5),
                     labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="Chile, dropping 11 outliers/ninorganic arsenic", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary inorganic arsenic (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line

dev.off()

minwater<-min(chile.sub$WaterAS6yr2,na.rm=T)
maxwater<-max(chile.sub$WaterAS6yr2,na.rm=T)

pdf(file = 'plot_pdfs/Chile_water_vs_inorganic_restricted_6yr.pdf', width = 5, height = 5)

ggplot(chile.sub, aes(x=WaterAS6yr2, y=UrinaryInArsenic)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(minwater, maxwater), breaks = seq(minwater, maxwater,length=5),
                     labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), breaks = seq(minurine, maxurine,length=5),
                     labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="Chile, dropping 11 outliers\ninorganic arsenic", x ="Water arsenic (µg/L, 6 year average)", y = "Urinary inorganic arsenic (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line
dev.off() 


#combining regular and recalibrated in one graph. All data

minwater<-min(chile$WaterAS1yr2,na.rm=T)
maxwater<-max(chile$WaterAS1yr2,na.rm=T)
minurine<-min(c(chile$UrinaryArsenic,chile$total.adj),na.rm=T)
maxurine<-max(c(chile$UrinaryArsenic,chile$total.adj),na.rm=T)

### Plot
pdf(file = 'plot_pdfs/Chile_water_vs_tot&recal_all.pdf', width = 5, height = 5)

ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1") + geom_point(aes(y = total.adj), col = 'darkorange',alpha = 0.3) +
  geom_smooth(method="lm",color="darkblue")+ geom_smooth(method="lm",color="darkorange",aes(y=total.adj)) + 
  scale_x_continuous(limits = c(minwater, maxwater), breaks = seq(minwater, maxwater,length=5),
                     labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), breaks = seq(minurine, maxurine,length=5),
                     labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="Chile, all subjects\ntotal arsenic (blue) & \nrecalibrated for arsenobetaine (orange)", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic, recalibrated for arsenobetaine (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line

dev.off()

#combining regular and recalibrated in one graph. no outliers

minwater<-min(chile.sub$WaterAS1yr2,na.rm=T)
maxwater<-max(chile.sub$WaterAS1yr2,na.rm=T)
minurine<-min(c(chile.sub$UrinaryArsenic,chile.sub$total.adj),na.rm=T)
maxurine<-max(c(chile.sub$UrinaryArsenic,chile.sub$total.adj),na.rm=T)

### Plot
pdf(file = 'plot_pdfs/Chile_water_vs_tot&recal_restricted.pdf', width = 5, height = 5)

ggplot(chile.sub, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1") + geom_point(aes(y = total.adj), col = 'darkorange',alpha = 0.3) +
  geom_smooth(method="lm",color="darkblue")+ geom_smooth(method="lm",color="darkorange",aes(y=total.adj)) + 
  scale_x_continuous(limits = c(minwater, maxwater), breaks = seq(minwater, maxwater,length=5),
                     labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), breaks = seq(minurine, maxurine,length=5),
                     labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="Chile, dropping 11 outliers\ntotal arsenic (blue) & \nrecalibrated for arsenobetaine (orange)", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic, recalibrated for arsenobetaine (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line

dev.off()


### plot with HEALS scale

pdf(file = 'plot_pdfs/Chile_water_vs_tot&recal_all_healsscale.pdf', width = 5, height = 5)

ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1") + geom_point(aes(y = total.adj), col = 'darkorange',alpha = 0.3) +
  geom_smooth(method="lm",color="darkblue",fullrange = T)+ 
  geom_smooth(method="lm",color="darkorange",aes(y=total.adj),fullrange = T) + 
  scale_x_continuous(limits = c(minwater, 900), breaks = seq(minwater, 900,length=5),
                     labels=round(seq(minwater,900,length=5)))+
  scale_y_continuous(limits = c(minurine, 2000), breaks = seq(minurine, 2000,length=5),
                     labels=round(seq(minurine,2000,length=5)))+
  labs(title="Chile, all subjects\ntotal arsenic (blue) & \nrecalibrated for arsenobetaine (orange)", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic, recalibrated for arsenobetaine (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line

dev.off()


#without outliers

pdf(file = 'plot_pdfs/Chile_water_vs_tot&recal_restricted_healsscale.pdf', width = 5, height = 5)

ggplot(chile.sub, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1") + geom_point(aes(y = total.adj), col = 'darkorange',alpha = 0.3) +
  geom_smooth(method="lm",color="darkblue",fullrange = T)+ 
  geom_smooth(method="lm",color="darkorange",aes(y=total.adj),fullrange = T) + 
  scale_x_continuous(limits = c(minwater, 900), breaks = seq(minwater, 900,length=5),
                     labels=round(seq(minwater,900,length=5)))+
  scale_y_continuous(limits = c(minurine, 2000), breaks = seq(minurine, 2000,length=5),
                     labels=round(seq(minurine,2000,length=5)))+
  labs(title="Chile, 11 outliers removed\ntotal arsenic (blue) & \nrecalibrated for arsenobetaine (orange)", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic, recalibrated for arsenobetaine (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line

dev.off()



############
ggplot(chile, aes(x=WaterAS1yr2, y=UrinaryArsenic)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(minwater, maxwater), expand = c(0,0),
                     breaks = seq(minwater, maxwater,length=5),labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), expand = c(0,0),
                     breaks = seq(minurine, maxurine,length=5),labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="Chile, all subjects", x ="Water arsenic (µg/L, year of enrollment)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 12)+ facet_wrap(~Race) + 
  geom_abline(color="red") # adds a "one-to-one" line



##########Binned plots

chile$WaterAS1yr.bin = .bincode(chile$WaterAS1yr2, breaks = quantile(chile$WaterAS1yr2,(0:10)/10),include.lowest = T)

chilebins = chile %>% group_by(WaterAS1yr.bin) %>% dplyr::summarize(mean.wat = mean(WaterAS1yr2),
                                                             mean.ur = mean(UrinaryArsenic), 
                                                             lwr = quantile(UrinaryArsenic,.025),
                                                             uppr = quantile(UrinaryArsenic,.975),
                                                             mean.adj = mean(total.adj), 
                                                             lwr.adj = quantile(total.adj,.025),
                                                             uppr.adj = quantile(total.adj,.975))

ggplot(chilebins, aes(x = mean.wat, y = mean.ur, ymax = uppr, ymin = lwr)) + 
  geom_errorbar(col = 'darkblue') + geom_point(col = 'darkblue') + 
  stat_smooth(method = 'lm',se = F, col = rgb(0,0,.4,.9),fullrange = T) + 
  geom_errorbar(aes(x = mean.wat + 0.5, ymin = lwr.adj, ymax = uppr.adj),col = 'darkorange') + 
  geom_point(aes(x = mean.wat + 0.5, y=mean.adj),col = 'darkorange',fullrange = T) + 
  stat_smooth(aes(x = mean.wat + 0.5, y = mean.adj), method = 'lm',se = F, col = rgb(.9,.6,0,.9)) + 
  xlim(0,500) + ylim(0,400) + labs(title = 'Chile, binned', x = 'Water arsenic (µg/L, year of enrollment)',
                                  y = 'Urinary Arsenic \n(µg/L; blue - total; orange - adjusted for AsB)')


chile$WaterAS1yr.manbin = .bincode(chile$WaterAS1yr2, breaks = c(0,20,37,42,70),include.lowest = T)

chilemanbins = chile %>% group_by(WaterAS1yr.manbin) %>% dplyr::summarize(mean.wat = mean(WaterAS1yr2),
                                                                    mean.ur = mean(UrinaryArsenic), 
                                                                    lwr = quantile(UrinaryArsenic,.025),
                                                                    uppr = quantile(UrinaryArsenic,.975),
                                                                    mean.adj = mean(total.adj), 
                                                                    lwr.adj = quantile(total.adj,.025),
                                                                    uppr.adj = quantile(total.adj,.975))

ggplot(chilemanbins, aes(x = mean.wat, y = mean.ur, ymax = uppr, ymin = lwr)) + 
  geom_errorbar(col = 'darkblue') + geom_point(col = 'darkblue') + 
  stat_smooth(method = 'lm',se = F, col = rgb(0,0,.4,.9),fullrange = T) + 
  geom_errorbar(aes(x = mean.wat + 0.5, ymin = lwr.adj, ymax = uppr.adj),col = 'darkorange') + 
  geom_point(aes(x = mean.wat + 0.5, y=mean.adj),col = 'darkorange') + 
  stat_smooth(aes(x = mean.wat + 0.5, y = mean.adj), method = 'lm',se = F, col = rgb(.9,.6,0,.9),
              fullrange = T) + 
  xlim(0,70) + ylim(0,270) + labs(title = 'Chile, binned', x = 'Water arsenic (µg/L, year of enrollment)',
                                   y = 'Urinary Arsenic \n(µg/L; blue - total; orange - adjusted for AsB)')


ggplot(chilemanbins, aes(x = mean.wat, y = mean.ur, ymax = uppr, ymin = lwr)) + 
  geom_errorbar(col = 'darkblue') + geom_point(col = 'darkblue') + 
  stat_smooth(method = 'lm',se = F, col = rgb(0,0,.4,.9),fullrange = T) + 
  geom_errorbar(aes(x = mean.wat + 0.5, ymin = lwr.adj, ymax = uppr.adj),col = 'darkorange') + 
  geom_point(aes(x = mean.wat + 0.5, y=mean.adj),col = 'darkorange') + 
  stat_smooth(aes(x = mean.wat + 0.5, y = mean.adj), method = 'lm',se = F, col = rgb(.9,.6,0,.9),
              fullrange = T) + 
  xlim(0,500) + ylim(0,400) + labs(title = 'Chile, binned', x = 'Water arsenic (µg/L, year of enrollment)',
                                   y = 'Urinary Arsenic \n(µg/L; blue - total; orange - adjusted for AsB)')

#clustering bins
chile$WaterAS1yr.clustbin = kmeans(chile$WaterAS1yr2,centers = 4,nstart = 1000)$cluster
chile$WaterAS1yr.clustbin = factor(as.numeric(reorder(chile$WaterAS1yr.clustbin,
                                               chile$WaterAS1yr2)))
#get variances of cluster means for weighting
chileclustbins = chile %>% group_by(WaterAS1yr.clustbin) %>% dplyr::summarize(mean.wat = mean(WaterAS1yr2),
                                                                          mean.ur = mean(UrinaryArsenic), 
                                                                          lwr = quantile(UrinaryArsenic,.025),
                                                                          uppr = quantile(UrinaryArsenic,.975),
                                                                          var = var(UrinaryArsenic)/n(),
                                                                          mean.adj = mean(total.adj), 
                                                                          lwr.adj = quantile(total.adj,.025),
                                                                          uppr.adj = quantile(total.adj,.975),
                                                                          var.adj = var(total.adj)/n())


#inverse weight by standard error, add marginal densities with cluster colors

library(egg)

p1 = ggplot(chileclustbins, aes(x = mean.wat, y = mean.ur, ymax = uppr, ymin = lwr)) + 
  geom_errorbar(col = 'darkblue') + 
  geom_point(col = 'darkblue') + 
  stat_smooth(method = 'lm',
              aes(weight = 1/var),
              se = F, col = rgb(0,0,.4,.9),fullrange = T) +
  geom_errorbar(aes(x = mean.wat+.5, ymax = uppr.adj, ymin = lwr.adj),
                col = 'darkorange') + 
  geom_point(aes(x = mean.wat+.5, y = mean.adj),
                col = 'darkorange') + 
  stat_smooth(aes(x = mean.wat + .5, y = mean.adj, weight = 1/var.adj), 
              method = 'lm',se = F, col = rgb(.9,.6,0,.9),
              fullrange = T) + 
  xlim(0,70) + ylim(0,270) + labs(title = 'Chile, binned', x = 'Water arsenic (µg/L, year of enrollment)',
                                  y = 'Urinary Arsenic \n(µg/L; blue - total; orange - adjusted for AsB)')

marghist1 = ggplot(chile, aes(x = WaterAS1yr2, 
                              col = WaterAS1yr.clustbin,
                              fill = WaterAS1yr.clustbin,
                              group = WaterAS1yr.clustbin)) + 
  geom_histogram(bins = 200, pos = 'identity',col = rgb(0,0,0,0)) + 
  theme(axis.title = element_blank(),panel.grid = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(linetype = 1),
        axis.ticks = element_blank()) + 
  scale_color_discrete(guide = F) + scale_fill_discrete(guide = F) + 
  scale_x_continuous(limits = c(0,70)) + 
  scale_y_continuous(expand = c(0,0))

egg::ggarrange(plots = list(p1, marghist1),nrow = 2,heights = c(8,1))

