##### Mass balance scatterplots
##### 27 April 2020

#### Load libraries
library(openxlsx)
library(Hmisc)
library(ggplot2)

#### Load data
heals<-read.xlsx("~/Desktop/Desktop March 2020/healsfixed.xlsx",detectDates=TRUE)

#### Create new var (WaterArsenic2) to replace 0 values with 0.1
heals$WaterArsenic2<-ifelse(heals$WaterArsenic==0,0.1,heals$WaterArsenic)
heals$WaterArsenic2<-as.numeric(heals$WaterArsenic2)

## Plot 1: Full scale, all participants
minwater<-min(heals$WaterArsenic2,na.rm=T)
maxwater<-max(heals$WaterArsenic2,na.rm=T)
minurine<-min(heals$UrineArsenic,na.rm=T)
maxurine<-max(heals$UrineArsenic,na.rm=T)

ggplot(heals, aes(x=WaterArsenic2, y=UrineArsenic)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(minwater, maxwater), breaks = seq(minwater, maxwater,length=5),labels=round(seq(minwater,maxwater,length=5)))+
  scale_y_continuous(limits = c(minurine, maxurine), breaks = seq(minurine, maxurine,length=5),labels=round(seq(minurine,maxurine,length=5)))+
  labs(title="HEALS full cohort", x ="Water arsenic (µg/L)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line

## Plot 2: Optimized range
subset<-heals[which(heals$WaterArsenic2<400),]
subsetminwater<-min(subset$WaterArsenic2,na.rm=T)
subsetmaxwater<-max(subset$WaterArsenic2,na.rm=T)
subsetminurine<-min(subset$UrineArsenic,na.rm=T)
subsetmaxurine<-max(subset$UrineArsenic,na.rm=T)

ggplot(subset, aes(x=WaterArsenic2, y=UrineArsenic)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(subsetminwater, subsetmaxwater), breaks = seq(subsetminwater, subsetmaxwater,length=5),labels=round(seq(subsetminwater,subsetmaxwater,length=5)))+
  scale_y_continuous(limits = c(subsetminurine, subsetmaxurine), breaks = seq(subsetminurine, subsetmaxurine,length=5),labels=round(seq(subsetminurine,subsetmaxurine,length=5)))+
  labs(title="HEALS subset, water arsenic <400 µg/L", x ="Water arsenic (µg/L)", y = "Urinary arsenic (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line



### Plot 3: Recalibrating for arsenobetaine

## Recalibrate total urinary arsenic by arsenobetaine levels- see PMCID: PMC5065621 for details
# Recalibrated values = residuals + conditional mean 

fit <- glm(log(UrineArsenic)~log(arsenobetaine),
           family=gaussian(link="identity"), data=heals)
b <- summary(fit)$coeff[1,1]  						# This is the conditional mean from the model for individuals with 0 log ab_cations (intercept B0, condition mean when arsenobetaine=1)
heals$ln.total <- b             							# Create a variable in the database with the value of the same baseline conditional mean for everybody- this gives everybody the urinary total arsenic value that corresponds to the conditional mean
residuals.total <- residuals(fit,type="response") 			# Save the residuals- this is the part that doesn't depend on AB and also gives you the variability between people
heals.lowab <- subset(heals, heals$arsenobetaine < 1)
fit.b0 <- glm(log(UrineArsenic)~offset(ln.total),    #offset = you are not obtaining a coefficient that is multiplying this variable (b1)- so ln.total is the same value for everyone (b)
              # offset tells you that "this variable is not a real coefficinet- it is just a stand in value- so instead give me another coefficient (alpha0) to calibrate
              family=gaussian(link="identity"), data=heals.lowab) 		 	# Intercept from this model is the estimated constant factor to recalibrate the baseline conditional mean into the adjusted marginal mean
heals$ln.total.adj <- summary(fit.b0)$coeff[1,1]+b+residuals.total	# Add the conditional mean, the calibration factor (marginal mean of the log-transformed total arsenic) and the residuals

# All the three together are the log-transformed arsenobetaine-adjusted individual total arsenic levels)
# alpha is the first thing (fit.b0$coeff(1,1), b is the conditional mean, and the residuals are the most important part

describe(heals$ln.total.adj)
heals$total.adj <- exp(heals$ln.total.adj)
describe(heals$total.adj) # New var of total arsenic recalibrated for urinary arsenobetaine

recalibratedminwater<-min(heals$WaterArsenic2,na.rm=T)
recalibratedmaxwater<-max(heals$WaterArsenic2,na.rm=T)
recalibratedminurine<-min(heals$total.adj,na.rm=T)
recalibratedmaxurine<-max(heals$total.adj,na.rm=T)

### Plot
ggplot(heals, aes(x=WaterArsenic2, y=total.adj)) +
  geom_point(color="#80cdc1") + 
  geom_smooth(method="lm",color="darkblue")+
  scale_x_continuous(limits = c(recalibratedminwater, recalibratedmaxwater), breaks = seq(recalibratedminwater, recalibratedmaxwater,length=5),
                     labels=round(seq(recalibratedminwater,recalibratedmaxwater,length=5)))+
  scale_y_continuous(limits = c(recalibratedminurine, recalibratedmaxurine), breaks = seq(recalibratedminurine, recalibratedmaxurine,length=5),
                     labels=round(seq(recalibratedminurine,recalibratedmaxurine,length=5)))+
  labs(title="HEALS full cohort, recalibrated for arsenobetaine", x ="Water arsenic (µg/L)", y = "Urinary arsenic, recalibrated for arsenobetaine (µg/L)")+
  theme_grey(base_size = 12)+
  geom_abline(intercept = 0, slope=1,color="red") # adds a "one-to-one" line




