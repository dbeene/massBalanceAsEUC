library(tidyverse)
library(lubridate)

##data loading and variable manipulations
ar.dat = readxl::read_xlsx('..\\..\\..\\FOR_ANDRES_012920.xlsx')
chile<-read.csv("..\\..\\..\\Chile_Arsenic_Data_Redux.csv")
chile = chile %>% mutate(Cancer = !is.na(Other_cancer) | Cancer_type != 'control')

chile = chile %>% group_by(SubjectID) %>% mutate(yrin = year(DateEnr)) %>% 
  mutate(WaterAS6yr = mean(Exp_WaterAS[(yrin - Exp_Year) %in% (0:5)]),
         WaterAS1yr = Exp_WaterAS[yrin == Exp_Year])

chile = chile %>% filter(!duplicated(SubjectID))

#Create new var (WaterArsenic2) to replace 0 values with 0.1
chile$WaterAS1yr2<-ifelse(chile$WaterAS1yr==0,0.1,chile$WaterAS1yr)
chile$WaterAS6yr2<-ifelse(chile$WaterAS6yr==0,0.1,chile$WaterAS6yr)

chile = subset(chile,!is.na(UrinaryArsenic))

chile$total_minus_AsB = chile$UrinaryArsenic - chile$UrinaryAsBet

chile$total_minus_AsB[chile$total_minus_AsB < 0] = 0.1

chile$DateEnr = as.Date(chile$DateEnr)

chile$allmuni = c('All municipal water','Not all municipal water')[2-(chile$prop.muni==1)]

chile$water_intake = ar.dat$TKHL[match(chile$SubjectID,ar.dat$commonID)]

propdat = readxl::read_xlsx('..\\..\\..\\WATER_INFO_060520.xlsx')
chile$prop_muni = propdat$percent_muni[match(chile$SubjectID,propdat$commonID)]/100

##get mean water intake
mean_water_intake = mean(chile$water_intake)

##get conditional mean urine volume
ur.dat = read.csv('TFI_vs_urine.csv')
ur.lm = lm(Urine_24 ~ TFI, ur.dat)

mean_urine_volume = coef(ur.lm)[1] + coef(ur.lm)[2] * mean_water_intake

#function to print lm summary on plot
lm_eqn <- function(m){
  eq <- substitute(italic(y) == a + b~italic(x)*","~~italic(r)^2~"="~r2~","~~italic(p)~"="~pval, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3),
                        pval = format(summary(m)$coefficients[2,4], digits = 3)))
  as.character(as.expression(eq));
}


ave.lm = lm(I(mean_urine_volume * UrinaryArsenic) ~ I(mean_water_intake * WaterAS1yr2), data = chile)
ind.lm = lm(I(coef(ur.lm)[1] + coef(ur.lm)[2] * water_intake * UrinaryArsenic) ~ 
              I(water_intake * WaterAS1yr2), data = chile)

#scatterplot using overall mean water intake
png(file = 'Chile_aggregate_AS_mass_Water_vs_Urine.png',width = 360, height = 360)

ggplot(chile, aes(x = mean_water_intake * WaterAS1yr2, y = mean_urine_volume * UrinaryArsenic)) + 
  geom_point() + geom_smooth(method = 'lm') + labs(x = 'Mass Arsenic Ingested in Water (µg / day)',
                                                   y = 'Mass Arsenic Excreted in Urine (µg / day)',
                                                   title = 'Chile: Average Water Intake') + 
  annotate('text',x = 70, y = 1100, label = lm_eqn(ave.lm), parse = TRUE,size = 3) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

dev.off()
#scatterplot using reported water intake
png(file = 'Chile_individual_AS_mass_Water_vs_Urine.png',width = 360, height = 360)

ggplot(chile, aes(x = water_intake * WaterAS1yr2, 
                  y = coef(ur.lm)[1] + coef(ur.lm)[2] * water_intake * UrinaryArsenic)) + 
  geom_point() + geom_smooth(method = 'lm') + labs(x = 'Mass Arsenic Ingested in Water (µg / day)',
                                                   y = 'Mass Arsenic Excreted in Urine (µg / day)',
                                                   title = 'Chile: Individual Water Intake') + 
  annotate('text',x = 320, y = 1100, label = lm_eqn(ind.lm), parse = TRUE,size = 3) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

dev.off()