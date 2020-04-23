library(ggplot2) 
library(openxlsx) 

##ggplot data 04222020

## using UNM MMETALS scale (EPA Water data)
## read in data file 
METALs<-read.xlsx("Navajo0422.xlsx",detectDates=TRUE)
METALs$UTAS2=ifelse(METALs$`UTAS.(ug/L)`>100,NA, METALs$`UTAS.(ug/L)`)
METALs$Gest_Wk2=as.numeric(METALs$Gest_Wk)
METALs$UTAS_del2=as.numeric(METALs$UTAS_del)
METALs$UAS3=as.numeric(METALs$`UAS3.(ug/L)`)
METALs$UAS5=as.numeric(METALs$`UAS5.(ug/L)`)
METALs$UDMA=as.numeric(METALs$`UDMA.(ug/L)`)
METALs$UMMA=as.numeric(METALs$`UMMA.(ug/L)`)

## remove outliers
METALs$UDMA2=ifelse(METALs$`UTAS.(ug/L)`>100,NA, METALs$UDMA)
METALs$UMMA2=ifelse(METALs$`UTAS.(ug/L)`>100,NA, METALs$UMMA)


fit1 <- lm(METALs$UTAS2 ~ METALs$waterAs6y+METALs$Gest_Wk2, family=gaussian(link="identity"), data=METALs)
fit1 <- lm(METALs$UTAS_del2 ~ METALs$waterAs6y+METALs$Gest_Wk2, family=gaussian(link="identity"), data=METALs)
fit1 <- lm(METALs$UAS3 ~ METALs$waterAs6y, family=gaussian(link="identity"), data=METALs)
fit1 <- lm(METALs$UAS5 ~ METALs$waterAs6y, family=gaussian(link="identity"), data=METALs)
fit1 <- lm(METALs$UDMA2 ~ METALs$waterAs6y, family=gaussian(link="identity"), data=METALs)
fit1 <- lm(METALs$UMMA2 ~ METALs$waterAs6y, family=gaussian(link="identity"), data=METALs)




##plot Urine UTAS vs water arsenic with the regression line
ggplot(METALs, aes(x=METALs$waterAs6y, y=METALs$UTAS2)) + 
  geom_point(size=1, col='red')+
  labs(x ="Water Arsenic (�g/L)", y = "Urinary Arsenic (�g/L)")+
  geom_smooth(method=lm, se=FALSE, color="blue")+
  ggtitle("UNM METALS Navajo Birth Cohort")

####plot Urine UTAS vs water arsenic with the regression line of Urine UTAS vs water arsenic controlling for ges age
ggplot(METALs, aes(x=METALs$waterAs6y, y=METALs$UTAS2)) + 
  geom_point(size=1, col='red')+
  labs(x ="Water Arsenic (�g/L)", y = "Urinary Arsenic (�g/L)")+
  geom_smooth(method="lm", formula = y ~ x, se = FALSE, color="blue")+
 ##add the line after adjuting for ges age: geom_abline(intercept=6.23955,slope=0.37789, color="green")+ 
  ggtitle("UNM METALS Navajo Birth Cohort")


##plot Urine UTAS_Del vs water arsenic with the regression line (not significant)
ggplot(METALs, aes(x=METALs$waterAs6y, y=METALs$UTAS_del2)) + 
  geom_point(size=1, col='red')+
  labs(x ="Water Arsenic (�g/L)", y = "Urinary Arsenic (�g/L)")+
  geom_smooth(method=lm, se=FALSE, color="blue")+
  ggtitle("UNM METALS Navajo Birth Cohort")



##plot Urine DMA vs water arsenic with the regression line
ggplot(METALs, aes(x=METALs$waterAs6y, y=METALs$UDMA2)) + 
  geom_point(size=1, col='red')+
  labs(x ="Water Arsenic (�g/L)", y = "Urinary Arsenic DMA(�g/L)")+
  geom_smooth(method=lm, se=FALSE, color="blue")+
  ggtitle("UNM METALS Navajo Birth Cohort")



































## Using scale of Heals
fit1 <- lm(METALs$`UTAS.(ug/L)` ~ METALs$waterAs6y, family=gaussian(link="identity"), data=METALs)


ggplot(METALs, aes(x=METALs$waterAs6y, y=METALs$`UTAS.(ug/L)`)) + 
  geom_point(size=1, col='red')+
  xlim(0, 500)+
  ylim(0,500)+
  labs(x ="Water Arsenic (�g/L)", y = "Urinary Arsenic (�g/L)")+
  geom_abline(intercept=6.5511,slope=0.3483, color="blue")+
  ggtitle("UNM METALS Navajo Birth Cohort (based on HEALS scale)")