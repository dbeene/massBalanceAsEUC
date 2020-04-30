library(ggplot2) 
library(openxlsx)
library(lubridate)
## read in data file 
ardata<-read.csv("Chile_Arsenic_Data_Redux.csv")

#subset to year of enrollment
ardata$yearin = year(ardata$DateEnr)
ardata = subset(ardata, yearin == Exp_Year)

#Following UNM, remove outliers above 100 ug/L for each concentration
ardata.nooutliers = subset(ardata, UrinaryArsenic <= 100)

##plot without outliers
pdf('plot_pdfs/Chile_total_vs_water_no_outliers.pdf',width = 5,height = 5)
ggplot(ardata.nooutliers, aes(x=Exp_WaterAS, y=UrinaryArsenic)) + 
  geom_point(size=1, col='red')+
  labs(x = expression(Municipal~Water~Arsenic~(mu*g/L)), y = expression(Total~Urinary~Arsenic~(mu*g/L))) +
  geom_smooth(method=lm, se=FALSE, color="blue")+
  ggtitle("Chile Case-Control Study") + geom_abline(lty=2) 
dev.off()

pdf('plot_pdfs/Chile_inorganic_vs_water_no_outliers.pdf',width = 5,height = 5)
ggplot(ardata.nooutliers, aes(x=Exp_WaterAS, y=UrinaryInArsenic)) + 
  geom_point(size=1, col='red')+
  labs(x = expression(Water~Arsenic~(mu*g/L)), y = expression(Total~Urinary~Inorganic~Arsenic~(mu*g/L))) +
  geom_smooth(method=lm, se=FALSE, color="blue")+
  ggtitle("Chile Case-Control Study") + geom_abline(lty=2)
dev.off()

pdf('plot_pdfs/Chile_total_less_asB_vs_water_no_outliers.pdf',width = 5,height = 5)
ggplot(ardata.nooutliers, aes(x=Exp_WaterAS, y=UrinaryArsenic - UrinaryAsBet)) + 
  geom_point(size=1, col='red')+
  labs(x = expression(Water~Arsenic~(mu*g/L)), y = expression(Total~Urinary~Arsenic~Less~Arsenobetaine~(mu*g/L))) +
  geom_smooth(method=lm, se=FALSE, color="blue")+
  ggtitle("Chile Case-Control Study") + geom_abline(lty=2)
dev.off()

##plot Urine DMA vs water arsenic with the regression line
pdf('plot_pdfs/Chile_DMA_vs_water_no_outliers.pdf',width = 5,height = 5)
ggplot(ardata.nooutliers, aes(x=Exp_WaterAS, y=UrinaryDiMeAr)) + 
  geom_point(size=1, col='red')+
  labs(x = expression(Water~Arsenic~(mu*g/L)), y = expression(Urinary~Dimethyl~Arsenate~(mu*g/L))) +
  geom_smooth(method=lm, se=FALSE, color="blue")+
  ggtitle("Chile Case-Control Study") + geom_abline(lty=2)
dev.off()


##plot with outliers
pdf('plot_pdfs/Chile_total_vs_water_all_data.pdf',width = 5,height = 5)
ggplot(ardata, aes(x=Exp_WaterAS, y=UrinaryArsenic)) + 
  geom_point(size=1, col='red')+
  labs(x = expression(Water~Arsenic~(mu*g/L)), y = expression(Total~Urinary~Arsenic~(mu*g/L))) +
  geom_smooth(method=lm, se=FALSE, color="blue")+
  ggtitle("Chile Case-Control Study") + geom_abline(lty=2)
dev.off()

pdf('plot_pdfs/Chile_inorganic_vs_water_all_data.pdf',width = 5,height = 5)
ggplot(ardata, aes(x=Exp_WaterAS, y=UrinaryInArsenic)) + 
  geom_point(size=1, col='red')+
  labs(x = expression(Water~Arsenic~(mu*g/L)), y = expression(Total~Urinary~Inorganic~Arsenic~(mu*g/L))) +
  geom_smooth(method=lm, se=FALSE, color="blue")+
  ggtitle("Chile Case-Control Study") + geom_abline(lty=2)
dev.off()

pdf('plot_pdfs/Chile_total_less_asB_vs_water_all_data.pdf',width = 5,height = 5)
ggplot(ardata, aes(x=Exp_WaterAS, y=UrinaryArsenic - UrinaryAsBet)) + 
  geom_point(size=1, col='red')+
  labs(x = expression(Water~Arsenic~(mu*g/L)), y = expression(Total~Urinary~Arsenic~Less~Arsenobetaine~(mu*g/L))) +
  geom_smooth(method=lm, se=FALSE, color="blue")+
  ggtitle("Chile Case-Control Study") + geom_abline(lty=2)
dev.off()

##plot Urine DMA vs water arsenic with the regression line
pdf('plot_pdfs/Chile_DMA_vs_water_all_data.pdf',width = 5,height = 5)
ggplot(ardata, aes(x=Exp_WaterAS, y=UrinaryDiMeAr)) + 
  geom_point(size=1, col='red')+
  labs(x = expression(Water~Arsenic~(mu*g/L)), y = expression(Urinary~Dimethyl~Arsenate~(mu*g/L))) +
  geom_smooth(method=lm, se=FALSE, color="blue")+
  ggtitle("Chile Case-Control Study") + geom_abline(lty=2)
dev.off()