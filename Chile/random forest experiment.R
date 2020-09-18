#install.packages('randomForest')
#install.packages('forestFloor')
library(Hmisc)
library(ggplot2)
library(dplyr)
library(lubridate)
#library(readxl)
#### Load data
chile<-read.csv("Chile_Arsenic_Data_Redux.csv")

  # waterprops = read_xlsx('WATER_INFO_060520.xlsx')
  # chile$prop.muni = waterprops$percent_muni[match(chile$SubjectID,waterprops$commonID)]/100
  # chile$prop.bottled = waterprops$percent_bottled[match(chile$SubjectID,waterprops$commonID)]/100
  # chile$prop.other = waterprops$percent_other[match(chile$SubjectID,waterprops$commonID)]/100
  # write.csv(chile, file = 'Chile_Arsenic_Data_Redux.csv',row.names = F)
# #Drinking water Arsenic concentration was based on the 6-year average of arsenic in public water 
#supply averaged from EPA’s Six Year Review of Contaminant Occurrence database. 

chile = chile %>% group_by(SubjectID) %>% mutate(yrin = year(DateEnr)) %>% 
  mutate(WaterAS6yr = mean(Exp_WaterAS[(yrin - Exp_Year) %in% (0:5)]),
         WaterAS1yr = Exp_WaterAS[yrin == Exp_Year])

chile = chile %>% filter(!duplicated(SubjectID))

#### Create new var (WaterArsenic2) to replace 0 values with 0.1
chile$WaterAS1yr2<-ifelse(chile$WaterAS1yr==0,0.1,chile$WaterAS1yr)
chile$WaterAS6yr2<-ifelse(chile$WaterAS6yr==0,0.1,chile$WaterAS6yr)

chile = subset(chile,!is.na(UrinaryArsenic))

chile$total_minus_AsB = chile$UrinaryArsenic - chile$UrinaryAsBet

chile$total_minus_AsB[chile$total_minus_AsB < 0] = 0.1

chile$DateEnr = as.Date(chile$DateEnr)

hist(chile$prop.muni,breaks = 100, xlab = 'Prop. of water from municipal source')

lm.all = glm(UrinaryArsenic ~ WaterAS1yr2, chile, family = Gamma(link = 'identity'))
lm.all.muni = glm(UrinaryArsenic ~ WaterAS1yr2, chile, subset = prop.muni==1, family = Gamma(link = 'identity'))
lm.all.other = glm(UrinaryArsenic ~ WaterAS1yr2, chile, subset = prop.muni<1, family = Gamma(link = 'identity'))

lm2.all = glm(total_minus_AsB ~ WaterAS1yr2, chile, family = Gamma(link = 'identity'))
lm2.all.muni = glm(total_minus_AsB ~ WaterAS1yr2, chile, subset = prop.muni==1, family = Gamma(link = 'identity'))
lm2.all.other = glm(total_minus_AsB ~ WaterAS1yr2, chile, subset = prop.muni<1, family = Gamma(link = 'identity'),
                    start = lm2.all$coefficients)


chile$allmuni = c('All municipal water','Not all municipal water')[2-(chile$prop.muni==1)]

ggplot(subset(chile,!is.na(prop.muni)), aes(x = WaterAS1yr2, y = UrinaryArsenic)) + geom_point() + stat_smooth(method = 'glm',
                                                                                                                                method.args = list(family = Gamma(link = 'identity'))) + 
  labs(title = 'All data together \nregression with gamma distributed errors') + geom_abline(col = 'red',lty = 2) + 
  theme_bw() + xlim(0, 60) + ylim(0, 700)


ggplot(subset(chile,!is.na(prop.muni)), aes(x = WaterAS1yr2, y = UrinaryArsenic, col = prop.muni)) + geom_point() + stat_smooth(method = 'glm',
                                                                                     method.args = list(family = Gamma(link = 'identity'))) + 
  labs(title = 'Data split by municipal water consumption \nregression with gamma distributed errors') + geom_abline(col = 'red',lty = 2) + 
  theme_bw() + xlim(0, 60) + ylim(0, 700) + facet_wrap(~allmuni)

ggplot(subset(chile,!is.na(prop.muni)), aes(x = WaterAS1yr2, y = UrinaryInArsenic)) + geom_point() + stat_smooth(method = 'glm',
                                                                                                               method.args = list(family = Gamma(link = 'identity'))) + 
  labs(title = 'All data together, Inorganic Arsenic \nregression with gamma distributed errors') + geom_abline(col = 'red',lty = 2) + 
  theme_bw() + xlim(0, 60) + ylim(0, 700)


ggplot(subset(chile,!is.na(prop.muni)), aes(x = WaterAS1yr2, y = UrinaryInArsenic, col = prop.muni)) + geom_point() + stat_smooth(method = 'glm',
                                                                                                                                method.args = list(family = Gamma(link = 'identity'))) + 
  labs(title = 'Inorganic arsenic, data split by municipal water consumption \nregression with gamma distributed errors') + geom_abline(col = 'red',lty = 2) + 
  theme_bw() + xlim(0, 60) + ylim(0, 700) + facet_wrap(~allmuni)


chile$ar.bins = kmeans(chile$WaterAS1yr2,4,nstart = 1000)$cluster
chile$ar.bins = order(tapply(chile$WaterAS1yr2,chile$ar.bins,mean))[match(chile$ar.bins, rank(tapply(chile$WaterAS1yr2,chile$ar.bins,mean)))]

for(b in 1:4){
  sub = subset(chile,ar.bins==b & !is.na(allmuni))
  binmin = min(sub$WaterAS1yr2)
  binmax = max(sub$WaterAS1yr2)
  ggplot(sub,aes(x = UrinaryArsenic,fill = allmuni)) + 
    geom_histogram(alpha = 0.4, position = 'identity') + 
    theme_bw() + labs(title = paste('Subjects with', binmin, 'to', binmax,'\nMicrograms / L Water Arsenic'))
  
  ks = ks.test(sub$UrinaryArsenic[sub$prop.muni==1], sub$UrinaryArsenic[sub$prop.muni<1])
  
  ks.dat = data.frame(UrinaryArsenic = seq(min(sub$UrinaryArsenic),max(sub$UrinaryArsenic),length.out = 100))
  ks.dat$muni = ecdf(sub$UrinaryArsenic[sub$prop.muni==1])( ks.dat$UrinaryArsenic)
  ks.dat$other = ecdf(sub$UrinaryArsenic[sub$prop.muni<1])( ks.dat$UrinaryArsenic)
  
  xloc = ks.dat$UrinaryArsenic[which.max(abs(ks.dat$muni-ks.dat$other))]
  y1 = ks.dat$muni[which.max(abs(ks.dat$muni-ks.dat$other))]
  y2 = ks.dat$other[which.max(abs(ks.dat$muni-ks.dat$other))]
  
  
  ggplot(ks.dat,aes(x = UrinaryArsenic, y = muni)) + geom_line(color = 'red') + geom_line(aes(y = other), col = 'blue') + 
    labs(y = 'Cumulative density function') + geom_segment(x = xloc, xend = xloc, y = y1, yend = y2, lty = 2) + 
    geom_text(x = 100, y = 0.25,label = paste('KS test p-value: \n',ks$p.value))
  
}

for(b in 1:4){
  sub = subset(chile,ar.bins==b & !is.na(allmuni))
  binmin = min(sub$WaterAS1yr2)
  binmax = max(sub$WaterAS1yr2)
  ggplot(sub,aes(x = UrinaryInArsenic,fill = allmuni)) + 
    geom_histogram(alpha = 0.4, position = 'identity') + 
    theme_bw() + labs(title = paste('Subjects with', binmin, 'to', binmax,'\nMicrograms / L Water Arsenic'))
  
  ks = ks.test(sub$UrinaryInArsenic[sub$prop.muni==1], sub$UrinaryInArsenic[sub$prop.muni<1])
  
  ks.dat = data.frame(UrinaryInArsenic = seq(min(sub$UrinaryInArsenic),max(sub$UrinaryInArsenic),length.out = 100))
  ks.dat$muni = ecdf(sub$UrinaryInArsenic[sub$prop.muni==1])( ks.dat$UrinaryInArsenic)
  ks.dat$other = ecdf(sub$UrinaryInArsenic[sub$prop.muni<1])( ks.dat$UrinaryInArsenic)
  
  xloc = ks.dat$UrinaryInArsenic[which.max(abs(ks.dat$muni-ks.dat$other))]
  y1 = ks.dat$muni[which.max(abs(ks.dat$muni-ks.dat$other))]
  y2 = ks.dat$other[which.max(abs(ks.dat$muni-ks.dat$other))]
  
  
  ggplot(ks.dat,aes(x = UrinaryInArsenic, y = muni)) + geom_line(color = 'red') + geom_line(aes(y = other), col = 'blue') + 
    labs(y = 'Cumulative density function') + geom_segment(x = xloc, xend = xloc, y = y1, yend = y2, lty = 2) + 
    geom_text(x = 100, y = 0.25,label = paste('KS test p-value: \n',ks$p.value))
  
}

library(randomForest)
library(iml)
#pre-processing
chile$AveCigs[chile$Smoker == 'Never'] = 0
chile$Cancer = !is.na(chile$Other_cancer) & chile$Cancer_type != 'control'

chile.rf = chile %>% ungroup %>% select_at(-c(1,8:12,22:27,29,48:51,53:54,56,58,82:88,92:94,96,99:100))

chile.rf = chile.rf[,apply(chile.rf,2,function(x) length(unique(na.omit(x))) > 1)]

chile.rf.UrAs = chile.rf %>% select_at(-c(8:15,63))
#chile.rf.UrAs$DateEnr = as.Date(chile.rf.UrAs$DateEnr)
chile.rf.InAs = chile.rf %>% select_at(-c(7,9:15,63))
chile.rf.As_mAsB = chile.rf %>% select_at(-c(7:15))

UrAs.subset = data.frame(model.matrix(~.,chile.rf.UrAs)[,-1]) #dropped 52 obs with missing predictors

rf.UrAs = randomForest(I(log(UrinaryArsenic)) ~ ., data = UrAs.subset,
                       keep.inbag = T)

Pred.1 = Predictor$new(rf.UrAs, data = UrAs.subset %>% select(-UrinaryArsenic),
                       y = log(UrAs.subset$UrinaryArsenic))

imp.1 = FeatureImp$new(Pred.1, loss = 'mse', compare = 'difference')

ggplot(imp.1$results[1:10,], aes(y = factor(feature,levels = rev(feature)), 
                                 x = importance, xmin = importance.05,xmax = importance.95)) + 
  geom_errorbarh(height = 0.2, size = 1, col = 'darkgrey') + geom_point() + theme_light() + 
  labs(title = 'Random Forest on Log(Urinary Arsenic)', y = 'Feature', 
       x = 'Increase in residual error after permutation') + 
  theme(axis.text.y = element_text(angle = 30,hjust = 0.8,size = 10))


UrAs.subset.muni = subset(UrAs.subset, prop.muni == 1, select = - prop.muni)

rf.UrAs.muni = randomForest(I(log(UrinaryArsenic)) ~ ., data = UrAs.subset.muni,
                       keep.inbag = T)

Pred.1m = Predictor$new(rf.UrAs.muni, data = UrAs.subset.muni %>% select(-UrinaryArsenic),
                       y = log(UrAs.subset.muni$UrinaryArsenic))

imp.1m = FeatureImp$new(Pred.1m, loss = 'mse', compare = 'difference')

ggplot(imp.1m$results[1:10,], aes(y = factor(feature,levels = rev(feature)), 
                                 x = importance, xmin = importance.05,xmax = importance.95)) + 
  geom_errorbarh(height = 0.2, size = 1, col = 'darkgrey') + geom_point() + theme_light() + 
  labs(title = 'Random Forest on Log(Urinary Arsenic)', y = 'Feature', 
       x = 'Increase in residual error after permutation') + 
  theme(axis.text.y = element_text(angle = 30,hjust = 0.8,size = 10))

UrAs.subset.other = subset(UrAs.subset, prop.muni < 1)

rf.UrAs.other = randomForest(I(log(UrinaryArsenic)) ~ ., data = UrAs.subset.other,
                            keep.inbag = T)

Pred.1o = Predictor$new(rf.UrAs.other, data = UrAs.subset.other %>% select(-UrinaryArsenic),
                        y = log(UrAs.subset.other$UrinaryArsenic))

imp.1o = FeatureImp$new(Pred.1o, loss = 'mse', compare = 'difference')

ggplot(imp.1o$results[1:10,], aes(y = factor(feature,levels = rev(feature)), 
                                  x = importance, xmin = importance.05,xmax = importance.95)) + 
  geom_errorbarh(height = 0.2, size = 1, col = 'darkgrey') + geom_point() + theme_light() + 
  labs(title = 'Random Forest on Log(Urinary Arsenic)', y = 'Feature', 
       x = 'Increase in residual error after permutation') + 
  theme(axis.text.y = element_text(angle = 30,hjust = 0.8,size = 10))



InAs.subset = data.frame(model.matrix(~.,chile.rf.InAs)[,-1]) #dropped 52 obs with missing predictors

rf.InAs = randomForest(I(log(UrinaryInArsenic)) ~ ., data = InAs.subset,
                       keep.inbag = T)

Pred.2 = Predictor$new(rf.InAs, data = InAs.subset %>% select(-UrinaryInArsenic),
                       y = log(InAs.subset$UrinaryInArsenic))

imp.2 = FeatureImp$new(Pred.2, loss = 'mse', compare = 'difference')

adj.fac = 28.3495 / 7 #factor to adjust oz per week to grams per day

ggplot(imp.2$results[1:10,], aes(y = factor(feature,levels = rev(feature)), 
                                 x = importance, xmin = importance.05,xmax = importance.95)) + 
  geom_errorbarh(height = 0.2, size = 1, col = 'darkgrey') + geom_point() + theme_light() + 
  labs(title = 'Random Forest on Log(Urinary Inorganic Arsenic)', y = 'Feature', 
       x = 'Increase in residual error after permutation') + 
  theme(axis.text.y = element_text(angle = 30,hjust = 0.8,size = 10))

InAs.subset.m = subset(InAs.subset, prop.muni==1, select = -prop.muni)

rf.InAs.m = randomForest(I(log(UrinaryInArsenic)) ~ ., data = InAs.subset.m,
                       keep.inbag = T)

Pred.2.m = Predictor$new(rf.InAs.m, data = InAs.subset.m %>% select(-UrinaryInArsenic),
                       y = log(InAs.subset.m$UrinaryInArsenic))

imp.2.m = FeatureImp$new(Pred.2.m, loss = 'mse', compare = 'difference')

ggplot(imp.2.m$results[1:10,], aes(y = factor(feature,levels = rev(feature)), 
                                 x = importance, xmin = importance.05,xmax = importance.95)) + 
  geom_errorbarh(height = 0.2, size = 1, col = 'darkgrey') + geom_point() + theme_light() + 
  labs(title = 'Random Forest on Log(Urinary Inorganic Arsenic)', y = 'Feature', 
       x = 'Increase in residual error after permutation') + 
  theme(axis.text.y = element_text(angle = 30,hjust = 0.8,size = 10))

InAs.subset.o = subset(InAs.subset, prop.muni<1)

rf.InAs.o = randomForest(I(log(UrinaryInArsenic)) ~ ., data = InAs.subset.o,
                         keep.inbag = T)

Pred.2.o = Predictor$new(rf.InAs.o, data = InAs.subset.o %>% select(-UrinaryInArsenic),
                         y = log(InAs.subset.o$UrinaryInArsenic))

imp.2.o = FeatureImp$new(Pred.2.o, loss = 'mse', compare = 'difference')

ggplot(imp.2.o$results[1:10,], aes(y = factor(feature,levels = rev(feature)), 
                                   x = importance, xmin = importance.05,xmax = importance.95)) + 
  geom_errorbarh(height = 0.2, size = 1, col = 'darkgrey') + geom_point() + theme_light() + 
  labs(title = 'Random Forest on Log(Urinary Inorganic Arsenic)', y = 'Feature', 
       x = 'Increase in residual error after permutation') + 
  theme(axis.text.y = element_text(angle = 30,hjust = 0.8,size = 10))





AsmAsB.subset = data.frame(model.matrix(~.,chile.rf.As_mAsB)[,-1]) #dropped 52 obs with missing predictors



rf.AsmAsB = randomForest(I(log(total_minus_AsB)) ~ ., data = AsmAsB.subset,
                       keep.inbag = T)


library(forestFloor)

fe.1 = forestFloor(rf.UrAs,Pred.1$data$X)

int.date.1 = Interaction$new(Pred.1, feature = 'DateEnr')
int.water.1 = Interaction$new(Pred.1, feature = 'WaterAS1yr2')

ggplot(data.frame(cbind(UrAs.subset,fe.1$FCmatrix)), aes(x = DateEnr, y = DateEnr.1, col = OZ_lean_meat_eq)) + 
  geom_point() + scale_color_gradientn(colours = rainbow(5)) + geom_smooth() + 
    facet_wrap(~ cut(OZ_lean_meat_eq, 
                           breaks = quantile(OZ_lean_meat_eq, seq(0,1,length.out = 6)),
                           include.lowest = T))

ggplot(data.frame(cbind(UrAs.subset,fe.1$FCmatrix)), aes(x = DateEnr, y = DateEnr.1, col = WaterAS1yr2)) + 
  geom_point() + scale_color_gradientn(colours = rainbow(5)) + geom_smooth() + 
  facet_wrap(~ cut(WaterAS1yr2, 
                   breaks = seq(0,max(WaterAS1yr2),length.out = 5),
                   include.lowest = T))

tree <- TreeSurrogate$new(Pred.1, maxdepth = 4)


plot(fe.1, plot_seq = match(imp.1$results$feature[1:10],colnames(fe.1$FCmatrix)),orderByImportance = F,
     GOF_args = list(col = rgb(1,0,0,0.8)))

ggplot(data.frame(cbind(UrAs.subset,fe.1$FCmatrix)), aes(x = WaterAS1yr2, y = WaterAS1yr2.1, col = DateEnr)) + 
  geom_point() + scale_color_gradientn(colours = rainbow(5)) + geom_smooth()


#fe.1 = FeatureEffects$new(Pred.1)
#fe.1$plot(features = head(imp.1$results$feature,10))


fe.2 = forestFloor(rf.InAs,Pred.2$data$X)

plot(fe.2, plot_seq = match(imp.2$results$feature[1:10],colnames(fe.2$FCmatrix)),orderByImportance = F,
     GOF_args = list(col = rgb(1,0,0,0.8)))



Pred.3 = Predictor$new(rf.AsmAsB, data = AsmAsB.subset %>% select(-total_minus_AsB),
                       y = log(AsmAsB.subset$total_minus_AsB))

imp.3 = FeatureImp$new(Pred.3, loss = 'mse', compare = 'difference')

ggplot(imp.3$results[1:10,], aes(y = factor(feature,levels = rev(feature)), 
                                 x = importance, xmin = importance.05,xmax = importance.95)) + 
  geom_errorbarh(height = 0.2, size = 1, col = 'darkgrey') + geom_point() + theme_light() + 
  labs(title = 'Random Forest on Log(Urinary Arsenic - AsB)', y = 'Feature', 
       x = 'Increase in residual error after permutation') + 
  theme(axis.text.y = element_text(angle = 30,hjust = 0.8,size = 10))

fe.3 = forestFloor(rf.AsmAsB,Pred.3$data$X)

plot(fe.3, plot_seq = match(imp.3$results$feature[1:10],colnames(fe.3$FCmatrix)),orderByImportance = F,
     GOF_args = list(col = rgb(1,0,0,0.8)), limitY = F)


