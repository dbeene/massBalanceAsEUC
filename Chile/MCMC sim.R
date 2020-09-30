#MCMC simulation

library(tidyverse)
library(lubridate)
ar.dat = readxl::read_xlsx('../../../FOR_ANDRES_012920.xlsx')
chile<-read.csv("../../../Chile_Arsenic_Data_Redux.csv")
chile = chile %>% mutate(Cancer = !is.na(Other_cancer) | Cancer_type != 'control')

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

chile$allmuni = c('All municipal water','Not all municipal water')[2-(chile$prop.muni==1)]

chile$water_intake = ar.dat$TKHL[match(chile$SubjectID,ar.dat$commonID)]

propdat = readxl::read_xlsx('../../../WATER_INFO_060520.xlsx')
chile$prop_muni = propdat$percent_muni[match(chile$SubjectID,propdat$commonID)]/100

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b~italic(x)*","~~italic(r)^2~"="~r2~","~~italic(p)~"="~pval, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3),
                        pval = format(summary(m)$coefficients[2,4], digits = 3)))
  as.character(as.expression(eq));
}

p1 = ggplot(chile,aes(x = UrinaryAsBet, y = UrinaryArsenic)) + geom_point() + geom_smooth(method = 'lm') + 
  geom_smooth(col = rgb(0,1,0,1), fill = rgb(0,1,0), alpha = 0.2, lty = 2) + theme_bw() + 
  annotate('text',x = 300, y = 100, label = lm_eqn(data.frame(x = chile$UrinaryAsBet,
                                                       y = chile$UrinaryArsenic)), parse = TRUE,size = 3)
  
p2 = ggplot(chile,aes(x = UrinaryAsBet, y = UrinaryDiMeAr)) + geom_point() + geom_smooth(method = 'lm') + 
  geom_smooth(col = rgb(0,1,0,1), fill = rgb(0,1,0), alpha = 0.2, lty = 2) + theme_bw() + 
  annotate('text',x = 300, y = 200, label = lm_eqn(data.frame(x = chile$UrinaryAsBet,
                                                              y = chile$UrinaryDiMeAr)), parse = TRUE,size = 3)

p3 = ggplot(chile,aes(x = UrinaryAsBet, y = UrinaryArsenic - UrinaryAsBet)) + geom_point() + geom_smooth(method = 'lm') + 
  geom_smooth(col = rgb(0,1,0,1), fill = rgb(0,1,0), alpha = 0.2, lty = 2) + theme_bw() + 
  annotate('text',x = 300, y = 250, label = lm_eqn(data.frame(x = chile$UrinaryAsBet,
                                                              y = chile$UrinaryArsenic - chile$UrinaryAsBet)), parse = TRUE,size = 3)

p4 = ggplot(chile,aes(x = UrinaryCreat, y = UrinaryArsenic)) + geom_point() + geom_smooth(method = 'lm') + 
  geom_smooth(col = rgb(0,1,0,1), fill = rgb(0,1,0), alpha = 0.2, lty = 2) + theme_bw() + 
  annotate('text',x = 300, y = 450, label = lm_eqn(data.frame(x = chile$UrinaryCreat,
                                                              y = chile$UrinaryArsenic)), parse = TRUE,size = 3)

p5 = ggplot(chile,aes(x = OZ_fish_seafood, y = UrinaryAsBet)) + geom_point() + geom_smooth(method = 'lm') + 
  geom_smooth(col = rgb(0,1,0,1), fill = rgb(0,1,0), alpha = 0.2, lty = 2) + theme_bw() + 
  annotate('text',x = 3, y = 300, label = lm_eqn(data.frame(x = chile$OZ_fish_seafood,
                                                              y = chile$UrinaryAsBet)), parse = TRUE,size = 3)

p6 = ggplot(chile,aes(x = water_intake, y = UrinaryCreat)) + geom_point() + geom_smooth(method = 'lm') + 
  geom_smooth(col = rgb(0,1,0,1), fill = rgb(0,1,0), alpha = 0.2, lty = 2) + theme_bw() + 
  annotate('text',x = 4, y = 400, label = lm_eqn(data.frame(x = chile$water_intake,
                                                            y = chile$UrinaryCreat)), parse = TRUE,size = 3)

p7 = ggplot(chile,aes(x = UrinaryCreat, y = UrinaryAsBet)) + geom_point() + geom_smooth(method = 'lm') + 
  geom_smooth(col = rgb(0,1,0,1), fill = rgb(0,1,0), alpha = 0.2, lty = 2) + theme_bw() + 
  annotate('text',x = 200, y = 400, label = lm_eqn(data.frame(x = chile$UrinaryCreat,
                                                            y = chile$UrinaryAsBet)), parse = TRUE,size = 3)



#using urine vs fluid intake data from https://www.hindawi.com/journals/dm/2015/231063/ to get conditional distribution
ur.dat = read.csv('TFI_vs_urine.csv')

ggplot(ur.dat,aes(x = TFI, y = Urine_24)) + geom_point() + stat_smooth(method = 'lm',formula = y~x-1) + 
  stat_smooth(method = 'lm',formula = y~x, col = 'orange',fill = 'orange') + stat_smooth(col = 'red', fill = 'red') 

ur.lm = lm(Urine_24 ~ TFI, ur.dat)
library(mvtnorm) #for multivariate normal simulation of coefficients

uv.sim = function(wi){
  uv = rowSums(rmvnorm(length(wi),coef(ur.lm),vcov(ur.lm)) * cbind(1,wi)) + rnorm(length(wi),0,summary(ur.lm)$sigma)
  while(any(uv<0)){
    uv[uv<0] = rowSums(rmvnorm(sum(uv<0),coef(ur.lm),vcov(ur.lm)) * cbind(1,wi[uv<0])) + rnorm(sum(uv<0),0,summary(ur.lm)$sigma)
  }
  uv
}

#simulating arsenic measurement error in water
#https://www.sciencedirect.com/science/article/pii/S1047279710001638#bib39 SD = 0.2 * mean

# ar.water.sim = function(waterars){
#   wa_true = rnorm(waterars, waterars, 0.2 * waterars)
#   while(any(wa_true<0)){
#     wa_true[wa_true<0] = rnorm(sum(wa_true<0), waterars[wa_true<0], 0.2 * waterars[wa_true<0])
#   }
#   wa_true
# }

#shifting to using the Horwitz curve

ar.water.sim = function(waterars){
   wa_true = rnorm(waterars, waterars, .02 * (waterars/1e6)^.8495 * 1e6)
   while(any(wa_true<0)){
     wa_true[wa_true<0] = rnorm(sum(wa_true<0), waterars[wa_true<0], 0.02 * (waterars[wa_true<0]/1e6)^.8495 * 1e6)
   }
   wa_true
 }

#could potentially use
#Northern argentina temporal variability in well water arsenic
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3013252/

#Urinary arsenic measurement error
#use Horwirtz curve https://pubs.acs.org/doi/10.1021/ac9608376,
#AFS data from https://link.springer.com/article/10.1007/s00420-004-0562-x

# Hcurve.dat = data.frame(C = c(3.1,4.1,4.1,
#                               6.9,7.9,
#                               8.8,9.5,10.1, 12.1,
#                               13.1,15.1,17.7,24,
#                               27.4,27.7,28.3,31,
#                               34.5),
#                         CoV = c(0.12855,0.06162,
#                                 0.06047,0.06047,
#                                 0.03509,0.02297,
#                                 0.04028,0.04835,
#                                 0.03509,0.05412,
#                                 0.01201, 0.04835,
#                                 0.02701,0.03624,
#                                 0.02989,0.02643,
#                                 0.03047,0.05355))
# 
# Hcurve.dat$sigma = Hcurve.dat$C * Hcurve.dat$CoV                                
# 
# Hcurve = lm(log10(sigma)~log10(C),Hcurve.dat)                          
# ggplot(Hcurve.dat,
#        aes(x = log10(C),y = log10(CoV))) + geom_point() + 
#   geom_smooth(method = 'lm')
# 
# ar.ur.sim = function(urars){
#   sigmas = 10^(rowSums(rmvnorm(length(urars), coef(Hcurve), vcov(Hcurve)) * 
#     cbind(1,log10(urars))) + rnorm(urars,0,summary(Hcurve)$sigma))
#   
#   ua_true = rnorm(urars,urars,sigmas)
#   while(any(ua_true<0)){
#     ua_true[ua_true<0] = rnorm(urars[ua_true<0],urars[ua_true<0],sigmas[ua_true<0])
#   }
#   ua_true
# }

#first simulation, just water arsenic 
chile.mc = subset(chile,!is.na(OZ_fish_seafood) & !Cancer)


MC1 = lapply(1:1000, function(i){
  x_true = ar.water.sim(chile.mc$WaterAS1yr2) * chile.mc$water_intake
  y_true = ar.water.sim(chile.mc$UrinaryArsenic) * uv.sim(chile.mc$water_intake)
  res = lm(y_true ~ x_true)
  data.frame(intcpt = coef(res)[1], slp = coef(res)[2], rsq = summary(res)$r.squared,
             resid.mean = mean(y_true-x_true), resid.med = median(y_true - x_true),
             resid.prop.pos = mean((y_true - x_true) > 0))
})

MC1.1 = lapply(1:1000, function(i){ #random water intake with UNM distribution
  water.in = qnorm(runif(nrow(chile.mc),pnorm(0,3,1),1),3,1)
  x_true = ar.water.sim(chile.mc$WaterAS1yr2) * water.in
  y_true = ar.water.sim(chile.mc$UrinaryArsenic) * uv.sim(water.in)
  res = lm(y_true ~ x_true)
  data.frame(intcpt = coef(res)[1], slp = coef(res)[2], rsq = summary(res)$r.squared,
             resid.mean = mean(y_true-x_true), resid.med = median(y_true - x_true),
             resid.prop.pos = mean((y_true - x_true) > 0))
})

MC1.2 = lapply(1:1000, function(i){ #random water intake with Chile distribution
  water.in = qnorm(runif(nrow(chile.mc),pnorm(0,1.78,0.95),1),1.78,0.95)
  x_true = ar.water.sim(chile.mc$WaterAS1yr2) * water.in
  y_true = ar.water.sim(chile.mc$UrinaryArsenic) * uv.sim(water.in)
  res = lm(y_true ~ x_true)
  data.frame(intcpt = coef(res)[1], slp = coef(res)[2], rsq = summary(res)$r.squared,
             resid.mean = mean(y_true-x_true), resid.med = median(y_true - x_true),
             resid.prop.pos = mean((y_true - x_true) > 0))
})


MC1 = do.call(rbind, MC1)

MC1.1 = do.call(rbind,MC1.1)

MC1.2 = do.call(rbind,MC1.2)

rbind(colMeans(MC1),apply(MC1, 2, quantile, c(0.025, 0.975)))
rbind(colMeans(MC1.1),apply(MC1.1, 2, quantile, c(0.025, 0.975)))
rbind(colMeans(MC1.2),apply(MC1.2, 2, quantile, c(0.025, 0.975)))


p1 = ggplot(chile.mc,aes(x = WaterAS1yr2 * water_intake, y = UrinaryArsenic * (coef(ur.lm)[1] + 
                                                                          coef(ur.lm)[2] * water_intake))) +
  geom_point(alpha = 0.1) + geom_abline(data = MC1, aes(intercept = intcpt, slope = slp),col = 'red',alpha=0.01) + 
  theme_bw() + geom_abline(intercept = mean(MC1$intcpt), slope = mean(MC1$slp),col = 'blue', lwd = 1) + 
  xlab('Arsenic intake, ug') + ylab('Urinary arsenic, ug') + geom_abline(intercept = 0, slope = 1, lty = 2, lwd = 1) + 
  ylim(0,1400) + xlim(0,600) + labs(title = 'Chile cohort, water intake only',subtitle = 'reported intake')

p2 = ggplot(chile.mc,aes(x = WaterAS1yr2 * 3, y = UrinaryArsenic * (coef(ur.lm)[1] + 
                                                                            coef(ur.lm)[2] * 3))) +
  geom_point(alpha = 0.1) + geom_abline(data = MC1.1, aes(intercept = intcpt, slope = slp),col = 'red',alpha=0.01) + 
  theme_bw() + geom_abline(intercept = mean(MC1.1$intcpt), slope = mean(MC1.1$slp),col = 'blue', lwd = 1) + 
  xlab('Arsenic intake, ug') + ylab('Urinary arsenic, ug') + geom_abline(intercept = 0, slope = 1, lty = 2, lwd = 1) + 
  ylim(0,1400) + xlim(0,600) + labs(title = 'Chile cohort, water intake only',subtitle = 'simulated with mean 3, sd 1')

p3 = ggplot(chile.mc,aes(x = WaterAS1yr2 * 1.78, y = UrinaryArsenic * (coef(ur.lm)[1] + 
                                                                 coef(ur.lm)[2] * 3))) +
  geom_point(alpha = 0.1) + geom_abline(data = MC1.2, aes(intercept = intcpt, slope = slp),col = 'red',alpha=0.01) + 
  theme_bw() + geom_abline(intercept = mean(MC1.2$intcpt), slope = mean(MC1.2$slp),col = 'blue', lwd = 1) + 
  xlab('Arsenic intake, ug') + ylab('Urinary arsenic, ug') + geom_abline(intercept = 0, slope = 1, lty = 2, lwd = 1) + 
  ylim(0,1400) + xlim(0,600) + labs(title = 'Chile cohort, water intake only',subtitle = 'simulated with mean 1.78, sd 0.95')

grid.arrange(p1,p2,p3, nrow = 1)


#Adding in seafood arsenic
#concentrations from https://pubs.acs.org/doi/pdf/10.1021/acs.jafc.9b02314
#proportions from https://www.sciencedirect.com/science/article/pii/S0278691505001511

tuna.dist = function(x){
  means = c(1130, 256,2460,878,1510, 1170,1060)
  sd = c(12.24,45.92,70.41, 83.16,
         33.16, 37.76,19.90)
  inds = sample(length(means),length(x), replace = T)
  rnorm(x, means[inds], sd[inds])
}

hake.dist = function(x){
  means = c(581, 2940, 4420)
  sd = c(12,117,185) / 1.96
  inds = sample(length(means),length(x), replace = T)
  rnorm(x, means[inds], sd[inds])
}

mackerel.dist = function(x){
  means = c(1530, 1720, 830)
  sd = c(74,26,72) / 1.96
  inds = sample(length(means),length(x), replace = T)
  rnorm(x, means[inds], sd[inds])
}

otherfish.dist = function(x){
  means = c(313, 348, 310, 94, 261, 337, 90, 286,
            377, 39, 146, 173)
  sd = c(6, 17, 21, 7, 31, 5, 8, 10,
         31, 6, 5, 6) / 1.96
  inds = sample(length(means),length(x), replace = T)
  rnorm(x, means[inds], sd[inds])
}

clams.dist = function(x){
  means = c(1480, 1050, 748, 2660, 988)
  sd = c(131, 41, 82, 109, 33) / 1.96
  inds = sample(length(means),length(x), replace = T)
  rnorm(x, means[inds], sd[inds])
}

shellfish.dist = function(x){
  means = c(333, 1660, 6320, 2440, 444, 
            7560, 236, 2650, 6560, 557, 
            8360, 7880, 10100, 4030, 22200, 
            8730, 94, 670)
  sd = c(21, 41, 211, 69, 40, 481, 28, 68,
         613, 42, 607, 426, 164, 61, 760,
         116, 2, 24) / 1.96
  inds = sample(length(means),length(x), replace = T)
  rnorm(x, means[inds], sd[inds])
}

sfood.sim = function(oz.sf){
  props = c(10.5, 7, 1.8, 7.6, 3.4, 2.87)
  props = props / sum(props)
  
  conc = rbind(tuna.dist(oz.sf), 
  hake.dist(oz.sf),
  mackerel.dist(oz.sf), 
  otherfish.dist(oz.sf), 
  clams.dist(oz.sf), 
  shellfish.dist(oz.sf))
  
  conc = t(props) %*% conc 
  c(conc / 1e3 * 28.3495 * oz.sf)
#  plot(oz.sf,conc / 1e3 * 28.3495 * oz.sf)
}


MC2 = lapply(1:1000, function(i){
  x_true = ar.water.sim(chile.mc$WaterAS1yr2) * chile.mc$water_intake + 
    sfood.sim(chile.mc$OZ_fish_seafood)
  y_true = ar.water.sim(chile.mc$UrinaryArsenic) * uv.sim(chile.mc$water_intake)
  res = lm(y_true ~ x_true)
  data.frame(intcpt = coef(res)[1], slp = coef(res)[2], rsq = summary(res)$r.squared,
             resid.mean = mean(y_true-x_true), resid.med = median(y_true - x_true),
             resid.prop.pos = mean((y_true - x_true) > 0))
})


MC2.1 = lapply(1:1000, function(i){
  x_true = #ar.water.sim(chile.mc$WaterAS1yr2) * chile.mc$water_intake + 
    sfood.sim(chile.mc$OZ_fish_seafood)
  y_true = ar.water.sim(chile.mc$UrinaryArsenic) * uv.sim(chile.mc$water_intake)
  res = lm(y_true ~ x_true)
  data.frame(intcpt = coef(res)[1], slp = coef(res)[2], rsq = summary(res)$r.squared,
             resid.mean = mean(y_true-x_true), resid.med = median(y_true - x_true),
             resid.prop.pos = mean((y_true - x_true) > 0))
})


MC2 = do.call(rbind, MC2)

MC2.1 = do.call(rbind, MC2.1)

rbind(colMeans(MC2),apply(MC2, 2, quantile, c(0.025, 0.975)))

sf.concs = do.call(rbind,(lapply(1:100, function(x){
  ys = sfood.sim(chile.mc$OZ_fish_seafood)
  xs = chile.mc$OZ_fish_seafood
  data.frame(ys,xs)
})))

sf.coef = coef(lm(ys~xs, sf.concs))[2]


ggplot(chile.mc,aes(x = WaterAS1yr2 * water_intake + 
                      sf.coef * OZ_fish_seafood, 
                    y = UrinaryArsenic * (coef(ur.lm)[1] + 
                           coef(ur.lm)[2] * water_intake))) +
  geom_point(alpha = 0.1) + geom_abline(data = MC2, aes(intercept = intcpt, slope = slp),col = 'red',alpha=0.01) + 
  theme_bw() + geom_abline(intercept = mean(MC2$intcpt), slope = mean(MC2$slp),col = 'blue', lwd = 1) + 
  xlab('Arsenic intake, ug (water and dietary seafood)') + ylab('Urinary arsenic, ug') + geom_abline(intercept = 0, slope = 1, lty = 2, lwd = 1) + 
  ylim(0,1400) + xlim(0,600)


ggplot(chile.mc,aes(x = sf.coef * OZ_fish_seafood, 
                    y = UrinaryArsenic * (coef(ur.lm)[1] + 
                                            coef(ur.lm)[2] * water_intake))) +
  geom_point(alpha = 0.1) + geom_abline(data = MC2.1, aes(intercept = intcpt, slope = slp),col = 'red',alpha=0.01) + 
  theme_bw() + geom_abline(intercept = mean(MC2.1$intcpt), slope = mean(MC2.1$slp),col = 'blue', lwd = 1) + 
  xlab('Arsenic intake, ug (water and dietary seafood)') + ylab('Urinary arsenic, ug') + geom_abline(intercept = 0, slope = 1, lty = 2, lwd = 1) + 
  ylim(0,1400) + xlim(0,600)

#examine patterns of residuals for mean values
X1s = chile.mc$WaterAS1yr2 * chile.mc$water_intake
Y1s = chile.mc$UrinaryArsenic * (coef(ur.lm)[1] + coef(ur.lm)[2] * chile.mc$water_intake) - X1s
Xb1s = .bincode(X1s,quantile(X1s,seq(0,1,.05)),include.lowest = T)
Xb1means = tapply(X1s, Xb1s, mean)[Xb1s]
ggplot(data.frame(x = Xb1means,bin = Xb1s, y = Y1s), aes(x = Xb1means, y = y, group = bin)) + geom_boxplot() + 
  xlab('Mean expected arsenic intake, water only') + ylab('Residual with respect to mas balance line') + 
  geom_hline(yintercept = 0, col='red')




X2s = chile.mc$WaterAS1yr2 * chile.mc$water_intake + sf.coef * chile.mc$OZ_fish_seafood
Y2s = chile.mc$UrinaryArsenic * (coef(ur.lm)[1] + coef(ur.lm)[2] * chile.mc$water_intake) - X2s
Xb2s = .bincode(X2s,quantile(X2s,seq(0,1,.05)),include.lowest = T)
Xb2means = tapply(X2s, Xb2s, mean)[Xb2s]
ggplot(data.frame(x = Xb2means,bin = Xb2s, y = Y2s), aes(x = x, y = y, group = bin)) + geom_boxplot() + 
  xlab('Mean expected arsenic intake, water and seafood') + ylab('Residual with respect to mas balance line')+ 
  geom_hline(yintercept = 0, col='red')


#Adding in arsenic from meat products




#############################################################################################
#reverse approach of simulating deviation from good fit under violations of model assumptions

#True graph with water intake only

water.lm = lm(I(UrinaryArsenic * (coef(ur.lm)[1] + 
                                    coef(ur.lm)[2] * water_intake)) ~ I(WaterAS1yr2 * water_intake), chile.mc)

ggplot(chile.mc,aes(x = WaterAS1yr2 * water_intake, y = UrinaryArsenic * (coef(ur.lm)[1] + 
                                                                            coef(ur.lm)[2] * water_intake))) +
  geom_point(alpha = 0.1) + stat_smooth(method = 'lm', se = F, fullrange = T) + 
  theme_bw() +
  xlab('Arsenic intake, ug (water only)') + ylab('Urinary arsenic, ug') + geom_abline(intercept = 0, slope = 1, lty = 2, lwd = 1) + 
  ylim(0,1400) + xlim(0,600) + 
  geom_text(x = 500, y = 150, size = 4,
            label = paste0(round(coef(water.lm)[1],2),'+',round(coef(water.lm)[2],2),
                           'X \nR^2 = ', round(summary(water.lm)$r.squared,3)))

intakexintake1.lm = lm(I(WaterAS1yr2 * water_intake + 
                           sf.coef * OZ_fish_seafood) ~ I(WaterAS1yr2 * water_intake), chile.mc)

ggplot(chile.mc,aes(y = WaterAS1yr2 * water_intake + 
                      sf.coef * OZ_fish_seafood, 
                    x = WaterAS1yr2 * water_intake)) +
  geom_point(alpha = 0.1) + stat_smooth(method = 'lm', fullrange = T, se = F) + 
  theme_bw() +
  xlab('Arsenic intake, ug from water') + ylab('Arsenic intake, ug from water and seafood') + geom_abline(intercept = 0, slope = 1, lty = 2, lwd = 1) + 
  ylim(0,1400) + xlim(0,600) + 
  geom_text(x = 500, y = 150, size = 4,
            label = paste0(round(coef(intakexintake1.lm)[1],2),'+',round(coef(intakexintake1.lm)[2],2),
                           'X \nR^2 = ', round(summary(intakexintake1.lm)$r.squared,3)))


grandMCdf = rbind(MC1,MC2)
grandMCdf$scen = factor(rep(c('Chile water only','Chile water + seafood'),each = 1000),
                        levels = c('Chile water only','Chile water + seafood'))

library(gridExtra)

#it would be great if you could each use the same ranges of 0.2-1.7 for the slope and 0-180 ug/day for the intercept. 

inthist = ggplot(grandMCdf, aes(x = intcpt)) + geom_histogram(bins = 100) + geom_vline(xintercept = 0, lty = 2) + 
  facet_wrap(~ scen,nrow = 2) + xlim(0,180) + xlab('Intercept (ug / day)')

slopehist = ggplot(grandMCdf, aes(x = slp)) + geom_histogram(bins = 100) + geom_vline(xintercept = 1, lty = 2) + 
  facet_wrap(~ scen,nrow = 2) + xlim(0.2,1.7) + xlab ('Slope')

gridExtra::grid.arrange(inthist, slopehist,layout_matrix = matrix(c(1,2),ncol = 2))

ggplot(grandMCdf, aes(x=intcpt, y = slp, col = scen)) + geom_point(alpha = 0.1, shape=3) + 
  #stat_density_2d(aes(alpha = ..level..), geom = 'polygon') + 
  stat_density_2d(bins = 5) + 
  theme_bw() + geom_point(x = 0, y = 1, col = 'black') + geom_hline(yintercept = 1, lty = 2) + geom_vline(xintercept = 0, lty = 2) + 
  #scale_alpha(guide = F,range = c(0,0.5)) +  
  guides(fill = guide_legend(override.aes = list(shape = NA))) + xlim(0, 180) + ylim(0.2,1.7) + 
  xlab('Intercept (ug / day)') + ylab('Slope') + labs(col = 'Intake sources')



#####adding data from other studies
UNMdat = read.csv('UNMData.csv')[,-c(1,4:5)]

grandMCdf = grandMCdf[,-c(3:6)]

HEALSdat = read.csv('grandMCdfHEALS.csv')[,c(-1,-4)]

names(grandMCdf) = names(HEALSdat) = names(UNMdat)

grandMC = bind_rows(UNMdat,grandMCdf,HEALSdat)

grandMC$Study = rep(c("UNM","Chile","HEALS"), c(2000,2000,3000))
grandMC$scen = factor(grandMC$scen, levels = unique(grandMC$scen))
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
ggplot(grandMC, aes(x=Intercept, y = Slope, col = scen, lty = scen, shape = scen)) + 
  geom_point(alpha = 0.1) + 
  #stat_density_2d(aes(alpha = ..level..), geom = 'polygon') + 
  #stat_density_2d(bins = 5) + 
  theme_bw() + geom_hline(yintercept = 1, lty = 2, show.legend = F) +
  geom_vline(xintercept = 0, lty = 2, show.legend = F) + 
  geom_point(x = 0, y = 1, col = 'black', show.legend = F) + 
  #scale_alpha(guide = F,range = c(0,0.5)) +  
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  xlab('Intercept (ug / day)') + ylab('Slope') + 
  scale_linetype_manual(name = 'Study / Intake sources',values = c(1,1,2,2,3,3,3)) + 
  scale_shape_manual(name = 'Study / Intake sources', values = c(15,15,16,16,17,17,17)) + 
  scale_color_manual(name = 'Study / Intake sources',values = gg_color_hue(7)[c(6,7,1,2,3,4,5)])
  #scale_color_brewer(type = 'qual',palette = 6,name = 'Study / Intake sources') 

inthist = ggplot(grandMC, aes(x = Intercept)) + geom_histogram(bins = 100) + geom_vline(xintercept = 0, lty = 2) + 
  facet_wrap(~ scen,nrow = 7, scales = 'free_y') + xlab('Intercept (ug / day)')

slopehist = ggplot(grandMC, aes(x = Slope)) + geom_histogram(bins = 100) + geom_vline(xintercept = 1, lty = 2) + 
  facet_wrap(~ scen,nrow = 7, scales = 'free_y') + xlab ('Slope')

gridExtra::grid.arrange(inthist, slopehist,layout_matrix = matrix(c(1,2),ncol = 2))


#a serving size of fish is about 6 oz, so people reporting less than that probably have fish less than once per day
#so potential variability from how long since last seafood meal
chile.mc$fishrate = chile.mc$OZ_fish_seafood / 6






#Pomroy curve - y = 65.9e^(-.381x) + 30.4e^(-.0728x) + 3.7e^(-0.018x)
#expected time since last seafood meal is #days meal consumed per week

# spd        expected lag
# 1 or more  0
# 1/2        0.5
# 1/3        1
# 1/4        1.5