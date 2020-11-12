#Manuscript plots
library(ggplot2)
library(tidyverse)
library(lubridate)

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b~italic(x)*","~~italic(r)^2~"="~r2~","~~italic(p)~"="~pval, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3),
                        pval = format(summary(m)$coefficients[2,4], digits = 3)))
  as.character(as.expression(eq));
}
### Data load in and variable collation
ar.dat = readxl::read_xlsx('../../../FOR_ANDRES_012920.xlsx')
chile<-read.csv("../../../Chile_Arsenic_Data_Redux.csv")
chile = chile %>% mutate(Cancer = !is.na(Other_cancer) | Cancer_type != 'control')

chile = chile %>% group_by(SubjectID) %>% mutate(yrin = year(DateEnr)) %>% 
  mutate(WaterAS1yr = Exp_WaterAS[yrin == Exp_Year])

chile = chile %>% filter(!duplicated(SubjectID))

chile$WaterAS1yr2<-ifelse(chile$WaterAS1yr==0,0.1,chile$WaterAS1yr)

chile = subset(chile,!is.na(UrinaryArsenic))
chile$total_minus_AsB = chile$UrinaryArsenic - chile$UrinaryAsBet
chile$total_minus_AsB[chile$total_minus_AsB < 0] = 0.1

chile$DateEnr = as.Date(chile$DateEnr)

chile$allmuni = c('All municipal water','Not all municipal water')[2-(chile$prop.muni==1)]

chile$water_intake = ar.dat$TKHL[match(chile$SubjectID,ar.dat$commonID)]

propdat = readxl::read_xlsx('../../../WATER_INFO_060520.xlsx')
chile$prop_muni = propdat$percent_muni[match(chile$SubjectID,propdat$commonID)]/100

lm.creat = lm(UrinaryArsenic ~ UrinaryCreat, chile)
lm.creat2 = lm(UrinaryArsenic ~ UrinaryCreat + Exp_Age + factor(Sex) + Wght, chile)
chile$UrAr.CreatAdj = NA
chile$UrAr.CreatAdj[!is.na(chile$UrinaryCreat) & !is.na(chile$Wght)] = lm.creat2$residuals
chile$UrAr.CreatAdj = chile$UrAr.CreatAdj + lm.creat$coefficients[1] + 
  lm.creat$coefficients[2] * mean(chile$UrinaryCreat,na.rm=T)

ggplot(chile, aes(x = UrinaryCreat, y = UrinaryArsenic)) + stat_smooth(method = 'lm') + geom_point() + 
  annotate('text',300, 500,label = lm_eqn(data.frame(x = chile$UrinaryCreat,
                                                     y = chile$UrinaryArsenic)), parse = TRUE,size = 3)
ggplot(chile, aes(x = UrinaryCreat, y = UrAr.CreatAdj)) + stat_smooth(method = 'lm') + geom_point() + 
  annotate('text',300, 500,label = lm_eqn(data.frame(x = chile$UrinaryCreat,
                                                     y = chile$UrAr.CreatAdj)), parse = TRUE,size = 3)

ggplot(subset(chile,!is.na(UrinaryCreat)), aes(x = WaterAS1yr2, y = UrinaryArsenic)) + stat_smooth(method = 'lm') + geom_point() + 
  annotate('text',40, 500,label = lm_eqn(data.frame(x = chile$WaterAS1yr2[!is.na(chile$UrAr.CreatAdj)],
                                                     y = chile$UrinaryArsenic[!is.na(chile$UrAr.CreatAdj)])), parse = TRUE,size = 3)

ggplot(chile, aes(x = WaterAS1yr2, y = UrAr.CreatAdj)) + stat_smooth(method = 'lm') + geom_point() + 
  annotate('text',40, 500,label = lm_eqn(data.frame(x = chile$WaterAS1yr2,
                                                    y = chile$UrAr.CreatAdj)), parse = TRUE,size = 3)

####Figure 2: histogram of water intake

TW_inflate = .9/.7 #based on ~70% of water from fluids, ~20% from diet, as currently in text

png('Fig2_Chile_TFI_histogram.png',width = 360, height = 360)

ggplot(chile, aes(x = water_intake * TW_inflate)) + 
  geom_histogram(bins = 50) + 
  labs(x = 'Fluid Intake from Food and Water') + 
  theme_bw()

dev.off()


####Figure S6: histogram of fish and seafood consumption

png('FigS6_Chile_Fish_histogram.png',width = 360, height = 360)

ggplot(chile, aes(x = OZ_fish_seafood * 28.3495)) + 
  geom_histogram(bins = 50) + 
  labs(x = 'Daily Fish and Seafood Consumption (g)') + 
  theme_bw()

dev.off()

####Figure 3: water AS vs urine AS

##3a: On HEALS scale 
healsminx = 0.1
healsmaxx = 864
healsminy = 1
healsmaxy = 2000

png('Fig3a_Chile_AS_water_vs_urine.png',width = 360, height = 360)

ggplot(chile, aes(x = WaterAS1yr2, y = UrinaryArsenic)) + 
  geom_point() + 
  geom_smooth(method = 'lm', fullrange = T, se = F, col = 'black', lwd = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, col = 'red') +
  labs(x = 'Arsenic Concentration in Drinking Water (µg/L)',
       y = 'Arsenic Concentration in Urine (µg/L)') + 
  xlim(healsminx, healsmaxx) + ylim(healsminy, healsmaxy) + 
  theme_bw()

dev.off()

##3b: On own scale

png('Fig3b_Chile_AS_water_vs_urine.png',width = 360, height = 360)

ggplot(chile, aes(x = WaterAS1yr2, y = UrinaryArsenic)) + 
  geom_point() + 
  stat_smooth(method = 'lm', fullrange = T, se = F, col = 'red', lwd = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, col = 'black') +
  labs(x = 'Arsenic Concentration in Drinking Water (µg/L)',
       y = 'Arsenic Concentration in Urine (µg/L)') + 
  theme_bw() + scale_x_continuous(expand = c(0.01,0))

dev.off()

##3c: Binned / with error bars on own scale
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
                   ysd = sd(UrinaryArsenic))

png('Fig3c_Chile_AS_water_vs_urine.png',width = 360, height = 360)

ggplot(chile.bin, aes(x=xmean, y=ymean, xmax = xhi, xmin = xlo, ymax = yhi, ymin = ylo)) +
  geom_errorbar(col = rgb(0.5,0.5,0.5)) + 
  geom_errorbarh(col = rgb(0.5,0.5,0.5)) + 
  geom_point(size = 2, col = rgb(0.5,0.5,0.5)) +
  geom_smooth(method = 'lm', fullrange = T, se = F, col = 'black', lwd = 0.5) + 
  labs(x = 'Arsenic Concentration in Drinking Water (µg/L)',
       y = 'Arsenic Concentration in Urine (µg/L)') + 
  theme_bw() + scale_x_continuous(expand = c(0.01,0)) + 
  geom_abline(color="red", lty = 2)

dev.off()

####Figure 4: Arsenic intake vs excretion using mean parameters for water / seafood

#Inflation factor for arsenic lost to feces
Ar.expand = 100/ (100-6.065280)

#TFI vs urine volume
ur.dat = read.csv('TFI_vs_urine.csv')
ur.lm = lm(Urine_24 ~ TFI, ur.dat)

#mean seafood concentration
tuna.mean = weighted.mean(c(1130, 256,2460,878,1510, 1170,1060),
                          w = 1/(c(12.24,45.92,70.41, 83.16, 33.16, 37.76,19.90)/1.96)^2)

tuna.var = mean((c(12.24,45.92,70.41, 83.16, 33.16, 37.76,19.90)/1.96)^2)   

hake.mean = weighted.mean(c(581, 2940, 4420),
                          w = 1/(c(12,117,185)/1.96)^2)

hake.var = mean((c(12,117,185)/1.96)^2)   

mackerel.mean = weighted.mean(c(1530, 1720, 830),
                          w = 1/(c(74,26,72)/1.96)^2)

mackerel.var = mean((c(74,26,72)/1.96)^2)   

otherfish.mean = weighted.mean(c(313, 348, 310, 94, 261, 337, 90, 286,
                                 377, 39, 146, 173),
                              w = 1/(c(6, 17, 21, 7, 31, 5, 8, 10,
                                     31, 6, 5, 6)/1.96)^2)

otherfish.var = mean((c(6, 17, 21, 7, 31, 5, 8, 10,
                        31, 6, 5, 6)/1.96)^2)   

clams.mean = weighted.mean(c(1480, 1050, 748, 2660, 988),
                               w = 1/(c(131, 41, 82, 109, 33)/1.96)^2)

clams.var = mean((c(131, 41, 82, 109, 33)/1.96)^2)   

shellfish.mean = weighted.mean(c(333, 1660, 6320, 2440, 444, 
                                 7560, 236, 2650, 6560, 557, 
                                 8360, 7880, 10100, 4030, 22200, 
                                 8730, 94, 670),
                           w = 1/(c(21, 41, 211, 69, 40, 481, 28, 68,
                                  613, 42, 607, 426, 164, 61, 760,
                                  116, 2, 24)/1.96)^2)

shellfish.var = mean((c(21, 41, 211, 69, 40, 481, 28, 68,
                        613, 42, 607, 426, 164, 61, 760,
                        116, 2, 24)/1.96)^2)   

sfood.props = c(10.5, 7, 1.8, 7.6, 3.4, 2.87)
sfood.props = sfood.props/sum(sfood.props)  

fish.conc = weighted.mean(c(tuna.mean, hake.mean, mackerel.mean, 
                            otherfish.mean, clams.mean, shellfish.mean),
                          w = sfood.props) / 1e3 * 28.3495

# fishconcsim = replicate(1000, {
#   tuna = rnorm(1e3, tuna.mean, sqrt(tuna.var))
#   hake = rnorm(1e3, hake.mean, sqrt(hake.var))
#   mackerel = rnorm(1e3, mackerel.mean, sqrt(mackerel.var))
#   otherfish = rnorm(1e3, otherfish.mean, sqrt(otherfish.var))
#   clams = rnorm(1e3, clams.mean, sqrt(clams.var))
#   shellfish = rnorm(1e3, shellfish.mean, sqrt(shellfish.var))
#   
#   weighted.mean(c(mean(tuna),mean(hake),mean(mackerel),
#     mean(otherfish), mean(clams), mean(shellfish)), w = sfood.props)
# })
chile = subset(chile, !is.na(OZ_fish_seafood))

chile$AS_in2 = chile$water_intake * chile$WaterAS1yr2 + chile$OZ_fish_seafood * fish.conc
chile$AS_out = (coef(ur.lm)[1] + coef(ur.lm)[2] * chile$water_intake * TW_inflate) * 
  chile$UrinaryArsenic * Ar.expand 

chile$AS_in = chile$water_intake * chile$WaterAS1yr2
# chile$AS_out = (coef(ur.lm)[1] + coef(ur.lm)[2] * chile$water_intake * TW_inflate) * 
#   chile$UrinaryArsenic * Ar.expand 
summary(lm(AS_out ~ AS_in, chile))
 

png('Fig4_Chile_AS_mass_in_vs_out.png',width = 360, height = 360)

ggplot(chile, aes(x=AS_in2, y=AS_out)) +
  geom_point() + stat_smooth(method = 'lm', fullrange = T, se = F, col = 'red', lwd = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, col = 'black') +
  labs(x = 'Mass Arsenic Consumed (µg)',
       y = 'Mass Arsenic Excreted (µg)') + 
  theme_bw() + scale_x_continuous(expand = c(0.01,0))

dev.off()

#Version inferring total arsenic mass excreted from creatinine concentration
##Using equation and RMSE from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3022241

pred_total_creatinine = function(weight, age, black, female, sim.errors = F){
  out = 879.89 + 12.51 * weight - 6.19 * age + 34.51 * black - 379.42 * female
  if(sim.errors){
    out = rnorm(length(out), out, 357)
  }
  return(out)
}

#assume Urinary Creatinine is in units of mg/deciLiter

chile$AS_out2 = chile$UrinaryArsenic/(chile$UrinaryCreat * 10) * 
  pred_total_creatinine(chile$Wght, chile$Exp_Age, 0, chile$Sex - 1) * Ar.expand 

summary(lm(AS_out2 ~ AS_in, chile))

png('Fig4_Chile_AS_mass_in_vs_out_creatinine_adj.png',width = 360, height = 360)

ggplot(chile, aes(x=AS_in, y=AS_out)) +
  geom_point() + stat_smooth(method = 'lm', fullrange = T, se = F, col = 'black',lwd = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, col = 'red') +
  labs(x = 'Mass Arsenic Consumed (µg)',
       y = 'Mass Arsenic Excreted (µg)') + 
  theme_bw() + scale_x_continuous(expand = c(0.01,0)) + 
  annotate('text', 300, 800, label = lm_eqn(df = data.frame(x = chile$AS_in, 
                                                            y = chile$AS_out)),
           parse = T, size = 3) + 
  labs(title = 'Water intake only, drinking water-based excretion')

ggplot(chile, aes(x=AS_in2, y=AS_out)) +
  geom_point() + stat_smooth(method = 'lm', fullrange = T, se = F, col = 'black',lwd = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, col = 'red') +
  labs(x = 'Mass Arsenic Consumed (µg)',
       y = 'Mass Arsenic Excreted (µg)') + 
  theme_bw() + scale_x_continuous(expand = c(0.01,0)) + 
  annotate('text', 300, 500, label = lm_eqn(df = data.frame(x = chile$AS_in2, 
                                                            y = chile$AS_out)),
           parse = T, size = 3) + 
  labs(title = 'Water and seafood intake, drinking water-based excretion')



ggplot(chile, aes(x=AS_in, y=AS_out2)) +
  geom_point() + stat_smooth(method = 'lm', fullrange = T, se = F, col = 'black',lwd = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, col = 'red') +
  labs(x = 'Mass Arsenic Consumed (µg)',
       y = 'Mass Arsenic Excreted (µg)') + 
  theme_bw() + scale_x_continuous(expand = c(0.01,0)) + 
  annotate('text', 300, 400, label = lm_eqn(df = data.frame(x = chile$AS_in, 
                                                            y = chile$AS_out2)),
           parse = T, size = 3) + 
  labs(title = 'Water intake only, creatinine-based excretion')

ggplot(chile, aes(x=AS_in2, y=AS_out2)) +
  geom_point() + stat_smooth(method = 'lm', fullrange = T, se = F, col = 'black',lwd = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, col = 'red') +
  labs(x = 'Mass Arsenic Consumed (µg)',
       y = 'Mass Arsenic Excreted (µg)') + 
  theme_bw() + scale_x_continuous(expand = c(0.01,0)) + 
  annotate('text', 300, 400, label = lm_eqn(df = data.frame(x = chile$AS_in2, 
                                                            y = chile$AS_out2)),
           parse = T, size = 3) + 
  labs(title = 'Water and seafood intake, creatinine-based excretion')


dev.off()


#plot(chile$AS_out, chile$AS_out2)
#abline(0,1)
#results in generally lower mass of arsenic

# exp_creat = pred_total_creatinine(chile$Wght, chile$Exp_Age, 0, chile$Sex - 1, F)
# obs_tot1 = chile$UrinaryCreat * (coef(ur.lm)[1] + coef(ur.lm)[2] * chile$water_intake * TW_inflate) * 10
# 
# plot(seq(0,max(exp_creat,na.rm=T)), seq(0,max(exp_creat,na.rm=T)), type = 'l', ylab = 'calculated CER',
#      xlab = 'expected CER', main = 'assume urinary creatinine is concentration in mg/L')
# points(exp_creat, obs_tot1)


####Figure 5: Arsenic intake vs excretion MC simulation histograms

tuna.dist = function(x){
  rnorm(x, weighted.mean(c(1130, 256,2460,878,1510, 1170,1060), 
                         1/(c(12.24,45.92,70.41, 83.16,
                              33.16, 37.76,19.90) / 1.96)^2),
        sd = sqrt(mean((c(12.24,45.92,70.41, 83.16,
                          33.16, 37.76,19.90) / 1.96)^2)))
}

hake.dist = function(x){
  rnorm(x, weighted.mean(c(581, 2940, 4420), 1/(c(12,117,185) / 1.96)^2),
        sd = sqrt(mean((c(12,117,185) / 1.96)^2)))
        }

mackerel.dist = function(x){
  rnorm(x, weighted.mean(c(1530, 1720, 830), 
                         1/(c(74,26,72) / 1.96)^2),
        sd = sqrt(mean((c(74,26,72) / 1.96)^2)))
}

otherfish.dist = function(x){
  rnorm(x, weighted.mean(c(313, 348, 310, 94, 261, 337, 90, 286,
                           377, 39, 146, 173), 
                         1/(c(6, 17, 21, 7, 31, 5, 8, 10,
                              31, 6, 5, 6) / 1.96)^2),
        sd = sqrt(mean((c(6, 17, 21, 7, 31, 5, 8, 10,
                          31, 6, 5, 6) / 1.96)^2)))
}

clams.dist = function(x){
  rnorm(x, weighted.mean(c(1480, 1050, 748, 2660, 988), 
                         1/(c(131, 41, 82, 109, 33) / 1.96)^2),
        sd = sqrt(mean((c(131, 41, 82, 109, 33) / 1.96)^2)))
}

shellfish.dist = function(x){
  rnorm(x, weighted.mean(c(333, 1660, 6320, 2440, 444, 
                           7560, 236, 2650, 6560, 557, 
                           8360, 7880, 10100, 4030, 22200, 
                           8730, 94, 670), 
                         1/(c(21, 41, 211, 69, 40, 481, 28, 68,
                              613, 42, 607, 426, 164, 61, 760,
                              116, 2, 24) / 1.96)^2),
        sd = sqrt(mean((c(21, 41, 211, 69, 40, 481, 28, 68,
                          613, 42, 607, 426, 164, 61, 760,
                          116, 2, 24) / 1.96)^2)))
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

uv.sim = function(wi){
  uv = rowSums(rmvnorm(length(wi),coef(ur.lm),vcov(ur.lm)) * cbind(1,wi)) + rnorm(length(wi),0,summary(ur.lm)$sigma)
  while(any(uv<0)){
    uv[uv<0] = rowSums(rmvnorm(sum(uv<0),coef(ur.lm),vcov(ur.lm)) * cbind(1,wi[uv<0])) + rnorm(sum(uv<0),0,summary(ur.lm)$sigma)
  }
  uv
}

library(mvtnorm)

MC1 = lapply(1:1000, function(i){
  x_true = chile$WaterAS1yr2 * chile$water_intake 
  y_true = chile$UrinaryArsenic * uv.sim(chile$water_intake * TW_inflate) * Ar.expand
  res = lm(y_true ~ x_true)
  data.frame(intcpt = coef(res)[1], slp = coef(res)[2], rsq = summary(res)$r.squared,
             resid.mean = mean(y_true-x_true), resid.med = median(y_true - x_true),
             resid.prop.pos = mean((y_true - x_true) > 0))
  })

MC1 = do.call(rbind,MC1)

MC2 = lapply(1:1000, function(i){
  x_true = chile$WaterAS1yr2 * chile$water_intake + sfood.sim(chile$OZ_fish_seafood)
  y_true = chile$UrinaryArsenic * uv.sim(chile$water_intake * TW_inflate) * Ar.expand
  res = lm(y_true ~ x_true)
  data.frame(intcpt = coef(res)[1], slp = coef(res)[2], rsq = summary(res)$r.squared,
             resid.mean = mean(y_true-x_true), resid.med = median(y_true - x_true),
             resid.prop.pos = mean((y_true - x_true) > 0))
})

MC2 = do.call(rbind, MC2)

library(gridExtra)

png('Fig5_Chile_water_intake.png', width = 720, height = 360)

p1.a = ggplot(MC1, aes(x = intcpt)) + geom_histogram() + theme_bw() + 
  geom_vline(xintercept = 0, lty = 2) + xlab('Intercept (µg)')
p1.b = ggplot(MC1, aes(x = slp)) + geom_histogram() + theme_bw() + 
  geom_vline(xintercept = 1, lty = 2) + xlab('Slope')
p1.c = ggplot(MC1, aes(x = rsq)) + geom_histogram() + theme_bw() + xlab(expression(bold(r^2)))

grid.arrange(p1.a,p1.b,p1.c,nrow = 1)

dev.off()

png('Fig5_Chile_water&seafood_intake.png', width = 720, height = 360)

p2.a = ggplot(MC2, aes(x = intcpt)) + geom_histogram() + theme_bw() + 
  geom_vline(xintercept = 0, lty = 2) + xlab('Intercept (µg)')
p2.b = ggplot(MC2, aes(x = slp)) + geom_histogram() + theme_bw() + 
  geom_vline(xintercept = 1, lty = 2) + xlab('Slope')
p2.c = ggplot(MC2, aes(x = rsq)) + geom_histogram() + theme_bw() + xlab(expression(bold(r^2)))

grid.arrange(p2.a,p2.b,p2.c,nrow = 1)

dev.off()

####Figure 6: Arsenic intake vs excretion MC simulation cloudplot
MC1$source = 'Chile: Water'
MC2$source = 'Chile: Water & Seafood'
grandMC = rbind(MC1, MC2)

unmMC = read.csv('../../../NavajoMC.csv')
unmMC = unmMC[,-1]

grandMC = rbind(grandMC, unmMC)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

png('Fig6_Chile_UNMcloudplot.png', width = 720, height = 360)

ggplot(grandMC, aes(x=intcpt, y = slp, col = source, lty = source, shape = source)) + 
  geom_point(alpha = 0.1) + 
  #stat_density_2d(aes(alpha = ..level..), geom = 'polygon') + 
  #stat_density_2d(bins = 5) + 
  theme_bw() + geom_hline(yintercept = 1, lty = 2, show.legend = F) +
  geom_vline(xintercept = 0, lty = 2, show.legend = F) + 
  geom_point(x = 0, y = 1, col = 'black', show.legend = F) + 
  #scale_alpha(guide = F,range = c(0,0.5)) +  
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  xlab('Intercept (µg / day)') + ylab('Slope') + 
  scale_linetype_manual(name = 'Study / Intake sources',values = c(1,1,2,2,3,3,3)) + 
  scale_shape_manual(name = 'Study / Intake sources', values = c(15,15,16,16,17,17,17)) + 
  scale_color_manual(name = 'Study / Intake sources',values = gg_color_hue(7)[c(6,7,1,2,3,4,5)])
#scale_color_brewer(type = 'qual',palette = 6,name = 'Study / Intake sources') 

dev.off()
