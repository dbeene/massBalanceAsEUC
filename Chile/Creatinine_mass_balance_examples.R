#simulating mass balance relationships using creatinine for excretion

#utility functions
##For extracting regression stats with mathematical notation
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b~italic(x)*","~~italic(r)^2~"="~r2~","~~italic(p)~"="~pval, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3),
                        pval = format(summary(m)$coefficients[2,4], digits = 3)))
  as.character(as.expression(eq));
}

##Total creatinine prediction based on weight, age, race, sex
##Provide variables from own data as appropriate 
##(note that black and female should be indicators, 1 if true, 0 if false)

pred_total_creatinine = function(weight, age, black, female, sim.errors = F){
  out = 879.89 + 12.51 * weight - 6.19 * age + 34.51 * black - 379.42 * female
  if(sim.errors){
    out = rnorm(length(out), out, 357)
  }
  return(out)
}


#Other common quantities
Ar.expand = 100/ (100-6.065280) #Inflation factor to account for arsenic lost to feces

gg_color_hue <- function(n) { #color spectrum for shared graphs
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#simuating mean intake and excretion
##water only
chile$As.in_water = chile$water_intake * chile$WaterAS1yr2
chile$As.out_creat = chile$UrinaryArsenic/(chile$UrinaryCreat * 10) * #our creatinine concentrations are in mg/dL, hence multiplication by 10
  pred_total_creatinine(weight = chile$Wght, age = chile$Exp_Age, 
                        black=  0, female = chile$Sex - 1) * Ar.expand 

ggplot(chile, aes(x = As.in_water, y = As.out_creat)) + 
  geom_point() + stat_smooth(method = 'lm', fullrange = T, se = F, col = 'black',lwd = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, col = 'red') +
  labs(x = 'Mass Arsenic Consumed (µg)',
       y = 'Mass Arsenic Excreted (µg)') + 
  theme_bw() + scale_x_continuous(expand = c(0.01,0)) + 
  annotate('text', 300, 800, label = lm_eqn(df = data.frame(x = chile$As.in_water, 
                                                            y = chile$As.out_creat)),
           parse = T, size = 3) + 
  labs(title = 'Water intake only, creatinine-based excretion')

##water + seafood
chile$As.in_water.seafood = chile$water_intake * chile$WaterAS1yr2 + fish.conc * chile$OZ_fish_seafood

ggplot(chile, aes(x = As.in_water.seafood, y = As.out_creat)) + 
  geom_point() + stat_smooth(method = 'lm', fullrange = T, se = F, col = 'black',lwd = 0.5) + 
  geom_abline(slope = 1, intercept = 0, lty = 2, col = 'red') +
  labs(x = 'Mass Arsenic Consumed (µg)',
       y = 'Mass Arsenic Excreted (µg)') + 
  theme_bw() + scale_x_continuous(expand = c(0.01,0)) + 
  annotate('text', 300, 800, label = lm_eqn(df = data.frame(x = chile$As.in_water.seafood, 
                                                            y = chile$As.out_creat)),
           parse = T, size = 3) + 
  labs(title = 'Water and seafood intake, creatinine-based excretion')

#MCMC simulations
#water only - only contributing spread from creatinine relationship

MC1 = lapply(1:1000, function(i){
  x_true = chile$WaterAS1yr2 * chile$water_intake 
  y_true = chile$UrinaryArsenic/(chile$UrinaryCreat * 10) * Ar.expand *#our creatinine concentrations are in mg/dL, hence multiplication by 10
    pred_total_creatinine(weight = chile$Wght, age = chile$Exp_Age, 
                          black=  0, female = chile$Sex - 1, sim.errors = T) 
  
  res = lm(y_true ~ x_true)
  data.frame(intcpt = coef(res)[1], slp = coef(res)[2], rsq = summary(res)$r.squared,
             resid.mean = mean(y_true-x_true,na.rm = T), 
             resid.med = median(y_true - x_true,na.rm = T),
             resid.prop.pos = mean((y_true - x_true) > 0, na.rm = T))
})

MC1 = do.call(rbind,MC1)

#water and seafood - contributing spread from creatinine relationship and seafood arsenic distributions
MC2 = lapply(1:1000, function(i){
  x_true = chile$water_intake * chile$WaterAS1yr2 + fish.conc * chile$OZ_fish_seafood
  y_true = chile$UrinaryArsenic/(chile$UrinaryCreat * 10) * Ar.expand *#our creatinine concentrations are in mg/dL, hence multiplication by 10
    pred_total_creatinine(weight = chile$Wght, age = chile$Exp_Age, 
                          black=  0, female = chile$Sex - 1, sim.errors = T) 
  
  res = lm(y_true ~ x_true)
  data.frame(intcpt = coef(res)[1], slp = coef(res)[2], rsq = summary(res)$r.squared,
             resid.mean = mean(y_true-x_true,na.rm = T), 
             resid.med = median(y_true - x_true,na.rm = T),
             resid.prop.pos = mean((y_true - x_true) > 0, na.rm = T))
})

MC2 = do.call(rbind,MC2)

MC1$source = 'Chile: Water'
MC2$source = 'Chile: Water & Seafood'
grandMC = rbind(MC1, MC2)

#histograms of intercept, slope, r^2
library(gridExtra)

p1.a = ggplot(grandMC, aes(x = intcpt)) + geom_histogram() + theme_bw() + 
  geom_vline(xintercept = 0, lty = 2) + xlab('Intercept (µg)') + 
  facet_wrap(~source,ncol = 1)
p1.b = ggplot(grandMC, aes(x = slp)) + geom_histogram() + theme_bw() + 
  geom_vline(xintercept = 1, lty = 2) + xlab('Slope') + 
  facet_wrap(~source,ncol = 1)
p1.c = ggplot(grandMC, aes(x = rsq)) + geom_histogram() + 
  theme_bw() + xlab(expression(bold(r^2))) + 
  facet_wrap(~source,ncol = 1)

grid.arrange(p1.a,p1.b,p1.c,nrow = 1)

#2d density plots
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



#Chile specific quantities and functions for seafood
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
}
