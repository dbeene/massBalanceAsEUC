#principal components experiment

dietary.vars = grep('OZ|SER',names(chile.rf), value = T)

dietary.pcs = prcomp(UrAs.subset[,dietary.vars])

#devtools::install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(dietary.pcs)

dietary.vars2 = dietary.vars[c(1,5:9,11:12,14:15,20:21)]

lms.total = lapply(dietary.vars2, function(v){
  eval(parse(text = 
               paste(
                 'lm(UrinaryArsenic ~', v, ',data = chile)'
               )))
})

lms.DiMe = lapply(dietary.vars2, function(v){
  eval(parse(text = 
               paste(
                 'lm(UrinaryDiMeAr ~', v, ',data = chile)'
               )))
})

lms.In = lapply(dietary.vars2, function(v){
  eval(parse(text = 
               paste(
                 'lm(UrinaryInArsenic ~', v, ',data = chile)'
               )))
})

lms.AsB = lapply(dietary.vars2, function(v){
  eval(parse(text = 
               paste(
                 'lm(UrinaryAsBet ~', v, ',data = chile)'
               )))
})

pr.comps = prcomp(na.omit(chile[,dietary.vars2]))

lm.pr.tot = lm(chile$UrinaryArsenic[!is.na(chile$SER_alcohol)] ~ pr.comps$x[,1:8])

lm.pr.In = lm(chile$UrinaryInArsenic[!is.na(chile$SER_alcohol)] ~ pr.comps$x[,1:8])

lm.pr.DiMe = lm(chile$UrinaryDiMeAr[!is.na(chile$SER_alcohol)] ~ pr.comps$x[,1:8])

lm.pr.AsB = lm(chile$UrinaryAsBet[!is.na(chile$SER_alcohol)] ~ pr.comps$x[,1:8])

cormat = cor(na.omit(chile[,sort(dietary.vars2)]))
cormat[upper.tri(cormat,diag = T)] <- NA
corplot = na.omit(melt(cormat))

ggplot(corplot,aes(x = X1, y = X2, fill = value)) + geom_tile() + theme_bw() + 
  scale_fill_gradientn(colours = c('red','white','green'),values = c(-1,0,1)) + 
  theme(axis.text.x = element_text(angle = 45,vjust = 1,
                                   hjust = 1)) + labs(fill = 'correlation')


#figures in slide deck 6/18
ggplot(chile,aes(x = OZ_fish_seafood, y = UrinaryArsenic)) + 
  geom_point() + geom_smooth(method = 'lm', fill = rgb(0,0,1,.3)) + #geom_smooth(col = rgb(1,0,0,.01), fill = rgb(1,0,0,.001)) + 
  geom_text(x = 4, y = 300, 
            label = paste('Slope:', round(lms[[1]]$coefficients[2],3),
                          '\nSE:',round(summary(lms[[1]])$coefficients[2,2],3))) 

chile$sfbin = .bincode(chile$OZ_fish_seafood,quantile(chile$OZ_fish_seafood,seq(0,1,.25),na.rm = T),
                      include.lowest = T)

sfbinned = chile %>% filter(!is.na(sfbin)) %>% group_by(sfbin) %>% 
  dplyr::summarize(total = mean(UrinaryArsenic),total.u = quantile(UrinaryArsenic,0.975),
            total.l = quantile(UrinaryArsenic,0.025),
            dimear = mean(UrinaryDiMeAr), dimear.u = quantile(UrinaryDiMeAr,0.975),
            dimear.l = quantile(UrinaryDiMeAr,0.025),
            x = mean(OZ_fish_seafood))

ggplot(sfbinned, aes(x = x, y = total,ymax = total.u,ymin = total.l)) + 
  geom_errorbar() + geom_point() + stat_smooth(method = 'lm') + 
  labs(x = 'Mean Oz fish and seafood', y = 'Total Urinary Arsenic')

ggplot(chile,aes(x = OZ_fish_seafood, y = UrinaryDiMeAr)) + 
  geom_point() + geom_smooth(method = 'lm', fill = rgb(0,0,1,.3)) + #geom_smooth(col = rgb(1,0,0,.01), fill = rgb(1,0,0,.001)) + 
  geom_text(x = 4, y = 300, 
            label = paste('Slope:', round(lms.DiMe[[1]]$coefficients[2],3),
                          '\nSE:',round(summary(lms.DiMe[[1]])$coefficients[2,2],3))) 

plot(lms.DiMe[[1]])

ggplot(sfbinned, aes(x = x, y = dimear,ymax = dimear.u,ymin = dimear.l)) + 
  geom_errorbar() + geom_point() + stat_smooth(method = 'lm') + 
  labs(x = 'Mean Oz fish and seafood', y = 'Total Urinary Dimethyl Arsenic')


ggplot(chile,aes(x = OZ_lean_beef_pork_lamb, y = UrinaryArsenic)) + 
  geom_point() + geom_smooth(method = 'lm', fill = rgb(0,0,1,.3)) + #geom_smooth(col = rgb(1,0,0,.01), fill = rgb(1,0,0,.001)) + 
  geom_text(x = 4, y = 300, 
            label = paste('Slope:', round(lms[[2]]$coefficients[2],3),
                          '\nSE:',round(summary(lms[[2]])$coefficients[2,2],3))) 
