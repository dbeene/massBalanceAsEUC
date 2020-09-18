#Diaz paper estimation

wheat = c(150,340,400,300,100,220,500,
          100,120,470,340,100,200,100,
          220,200,200,130,200,200)

rice = c(120,120,300,120,0,0,300,
         120,120,120,120,0,120,
         120,0,200,120,0,120,0)

quinoa = rep(0,20)
quinoa[c(3:7,9:10,13,18)] = c(
  300,200,300,200,300,300,200,200,200
)

carrots = rep(0,20)
carrots[c(1,8:9,11,15:19)] = c(
  100,80,80,80,80,50,80,80,50
)

lettuce = rep(0,20)
lettuce[c(3:5,8:9,11,13:15,17:18)] = c(
  100,100,50,80,50,100,100,50,100,80,80
)

tomatoes = rep(0,20)
tomatoes[c(5:6,10,12:13,16,18)] = c(
  150,100,60,150,100,100,80
)

red.meat = rep(0,20)
red.meat[c(2,4:5,8:14,16:20)] = c(
  rep(100,4),150,150,100,200,150,rep(100,4),150,100
)

lamb = rep(0,20)
lamb[c(1:2,6,10:11,16,19)] = c(
  100,200,200,150,100,100,150
)

chicken = rep(0,20)
chicken[c(13,15,18)] = c(
  200,100,100
)

cAs = c(.11,.11,.24,.22,.19,.52,.12,
        .13,.72,.62,.15,.06,.81,.33,
        .65,.52,.14,1.02,.50,
        .08)

diet.df = data.frame(wheat,rice,quinoa,carrots,
                     lettuce,tomatoes, red.meat,lamb,chicken,cAs)

diet.df$tot.g = rowSums(diet.df[,-10])

library(nnls)
lm.estim = lm((cAs * tot.g)~wheat + rice + quinoa + carrots + lettuce + tomatoes + 
                red.meat + lamb + chicken -1, diet.df)
nnls.estim = nnls(as.matrix(diet.df[,1:9]), diet.df$cAs * diet.df$tot.g)




lm.estim2 = lm((cAs * tot.g)~I(wheat + rice + quinoa) + I(carrots + lettuce + tomatoes) + 
                I(red.meat + lamb + chicken) -1, diet.df)
nnls.estim2 = nnls(matrix(
  c(diet.df$wheat + diet.df$rice + diet.df$quinoa,
    diet.df$tomatoes+diet.df$lettuce+diet.df$carrots,
    diet.df$red.meat + diet.df$lamb+diet.df$chicken),ncol=3
  
), diet.df$cAs * diet.df$tot.g)


