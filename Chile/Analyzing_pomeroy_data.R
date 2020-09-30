library(ggplot2)
library(readr)
pomdat = read_csv('Pomeroy_table5.csv')

ggplot(pomdat, aes(x = Day, y = `Percent Initial Intake Excreted in Urine`)) +
  geom_smooth(aes(col = factor(Subject)), se = F) + geom_point(aes(col = factor(Subject))) + 
  geom_smooth(col = 'black')  + geom_hline(yintercept = 0)

ggplot(pomdat, aes(x = Day, y = `Percent Initial Intake Excreted in feces`)) +
  geom_smooth(aes(col = factor(Subject)), se = F) + geom_point(aes(col = factor(Subject))) + 
  geom_smooth(col = 'black') + geom_hline(yintercept = 0)

pomdat$frac_fec = pomdat$`Percent Initial Intake Excreted in feces` / (
  pomdat$`Percent Initial Intake Excreted in Urine` + 
    pomdat$`Percent Initial Intake Excreted in feces`
)

ggplot(pomdat, aes(x = Day, y = frac_fec)) +
  geom_smooth(aes(col = factor(Subject)), se = F) + geom_point(aes(col = factor(Subject))) + 
  geom_smooth(col = 'black')

pomdat$cum_frac_fec = pomdat$`Cumulative Feces Percent` / (
  pomdat$`Cumulative Urine Percent` + 
    pomdat$`Cumulative Feces Percent`
)

ggplot(pomdat, aes(x = Day, y = cum_frac_fec)) +
  geom_smooth(aes(col = factor(Subject)), se = F) + geom_point(aes(col = factor(Subject))) + 
  geom_smooth(col = 'black')

#Feces dynamics look like a lot of AS is eliminated right away in 1-2 movements, 
#then almost no more is excreted in feces. This is probably because the bulk of the 
#AS eliminated in feces was never absorbed to the blood stream.

ret_fun = function(t, y, parms){
  inrate = parms[1]
  xdot = c(
    .659 * inrate - .332 * y[1],
    .304 * inrate - .0728 * y[2],
    .037 * inrate - .018 * y[3],
    .332 * y[1] + .0728 * y[2] + .018 * y[3]
  )
  list(xdot)
}

library(deSolve)
out = ode(y = c(0,0,0,0), times = seq(0,100,.1), func = ret_fun, parms = 6)
plot(seq(0,100,.1),rowSums(out[,2:4]), type = 'l' ,xlab ='time', ylab = 'AS in system')
plot(seq(0,100,.1),
     apply(out,1, function(x) ret_fun(1, x[2:5], 0)[[1]][4]), 
     type = 'l' ,xlab ='time', ylab = 'AS excretion rate')


#now try variable intake

ret_fun = function(t, y, parms){
  set.seed(round(t))
  inrate = rlnorm(1)
  xdot = c(
    .659 * inrate - .332 * y[1],
    .304 * inrate - .0728 * y[2],
    .037 * inrate - .018 * y[3],
    .332 * y[1] + .0728 * y[2] + .018 * y[3]
  )
  list(xdot)
}

library(deSolve)
out = ode(y = c(0,0,0,0), times = seq(0,100,.1), func = ret_fun, parms = 6)
plot(seq(0,100,.1),rowSums(out[,2:4]), type = 'l' ,xlab ='time', ylab = 'AS in system')
plot(seq(0,100,.1),
     apply(out,1, function(x) ret_fun(1, x[2:5], 0)[[1]][4]), 
     type = 'l' ,xlab ='time', ylab = 'AS excretion rate')

inrates = sapply(seq(0,100,.1), function(t) {set.seed(round(t)); rlnorm(1)})
plot(seq(0,100,.1), inrates, type = 'l', xlab ='time', ylab = 'AS intake rate')
lines(seq(0,100,.1),
     apply(out,1, function(x) ret_fun(1, x[2:5], 0)[[1]][4]), 
     col = 'red')

abline(h = mean(inrates), col = 'orange')

plot(seq(0,100,.1),
     apply(out,1, function(x) ret_fun(1, x[2:5], 0)[[1]][4]) - mean(inrates), 
     type = 'l' ,xlab ='time', ylab = 'AS excretion rate - mean intake rate')

plot(density((apply(out,1, function(x) ret_fun(1, x[2:5], 0)[[1]][4]) - mean(inrates))))
