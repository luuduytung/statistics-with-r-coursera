library(statsr)
library(dplyr)
library(ggplot2)

data(kobe_basket)

#Q3
kobe_streak = calc_streak(kobe_basket$shot)
ggplot(data=kobe_streak,aes(x=length))+geom_histogram(binwidth = 1)

#Q4
coin_outcomes = c("heads","tails")
fair_sim_coin = sample(coin_outcomes,size = 100,replace = TRUE)
table(fair_sim_coin)

unfair_sim_coin = sample(coin_outcomes,size=100,replace=TRUE,prob=c(0.2,0.8))
table(unfair_sim_coin)

shot_outcomes = c("H","M")
sim_basket = sample(shot_outcomes,size=133,replace=TRUE,prob=c(0.45,0.55))
table(sim_basket)

sim_streak = calc_streak(sim_basket)
ggplot(data=sim_streak,aes(x=length))+geom_histogram(binwidth = 1)
