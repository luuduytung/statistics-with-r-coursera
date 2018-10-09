library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)

data(ames)
names(ames)

ggplot(data=ames,aes(x=area))+geom_histogram(binwidth = 250)
ames %>% summarise(mu = mean(area),pop_med=median(area),sigma=sd(area),pop_iqr=IQR(area),
                   pop_min=min(area),pop_max=max(area),pop_q1=quantile(area,0.25),pop_q3=quantile(area,0.75))

samp1 = ames %>% sample_n(size=50)

ggplot(data=samp1,aes(x=area))+geom_histogram(binwidth = 250)
samp1 %>% summarise(x_bar = mean(area))



