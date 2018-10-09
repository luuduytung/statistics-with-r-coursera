library(statsr)
library(dplyr)
library(ggplot2)

data(nc)
str(nc)

summary(nc$gained)
ggplot(data = nc,aes(x=habit,y=weight))+geom_boxplot()
nc %>% group_by(habit) %>% summarise(x_bar = mean(weight))
nc %>% group_by(habit) %>% summarise(nb = n())

inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")
inference(y=weight, x=habit,data=nc,statistic="mean",type="ci",method="theoretical")

inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ci", 
          method = "theoretical", order = c("smoker","nonsmoker"))

nc %>% group_by(mature) %>% summarise(x_max = max(mage),x_min=min(mage))
