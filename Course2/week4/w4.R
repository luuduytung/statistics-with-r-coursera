library(statsr)
library(dplyr)
library(ggplot2)

data(atheism)

head(atheism)

us12 = atheism %>% filter(nationality=="United States",year=="2012")
us12a = us12 %>% filter(response=="atheist")
100*dim(us12a)[1]/dim(us12)[1]
us12a

inference(y = response, data = us12, statistic = "proportion", type = "ci", method = "theoretical", success = "atheist")

d = data.frame(p=seq(0,1,0.01))
n=1000

d = d %>% mutate(me = 1.96*sqrt(p*(1-p)/n))
ggplot(data=d,aes(x=p,y=me)) + geom_line()

spain = atheism %>% filter(nationality == "Spain")
inference(y = response, data = spain, statistic = "proportion", group=year, type = "ht", method = "theoretical", success = "atheist")
?inference
inference(y=response, spain$year, statistic = "proportion", type = "ci", method = "theoretical", success = "atheist")


#Q13
(1.96*0.5/0.01)^2
