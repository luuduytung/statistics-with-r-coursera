library(dplyr)
library(ggplot2)
library(statsr)

# Data arbuthnot

data(arbuthnot)
arbuthnot
dim(arbuthnot)
names(arbuthnot)
arbuthnot$girls

ggplot(data=arbuthnot,aes(x = year,y=girls))+geom_point()

arbuthnot$girls+arbuthnot$boys

arbuthnot$total = arbuthnot$girls+arbuthnot$boys
arbuthnot2 = arbuthnot %>% mutate(total2=boys+girls)
arbuthnot3 = mutate(arbuthnot,total2=boys+girls)

ggplot(data=arbuthnot,aes(x=year,y=total)) + geom_line() + geom_point()

prop_boys = arbuthnot$boys/arbuthnot$total;
ggplot(data=arbuthnot,aes(x=year,y=prop_boys)) + geom_line() + geom_point()

arbuthnot = arbuthnot %>% mutate(more_boys = boys>girls)



# Data present



