library(statsr)
library(dplyr)
library(ggplot2)

data(nycflights)
str(nycflights)
names(nycflights)

ggplot(data = nycflights, aes(x=dep_delay))+geom_histogram(binwidth=40)

rdu_flights = nycflights %>% filter(dest=="RDU");
ggplot(data = rdu_flights,aes(x=dep_delay))+geom_histogram(binwidth=40)

rdu_flights %>% summarise(mean_dd = mean(dep_delay),sd_dd=sd(dep_delay),n=n())

sfo_feb_flights = nycflights %>% filter(dest=="SFO", month==2)
# , <=> and, | <=> or

#Q1
sfo_feb_flights %>% summarise(n=n())

#Q2
ggplot(data=sfo_feb_flights,aes(x=arr_delay))+geom_histogram(binwidth = 10)

rdu_flights %>% group_by(origin) %>% summarise(mean_dd = mean(dep_delay),sd_dd = sd(dep_delay),n=n())

#Q3
sfo_feb_flights %>% group_by(carrier) %>% summarise(ad_median = median(arr_delay),ad_iqr = IQR(arr_delay))

#Q4
nycflights %>% group_by(month) %>% summarise(dd_mean=mean(dep_delay)) %>% arrange(desc(dd_mean))

#Q5
nycflights %>% group_by(month) %>% summarise(dd_median = median(dep_delay))%>%arrange(desc(dd_median))

#Q6

#Q7
nycflights = nycflights %>% mutate(dep_type=ifelse(dep_delay<5,"on time","delayed"))
nycflights %>% group_by(origin) %>% summarise(dd_rate = sum(dep_type == "on time")/n())%>%arrange(desc(dd_rate))
names(nycflights)

#Q8
ggplot(data=nycflights,aes(x=origin,fill=dep_type))+geom_bar()
nycflights = nycflights %>% mutate(avg_speed=distance/(air_time/60))
nycflights %>% select(tailnum,avg_speed)%>%arrange(desc(avg_speed))

#Q9
ggplot(data=nycflights,aes(x=distance,y=avg_speed)) + geom_point()

#Q10
nycflights = nycflights %>% mutate(arr_type = ifelse(arr_delay<=0,"on time","delayed"))
nycflights %>% filter(dep_type=="delayed") %>% summarise(prop_dep_delayed_arr_ontime = sum(arr_type=="on time")/n())
