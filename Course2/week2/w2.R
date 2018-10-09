set.seed(9102015)    
library(statsr)
library(dplyr)
library(ggplot2)

data(ames)
n = 60
samp = sample_n(ames,n)
# Q1
ggplot(data=samp,aes(x=area))+geom_histogram(binwidth =50)

ggplot(data=ames,aes(x=area))+geom_histogram(binwidth =50)

z_star_95 = qnorm(0.975)
samp %>% summarise(lower = mean(area)-z_star_95*sd(area)/sqrt(n),
                   upper = mean(area)+z_star_95*sd(area)/sqrt(n))


params = ames %>% summarise(mu=mean(area))
# Q4

ci = ames %>% rep_sample_n(size=n,reps=50,replace=TRUE)%>%summarise(lower = mean(area)-z_star_95*sd(area)/sqrt(n),
                                                                    upper = mean(area)+z_star_95*sd(area)/sqrt(n))
ci %>% slice(1:5)
ci = ci %>% mutate(capture_mu = ifelse(lower<params$mu & upper>params$mu,"yes","no"))
ci %>% slice(1:5)

ci_data = data.frame(ci_id = c(1:50,1:50),
                     ci_bounds = c(ci$lower, ci$upper),
                                   capture_mu = c(ci$capture_mu,ci$capture_mu))
ggplot(data=ci_data,aes(x=ci_bounds,y=ci_id,color=capture_mu,group=ci_id))+geom_point()+geom_line() + geom_vline(xintercept=params$mu)

# Q5
qnorm(0.995)

# Q6