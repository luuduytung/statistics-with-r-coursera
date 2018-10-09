
library(ggplot2)
library(dplyr)
library(knitr)
library(DT)
library(xtable)

load("brfss2013.RData")
names(brfss2013)

# sex_education_income
sex_edu_inc = data.frame(edu = as.factor(brfss2013$educa),inc=as.factor(brfss2013$income2),sex=as.factor(brfss2013$sex))
sex_edu_inc = sex_edu_inc %>% filter(!is.na(inc),!is.na(edu),!is.na(sex)) 

inc_twolevels = factor(rep(NA,length(sex_edu_inc$inc)),levels = c("Less than $25k","More than $25k"))
inc_twolevels[sex_edu_inc$inc == "Less than $75,000"|sex_edu_inc$inc =="$75,000 or more"
              |sex_edu_inc$inc == "Less than $50,000"|sex_edu_inc$inc == "Less than $35,000"]="More than $25k"
inc_twolevels[sex_edu_inc$inc == "Less than $25,000"|sex_edu_inc$inc == "Less than $20,000"
              |sex_edu_inc$inc == "Less than $15,000"|sex_edu_inc$inc == "Less than $10,000"]="Less than $25k"
sex_edu_inc = sex_edu_inc%>%mutate(inc_twolevels)

ggplot(sex_edu_inc, aes(x=sex, fill = inc_twolevels,group=inc_twolevels)) +
  geom_bar(position = 'fill') + facet_wrap( ~ edu, ncol=2)

pr = sex_edu_inc %>% group_by(edu,sex,inc_twolevels)%>%summarise(n = n()) %>% mutate(prop = n/sum(n))

# Sex_mentalHealth_phyHeath
men_phy_sex = data.frame(sex = as.factor(brfss2013$sex),men=brfss2013$menthlth,phy=brfss2013$physhlth)
men_phy_sex = men_phy_sex %>% filter(!is.na(sex),!is.na(men),!is.na(phy)) 
gr = men_phy_sex %>% group_by(phy,sex) %>% summarise(mean_men = mean(men))
ggplot(gr,aes(x=phy,y=mean_men,colour=sex))+  geom_point(alpha=0.3) + facet_wrap( ~ sex, ncol=2) 

male = gr%>%filter(sex=="Male")
female = gr%>%filter(sex=="Female")
gr_print = data.frame(phy=male$phy,mean_men_male = male$mean_men,
                      mean_men_female=female$mean_men)

gr_fem

# Gym_shape
gym_wei_hei = data.frame(gym = brfss2013$exerany2,wei=as.numeric(brfss2013$weight2),hei=as.numeric(brfss2013$height3)/10)
gym_wei_hei = gym_wei_hei %>% filter(!is.na(gym),!is.na(wei),!is.na(hei)) 
gym_wei_hei = gym_wei_hei %>% mutate(bmi = 703*gym_wei_hei$wei/(gym_wei_hei$hei)^2)
body_cat = factor(rep(NA,length(bmi)),levels = c("Underweight","Normal","Overweight","Obese/Moderately Obese","Severely/Morbidly Obese"))
body_cat[gym_wei_hei$bmi<=18.5] = "Underweight"
body_cat[gym_wei_hei$bmi>18.5 & gym_wei_hei$bmi<=25] = "Normal"
body_cat[gym_wei_hei$bmi>25 & gym_wei_hei$bmi<=27] = "Overweight"
body_cat[gym_wei_hei$bmi>27 & gym_wei_hei$bmi<=35] = "Obese/Moderately Obese"
body_cat[gym_wei_hei$bmi>35] = "Severely/Morbidly Obese"
gym_wei_hei = gym_wei_hei %>% mutate(body_cat)
gym_wei_hei_summarise = gym_wei_hei %>% group_by(gym,body_cat)%>%summarise(n=n())%>%mutate(prop = n/sum(n))
print(data.frame(Gym = gym_wei_hei_summarise$gym,Body_cat = gym_wei_hei_summarise$body_cat
                 ,Proportion=gym_wei_hei_summarise$prop))

ggplot(gym_wei_hei,aes(x=gym,fill=body_cat))+geom_bar(position="fill")
