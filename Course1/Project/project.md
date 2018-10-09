Exploring the BRFSS data
================

Setup
-----

### Load packages

``` r
library(ggplot2)
library(dplyr)
options(dplyr.print_max=1000)
```

### Load data

``` r
load("brfss2013.RData")
```

------------------------------------------------------------------------

Part 1: Data
------------

The observations in the sample brfss2013 were collected from the non-institutionalized adult population (18 years of age and older) in the United States using a cluster sampling. The clusters are the "states" of US. In each "state", the survey was conducted to the randomly selected adults in households (via landline telephones) and in private residences or college housings (via cellphones).

**Biases**: There are two sources of biases in this survey sampling.
<ol>
<li>
Convenience sample: The sampling covers only adults who live in households, private residences and college housings, and have telephones.
</li>
<li>
Non-reponse: Since the survey was conducted via telephone, some people were unable or unwilling to parpicipate.
</li>
</ol>
**Generalizability**: Thus, results can be generalizable to the population of adults who live in households, private residences and college housings, and have telephones.

**Causuality**: Since the study used no random assignment, it can not make causual conclusions.

------------------------------------------------------------------------

Part 2: Research questions
--------------------------

**Research quesion 1:**

**Research quesion 2:**

**Research quesion 3:**

------------------------------------------------------------------------

Part 3: Exploratory data analysis
---------------------------------

NOTE: Insert code chunks as needed by clicking on the "Insert a new code chunk" button (green button with orange arrow) above. Make sure that your code is visible in the project you submit. Delete this note when before you submit your work.

**Research quesion 1:**

``` r
inc_twolevels = factor(rep(NA,length(brfss2013$inc)),levels = c("Less than $25k","More than $25k"))
inc_twolevels[brfss2013$inc == "Less than $75,000"|brfss2013$inc =="$75,000 or more"
              |brfss2013$inc == "Less than $50,000"|brfss2013$inc == "Less than $35,000"]="More than $25k"
inc_twolevels[brfss2013$inc == "Less than $25,000"|brfss2013$inc == "Less than $20,000"
              |brfss2013$inc == "Less than $15,000"|brfss2013$inc == "Less than $10,000"]="Less than $25k"

men_phy_sex = data.frame(inc = inc_twolevels,men=brfss2013$menthlth,phy=brfss2013$physhlth)
men_phy_sex = men_phy_sex %>% filter(!is.na(inc),!is.na(men),!is.na(phy)) 

gr = men_phy_sex %>% group_by(phy,inc) %>% summarise(mean_men = mean(men))
poor = gr%>%filter(inc=="Less than $25k")
rich = gr%>%filter(inc=="More than $25k")
data.frame(phy=poor$phy,mean_men_poor = poor$mean_men,
                      mean_men_rich=rich$mean_men)
```

    ##    phy mean_men_poor mean_men_rich
    ## 1    0      2.673475      1.604975
    ## 2    1      3.872789      2.277663
    ## 3    2      4.405486      2.746049
    ## 4    3      5.168433      3.161844
    ## 5    4      5.586425      3.477401
    ## 6    5      6.085375      3.811676
    ## 7    6      6.220162      4.417947
    ## 8    7      6.506555      3.608187
    ## 9    8      6.164557      4.017520
    ## 10   9      6.924138      2.745098
    ## 11  10      8.051177      4.979154
    ## 12  11      8.442308      3.487805
    ## 13  12      8.074766      5.940774
    ## 14  13     11.265625      5.864407
    ## 15  14      7.540931      3.903308
    ## 16  15      9.951850      6.250452
    ## 17  16     12.445378      6.103093
    ## 18  17     11.594937      6.305085
    ## 19  18     12.412903      7.844262
    ## 20  19     15.285714      8.562500
    ## 21  20     11.531668      8.172231
    ## 22  21      9.025751      4.968174
    ## 23  22     11.913043     12.214286
    ## 24  23     14.058824      9.473684
    ## 25  24     12.383562     10.155556
    ## 26  25     12.928094      9.119612
    ## 27  26     14.900000      8.536585
    ## 28  27     17.261905     10.592593
    ## 29  28     14.040733      8.274924
    ## 30  29     12.941441     11.406667
    ## 31  30     12.359618      8.146053

``` r
ggplot(gr,aes(x=phy,y=mean_men,colour=inc))+  geom_point(alpha=0.3) + facet_wrap( ~ inc, ncol=2) 
```

![](project_files/figure-markdown_github/unnamed-chunk-2-1.png)

**Research quesion 2:**

``` r
sex_edu_inc = data.frame(edu = as.factor(brfss2013$educa),inc=inc_twolevels,sex=as.factor(brfss2013$sex))
sex_edu_inc = sex_edu_inc %>% filter(!is.na(inc),!is.na(edu),!is.na(sex)) 

ggplot(sex_edu_inc, aes(x=sex, fill = inc,group=inc)) +
  geom_bar(position = 'fill') + facet_wrap( ~ edu, ncol=2)
```

![](project_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
summarise_sex_edu_inc = sex_edu_inc %>% group_by(edu,sex,inc)%>%summarise(n = n()) %>% mutate(prop = n/sum(n))
print(data.frame(Edu = summarise_sex_edu_inc$edu,Sex = summarise_sex_edu_inc$sex,
                 Income = summarise_sex_edu_inc$inc,
                 Proportion = summarise_sex_edu_inc$prop))
```

    ##                                                             Edu    Sex
    ## 1                    Never attended school or only kindergarten   Male
    ## 2                    Never attended school or only kindergarten   Male
    ## 3                    Never attended school or only kindergarten Female
    ## 4                    Never attended school or only kindergarten Female
    ## 5                               Grades 1 through 8 (Elementary)   Male
    ## 6                               Grades 1 through 8 (Elementary)   Male
    ## 7                               Grades 1 through 8 (Elementary) Female
    ## 8                               Grades 1 through 8 (Elementary) Female
    ## 9                         Grades 9 though 11 (Some high school)   Male
    ## 10                        Grades 9 though 11 (Some high school)   Male
    ## 11                        Grades 9 though 11 (Some high school) Female
    ## 12                        Grades 9 though 11 (Some high school) Female
    ## 13                       Grade 12 or GED (High school graduate)   Male
    ## 14                       Grade 12 or GED (High school graduate)   Male
    ## 15                       Grade 12 or GED (High school graduate) Female
    ## 16                       Grade 12 or GED (High school graduate) Female
    ## 17 College 1 year to 3 years (Some college or technical school)   Male
    ## 18 College 1 year to 3 years (Some college or technical school)   Male
    ## 19 College 1 year to 3 years (Some college or technical school) Female
    ## 20 College 1 year to 3 years (Some college or technical school) Female
    ## 21                   College 4 years or more (College graduate)   Male
    ## 22                   College 4 years or more (College graduate)   Male
    ## 23                   College 4 years or more (College graduate) Female
    ## 24                   College 4 years or more (College graduate) Female
    ##            Income Proportion
    ## 1  Less than $25k  0.6872038
    ## 2  More than $25k  0.3127962
    ## 3  Less than $25k  0.7224490
    ## 4  More than $25k  0.2775510
    ## 5  Less than $25k  0.7209011
    ## 6  More than $25k  0.2790989
    ## 7  Less than $25k  0.8370560
    ## 8  More than $25k  0.1629440
    ## 9  Less than $25k  0.6047216
    ## 10 More than $25k  0.3952784
    ## 11 Less than $25k  0.7599660
    ## 12 More than $25k  0.2400340
    ## 13 Less than $25k  0.3582687
    ## 14 More than $25k  0.6417313
    ## 15 Less than $25k  0.4939844
    ## 16 More than $25k  0.5060156
    ## 17 Less than $25k  0.2502899
    ## 18 More than $25k  0.7497101
    ## 19 Less than $25k  0.3394589
    ## 20 More than $25k  0.6605411
    ## 21 Less than $25k  0.1006390
    ## 22 More than $25k  0.8993610
    ## 23 Less than $25k  0.1270759
    ## 24 More than $25k  0.8729241

**Research quesion 3:**

``` r
gym_wei_hei = data.frame(gym = brfss2013$exerany2,wei=as.numeric(brfss2013$weight2),hei=as.numeric(brfss2013$height3)/10)
gym_wei_hei = gym_wei_hei %>% filter(!is.na(gym),!is.na(wei),!is.na(hei)) 
gym_wei_hei = gym_wei_hei %>% mutate(bmi = 703*gym_wei_hei$wei/(gym_wei_hei$hei)^2)
body_cat = factor(rep(NA,length(gym_wei_hei$bmi)),levels = c("Underweight","Normal","Overweight","Obese/Moderately Obese","Severely/Morbidly Obese"))
body_cat[gym_wei_hei$bmi<=18.5] = "Underweight"
body_cat[gym_wei_hei$bmi>18.5 & gym_wei_hei$bmi<=25] = "Normal"
body_cat[gym_wei_hei$bmi>25 & gym_wei_hei$bmi<=27] = "Overweight"
body_cat[gym_wei_hei$bmi>27 & gym_wei_hei$bmi<=35] = "Obese/Moderately Obese"
body_cat[gym_wei_hei$bmi>35] = "Severely/Morbidly Obese"
gym_wei_hei = gym_wei_hei %>% mutate(body_cat)
gym_wei_hei_summarise = gym_wei_hei %>% group_by(gym,body_cat)%>%summarise(n=n())%>%mutate(prop = n/sum(n))
print(data.frame(Gym = gym_wei_hei_summarise$gym,Body_cat = gym_wei_hei_summarise$body_cat
                 ,Proportion=gym_wei_hei_summarise$prop))
```

    ##    Gym                Body_cat Proportion
    ## 1  Yes             Underweight 0.49730936
    ## 2  Yes                  Normal 0.23631232
    ## 3  Yes              Overweight 0.05320282
    ## 4  Yes  Obese/Moderately Obese 0.13276880
    ## 5  Yes Severely/Morbidly Obese 0.08040671
    ## 6   No             Underweight 0.42683401
    ## 7   No                  Normal 0.21659919
    ## 8   No              Overweight 0.05329555
    ## 9   No  Obese/Moderately Obese 0.16081781
    ## 10  No Severely/Morbidly Obese 0.14245344

``` r
ggplot(gym_wei_hei,aes(x=gym,fill=body_cat))+geom_bar(position="fill")
```

![](project_files/figure-markdown_github/unnamed-chunk-4-1.png)
