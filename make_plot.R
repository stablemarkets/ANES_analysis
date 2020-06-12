library(dplyr)
library(tidyr)
library(ggplot2)
library(quantreg)

d = read.csv("anes_pilot_2018.csv")

dd = d %>% 
  select( caseid, race, lcself, ftblack, ftwhite, fthisp, ftasian, weight ) %>%
  filter(race %in% c(1,2,3,4), 
         ## drop non-responses (bad to do, but good for a quick look)
         lcself!=-7, ftblack != -7 , ftwhite != -7 , fthisp != -7,  ftasian != -7 ) %>%
  ## I assume this is how we compute the score??
  mutate( in_group = ifelse( race==1, ftwhite, 
                             ifelse(race==2, ftblack, 
                                    ifelse(race==3, fthisp, 
                                           ifelse(race==4, ftasian, NA)))), 
          out_group = ifelse( race==1, (ftblack+fthisp+ftasian)/3, 
                              ifelse(race==2, (ftwhite+fthisp+ftasian)/3, 
                                     ifelse(race==3, (ftwhite+ftblack+ftasian)/3, 
                                            ifelse(race==4, (ftwhite+ftblack+fthisp)/3, NA ) ) ) ),
          score = in_group - out_group,
          `Political Lean` = factor(ifelse(lcself %in% c(1,2,3), 'liberal', 'non-liberal' )) )

dd$race = factor(ifelse(dd$race==1, 'white', 
                        ifelse(dd$race ==2, 'black', 
                               ifelse(dd$race==3, 'hispanic', 'asian'))),
                 levels = c('black', 'hispanic','asian', 'white') )

## get count of sample sizes
table(dd$race)
table(dd$race, dd$`Political Lean`)

## summary table - matches Tablet Mag results fairly closely... that's good.
## I wonder why their sample size counts differ...
## but wow look at that variability (huge SD for whites )
## difference in medians is much narrower -2, 2
dd %>%
  group_by(race, `Political Lean`) %>%
  summarize(m_score=mean(score), 
            sd_score = sd(score),
            med_score = median(score), 
            n_weight = sum(weight),
            n = n() )

## ... but lets look at the variability more closely via boxplots
ggplot(dd, aes(x = race, y = score, fill=`Political Lean`) ) + 
  geom_boxplot( position = position_dodge()) + 
  geom_abline( intercept=0, slope=0, linetype='dashed', col='red', show.legend = F) + 
  theme_bw()
ggsave(filename = 'improved_plot.png')

### Okay so here's the claim: 
## "Remarkably, white liberals were the only subgroup exhibiting a 
## pro-outgroup bias—meaning white liberals were more favorable toward nonwhites
## and are the only group to show this preference for group other than 
## their own."

## Conclusion: well...no. First, "asian" liberals also meet this
## standard with net score of 0. But this is dubious...we only havae 20 liberal
## asians in our sample so let's not draw any conclusions. 
## Second, the variability is so huge. Look at the whiskers on these boxplots, 
## look at all the spread. If we were to take a different survey, we may have 
## gotten different results - just a ton of variability her. with a lot of  
## outliers (especially among whites). So "means" aren't even a good measure 
## of central tendency here. What would be better are medians. And difference 
## in median scores are much narrow. 
## So i don't think this data really support the claim. 


## we need to account for survey sampling weights for population-level
## inference. E.g. if we under-sampled asians by say 20%, each asian's score 
## should really be score*1.2, weight 1.2 being the weight. Lucily, we're 
## given the survey weights. 

ggplot(dd, aes(x = race, y = score*weight, fill=liberal) ) + 
  geom_boxplot( position = position_dodge()) + 
  geom_abline( intercept=0, slope=0, linetype='dashed', col='red', show.legend = F) + 
  ylim(-100,100) + 
  theme_bw()

## okay...still a ton of variation...even more variation on liberal asians, 
## but not much different.
