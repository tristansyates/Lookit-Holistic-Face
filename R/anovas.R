# R-script for running various ANOVA analyses for Holistic Face project 

rm(list=ls()) #clear the environment

library(tidyverse)
library(dplyr)
library(rstatix)
library(ez)
library(lme4)
library(car)
library(compute.es)

################################################
################################################
# Experiment 1 

### RT Data 
# load the data
rt_data <-read.csv('rt_data_dict_cohorts_1_2.csv')
head(rt_data)
rt_data$sub <- factor(rt_data$sub)

# make age a categorical variable
rt_data$age <- factor(floor(rt_data$age))
rt_data %>% group_by(age) %>% tally()/4 # double check it worked

# group gender into male vs non-male
levels(rt_data$gender) # f, m, o
levels(rt_data$gender) <- c('non-m','m','non-m')
levels(rt_data$gender) # non-m, m

# What is the average RT?
mean(rt_data$time_screen)
median(rt_data$time_screen)
min(rt_data$time_screen)
max(rt_data$time_screen)

## Critical same trials 
my_anova = ezANOVA(
  data = filter(rt_data,condition=="same") #
  , dv = .(time_screen)
  , wid = .(sub)
  , within = .(alignment)
  , between = .(age,gender),
)
print(my_anova)

## Filler different trials 
my_anova = ezANOVA(
  data = filter(rt_data,condition=="different") #rt_data
  , dv = .(time_screen)
  , wid = .(sub)
  , within = .(alignment)#.(alignment,condition)
  , between = .(age,gender)
)
print(my_anova)

### Accuracy data
accuracy_data <- read.csv('accuracy_data_dict_cohorts_1_2.csv')
head(accuracy_data)
accuracy_data$sub <- factor(accuracy_data$sub)

# make age a categorical variable
accuracy_data$age <- factor(floor(accuracy_data$age))
accuracy_data %>% group_by(age) %>% tally()/4 # double check it worked

# group gender into male vs non-male
levels(accuracy_data$gender) # f, m, o
levels(accuracy_data$gender) <- c('non-m','m','non-m')
levels(accuracy_data$gender) # non-m, m

## Same trials
my_anova = ezANOVA(
  data = filter(accuracy_data,condition=="same")
  , dv = .(accuracy)
  , wid = .(sub)
  , within = .(alignment)#.(alignment,condition)
  , between = .(age, gender)
)
print(my_anova)

# follow-ups!! 
# How does accuracy (over all trials) differ between ages 
temp_acc_data<- accuracy_data %>%
  filter(condition=="same") %>%
  group_by(sub) %>%
  summarise(accuracy=mean(accuracy),age=mean(as.numeric(as.character(age))))
temp_acc_data

t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 5)$accuracy)
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("4", "5")), var.equal = FALSE)

t.test(filter(temp_acc_data, age == 5)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("5", "6")), var.equal = FALSE)

t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("4", "6")), var.equal = FALSE)

# pairwise t-tests grouped by subject
accuracy_data %>%
  filter(age==4,condition=="same")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 4,condition=="same",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 4,condition=="same",alignment=="misaligned")$accuracy,paired = TRUE)
test
test$p.value*3 # correct the pval
accuracy_data  %>%
  filter(age==4,condition=="same")%>%
  cohens_d(accuracy ~ alignment,paired=TRUE)


accuracy_data %>%
  filter(age==5,condition=="same")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 5,condition=="same",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 5,condition=="same",alignment=="misaligned")$accuracy,paired = TRUE)
test
test$p.value*3 # correct the pval
accuracy_data  %>%
  filter(age==5,condition=="same")%>%
  cohens_d(accuracy ~ alignment,paired=TRUE)

accuracy_data %>%
  filter(age==6,condition=="same")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 6,condition=="same",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 6,condition=="same",alignment=="misaligned")$accuracy,paired = TRUE)
test
test$p.value*3 # correct the pval
accuracy_data  %>%
  filter(age==6,condition=="same")%>%
  cohens_d(accuracy ~ alignment,paired=TRUE)

## Different trials
my_anova = ezANOVA(
  data = filter(accuracy_data,condition=="different")
  , dv = .(accuracy)
  , wid = .(sub)
  , within = .(alignment)#.(alignment,condition)
  , between = .(age, gender)
)
print(my_anova)
my_anova$ANOVA$p[1] # age
my_anova$ANOVA$p[2] # gender
my_anova$ANOVA$p[3] # alignment

# follow up tests!!
# How does accuracy (over all trials) differ between ages 
temp_acc_data<- accuracy_data %>%
  filter(condition=="different") %>%
  group_by(sub) %>%
  summarise(accuracy=mean(accuracy),age=mean(as.numeric(as.character(age))))

t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 5)$accuracy)
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("4", "5")), var.equal = FALSE)

t.test(filter(temp_acc_data, age == 5)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("5", "6")), var.equal = FALSE)

t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("4", "6")), var.equal = FALSE)


# pairwise t-tests grouped by subject
accuracy_data %>%
  filter(age==4,condition=="different")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 4,condition=="different",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 4,condition=="different",alignment=="misaligned")$accuracy,paired = TRUE)
test
test$p.value*3 # correct the pval
accuracy_data  %>%
  filter(age==4,condition=="different")%>%
  cohens_d(accuracy ~ alignment,paired=TRUE)

# pairwise t-tests grouped by subject
accuracy_data %>%
  filter(age==5,condition=="different")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 5,condition=="different",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 5,condition=="different",alignment=="misaligned")$accuracy,paired = TRUE)
test
test$p.value*3 # correct the pval
accuracy_data  %>%
  filter(age==5,condition=="different")%>%
  cohens_d(accuracy ~ alignment,paired=TRUE)

# pairwise t-tests grouped by subject
accuracy_data %>%
  filter(age==6,condition=="different")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 6,condition=="different",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 6,condition=="different",alignment=="misaligned")$accuracy,paired = TRUE)
test
test$p.value*3 # correct the pval
accuracy_data  %>%
  filter(age==6,condition=="different")%>%
  cohens_d(accuracy ~ alignment,paired=TRUE)

################################################
################################################
# Experiment 2 rt analysis 
rt_data_part2 <- read.csv('rt_data_dict_part2.csv')
head(rt_data_part2)
rt_data_part2$sub <- factor(rt_data_part2$sub)

# make age a categorical variable
rt_data_part2$age <- factor(floor(rt_data_part2$age))
rt_data_part2 %>%
  group_by(age) %>%
  tally()/4

# make all the 7 year 6 for group purposes
rt_data_part2$age[rt_data_part2$age==7] <- 6

# now view it 
rt_data_part2 %>%
  group_by(age) %>%
  tally()/4

# group gender into male vs non-male
levels(rt_data_part2$gender) # f, F, m, na
levels(rt_data_part2$gender) <- c('non-m','non-m','m','non-m')
levels(rt_data_part2$gender) # non-m, m

# now view it 
rt_data_part2 %>%
  group_by(gender) %>% tally()/4

# What is the average RT?
mean(rt_data_part2$time_screen)
min(rt_data_part2$time_screen)
max(rt_data_part2$time_screen)
sd(rt_data_part2$time_screen)

# how about the same trials ? 
my_anova = ezANOVA(
  data = filter(rt_data_part2,condition=="same")
  , dv = .(time_screen)
  , wid = .(sub)
  , within = .(alignment)#,condition)
  , between = .(age, gender)
)
print(my_anova)

my_anova = ezANOVA(
  data = filter(rt_data_part2,condition=="different")#rt_data_part2
  , dv = .(time_screen)
  , wid = .(sub)
  , within = .(alignment)#,condition)
  , between = .(age,gender)
)
print(my_anova)

# Follow up tests
temp_acc_data<- rt_data_part2 %>%
  filter(condition=="different") %>%
  group_by(sub,alignment) %>%
  summarise(time_screen=mean(time_screen),age=mean(as.numeric(as.character(age))))
temp_acc_data

mean(filter(temp_acc_data, age == 6,alignment == 'misaligned')$time_screen)

t.test(filter(temp_acc_data, age == 4,alignment == 'aligned')$time_screen,
       filter(temp_acc_data, age == 4,alignment == 'misaligned')$time_screen,paired=TRUE)

rt_data_part2  %>%
  filter(age==4,condition=="different")%>%
  cohens_d(time_screen ~ alignment,paired=TRUE)

t.test(filter(temp_acc_data, age == 5,alignment == 'aligned')$time_screen,
       filter(temp_acc_data, age == 5,alignment == 'misaligned')$time_screen,paired=TRUE)

rt_data_part2  %>%
  filter(age==5,condition=="different")%>%
  cohens_d(time_screen ~ alignment,paired=TRUE)

t.test(filter(temp_acc_data, age == 6,alignment == 'aligned')$time_screen,
       filter(temp_acc_data, age == 6,alignment == 'misaligned')$time_screen,paired=TRUE)

rt_data_part2  %>%
  filter(age==6,condition=="different")%>%
  cohens_d(time_screen ~ alignment,paired=TRUE)


# Experiment 2 main analysis 
accuracy_data_part2 <- read.csv('accuracy_data_dict_part2.csv')
head(accuracy_data_part2)
accuracy_data_part2$sub <- factor(accuracy_data_part2$sub)

# make age a categorical variable
accuracy_data_part2$age <- factor(floor(accuracy_data_part2$age))
accuracy_data_part2 %>%
  group_by(age) %>%
  tally()/4

# make all the 7 year 6 for group purposes
accuracy_data_part2$age[accuracy_data_part2$age==7] <- 6

# now view it 
accuracy_data_part2 %>%
  group_by(age) %>%
  tally()/4

# group gender into male vs non-male
levels(accuracy_data_part2$gender) # f, F, m, na
levels(accuracy_data_part2$gender) <- c('non-m','non-m','m','non-m')
levels(accuracy_data_part2$gender) # non-m, m

# now view it 
accuracy_data_part2 %>%
  group_by(gender) %>% tally()/4

# let's check out the critical same trials! 
my_anova = ezANOVA(
  data = filter(accuracy_data_part2,condition=="same")#accuracy_data_part2
  , dv = .(accuracy)
  , wid = .(sub)
  , within = .(alignment)#,condition)
  , between = .(age,gender)
)
print(my_anova)

# follow up tests!!
# How does accuracy (over all trials) differ between ages 
temp_acc_data<- accuracy_data_part2 %>%
  filter(condition=="same") %>%
  group_by(sub) %>%
  summarise(accuracy=mean(accuracy),age=mean(as.numeric(as.character(age))))

t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 5)$accuracy)
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("4", "5")), var.equal = FALSE)

t.test(filter(temp_acc_data, age == 5)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("5", "6")), var.equal = FALSE)

t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("4", "6")), var.equal = FALSE)

# how about the different trials?
my_anova = ezANOVA(
  data = filter(accuracy_data_part2,condition=="different")#accuracy_data_part2
  , dv = .(accuracy)
  , wid = .(sub)
  , within = .(alignment)#,condition)
  , between = .(age, gender,cohort)
)
print(my_anova)

# How does accuracy (over all trials) differ between ages 
temp_acc_data<- accuracy_data_part2 %>%
  filter(condition=="different") %>%
  group_by(sub) %>%
  summarise(accuracy=mean(accuracy),age=mean(as.numeric(as.character(age))))

test<-t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 5)$accuracy)
test
test$p.value*3
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("4", "5")), var.equal = FALSE)
t.test(filter(temp_acc_data, age == 5)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("5", "6")), var.equal = FALSE)
test<-t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
test
test$p.value*3
cohens_d(temp_acc_data,accuracy ~ age,comparisons=list(c("4", "6")), var.equal = FALSE)


###################################
########## Experiment 1 -- Questionnaire Data
covid_questions<-read.csv('coded_covid_questionnaire.csv')

# group gender into male vs non-male
levels(covid_questions$gender) # f, m, o
levels(covid_questions$gender) <- c('non-m','m','non-m')
levels(covid_questions$gender) # non-m, m

# make age a categorical variable
covid_questions$ages <- factor(floor(covid_questions$ages))
covid_questions %>% group_by(ages) %>% tally() # double check it worked

# face perceive
tallied_vals = covid_questions %>% group_by(ages,face_perceive) %>% tally()
chisquare_table <- spread(tallied_vals,ages,n)
chisq.test(chisquare_table)

# day care status -- past
# covid_questions$daycare_past[covid_questions$daycare_past==1] <- 2
covid_questions$daycare_past
tallied_vals = covid_questions %>% group_by(ages,daycare_past) %>% tally()
chisquare_table <- spread(tallied_vals,ages,n)
chisquare_table
chisquare_table[is.na(chisquare_table)] = 0 
chisquare_table
test<-chisq.test(chisquare_table)
test

chies(test$statistic,96)

temp_subset <- covid_questions %>% filter(!ages %in% c('4')) 
tallied_vals <- temp_subset %>% group_by(ages,daycare_past) %>% tally()
fisher_table <- spread(tallied_vals,ages,n)
fisher_table
fisher_table[is.na(fisher_table)] = 0 
fisher_table
test<-chisq.test(fisher_table)
test
test$p.value*3
chies(test$statistic,sum(fisher_table)-3)

# day care number -- past
covid_questions$daycare_num_past
mean(covid_questions$daycare_num_past)
sd(covid_questions$daycare_num_past)
covid_questions %>% group_by(ages) %>% summarise(mean=format(mean(daycare_num_past)),sd=format(sd(daycare_num_past)))
anova <- aov(daycare_num_past ~ ages,data = covid_questions)
summary(anova)

# day care status -- present
#covid_questions$daycare_current[covid_questions$daycare_current==1] <- 2
covid_questions$daycare_current
tallied_vals = covid_questions %>% group_by(ages,daycare_current) %>% tally()
chisquare_table <- spread(tallied_vals,ages,n)
chisquare_table
chisquare_table[is.na(chisquare_table)] = 0 
chisquare_table
chisq.test(chisquare_table)

# day care number -- present
covid_questions$daycare_num_current
mean(covid_questions$daycare_num_current)
sd(covid_questions$daycare_num_current)
covid_questions %>% group_by(ages) %>% summarise(mean=format(mean(daycare_num_current)),sd=format(sd(daycare_num_current)))
anova <- aov(daycare_num_current ~ ages,data = covid_questions)
summary(anova)

# mandates -- past
covid_questions$mandates_past
tallied_vals = covid_questions %>% group_by(ages,mandates_past) %>% tally()
chisquare_table <- spread(tallied_vals,ages,n)
chisquare_table
chisquare_table[is.na(chisquare_table)] = 0 
chisquare_table
chisq.test(chisquare_table)

# mandates -- present
covid_questions$mandates_current
tallied_vals = covid_questions %>% group_by(ages,mandates_current) %>% tally()
chisquare_table <- spread(tallied_vals,ages,n)
chisquare_table
chisquare_table[is.na(chisquare_table)] = 0 
chisquare_table
chisq.test(chisquare_table)

# community masking -- past
covid_questions$community_past
tallied_vals = covid_questions %>% group_by(ages,community_past) %>% tally()
chisquare_table <- spread(tallied_vals,ages,n)
chisquare_table
chisquare_table[is.na(chisquare_table)] = 0 
chisquare_table
chisq.test(chisquare_table)

# community masking -- present
covid_questions$community_current
tallied_vals = covid_questions %>% group_by(ages,community_current) %>% tally()
chisquare_table <- spread(tallied_vals,ages,n)
chisquare_table
chisquare_table[is.na(chisquare_table)] = 0 
chisquare_table
chisq.test(chisquare_table)

# household masking -- past
covid_questions$household_past
tallied_vals = covid_questions %>% group_by(ages,household_past) %>% tally()
chisquare_table <- spread(tallied_vals,ages,n)
chisquare_table
chisquare_table[is.na(chisquare_table)] = 0 
chisquare_table
chisq.test(chisquare_table)

# household masking -- present
covid_questions$household_current
tallied_vals = covid_questions %>% group_by(ages,household_current) %>% tally()
chisquare_table <- spread(tallied_vals,ages,n)
chisquare_table
chisquare_table[is.na(chisquare_table)] = 0 
chisquare_table
chisq.test(chisquare_table)

########## Experiment 1 -- Questionnaire Data
fa_results<-read.csv('factor_results.csv')

# group gender into male vs non-male
levels(fa_results$gender) # f, m, o

# make age a categorical variable
fa_results$ages <- factor(floor(fa_results$ages))
fa_results %>% group_by(ages) %>% tally() # double check it worked
head(fa_results)

anova <- aov(FA1 ~ ages, data = fa_results)
summary(anova)
fa_results

# actually better to get ges this way 
my_anova = ezANOVA(
  data = fa_results
  , dv = .(FA4)
  , wid = .(X)
  , between = .(ages)
)
print(my_anova)

t.test(filter(fa_results, ages == 4)$FA3,filter(fa_results, ages == 5)$FA3)
cohens_d(fa_results,FA3 ~ ages,comparisons=list(c("4", "5")), var.equal = FALSE)

t.test(filter(fa_results, ages == 4)$FA3,filter(fa_results, ages == 6)$FA3)
cohens_d(fa_results,FA3 ~ ages,comparisons=list(c("4", "6")), var.equal = FALSE)

t.test(filter(fa_results, ages == 5)$FA3,filter(fa_results, ages == 6)$FA3)
cohens_d(fa_results,FA3 ~ ages,comparisons=list(c("5", "6")), var.equal = FALSE)

