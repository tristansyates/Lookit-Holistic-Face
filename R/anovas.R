rm(list=ls()) #clear the environment

#install.packages("rstatix")
library(tidyverse)
library(dplyr)
library(rstatix)
library(ez)
library(lme4)
library(car)

###################################
########## Experiment 1 

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

## Critical same trials 
my_anova = ezANOVA(
  data = filter(rt_data,condition=="same") #
  , dv = .(time_screen)
  , wid = .(sub)
  , within = .(alignment)
  , between = .(age,gender)
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

# follow up test of relationship with age for different condition
rt_data %>%
  filter(condition=="different")  %>%
  group_by(sub) %>% 
  summarise(time_screen=mean(time_screen),age=mean(as.numeric(as.character(age)))) %>%
  group_by(age) %>%
  get_summary_stats(time_screen, type="mean_sd")

# pairwise t-tests grouped by subject
temp_rt_data<- rt_data %>%
  filter(condition=="different")  %>%
  group_by(sub) %>%
  summarise(time_screen=mean(time_screen),age=mean(as.numeric(as.character(age))))

t.test(filter(temp_rt_data, age == 4)$time_screen,filter(temp_rt_data, age == 5)$time_screen)
t.test(filter(temp_rt_data, age == 5)$time_screen,filter(temp_rt_data, age == 6)$time_screen)
t.test(filter(temp_rt_data, age == 4)$time_screen,filter(temp_rt_data, age == 6)$time_screen)

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
my_anova$ANOVA$p[1] # age
my_anova$ANOVA$p[2] # gender
my_anova$ANOVA$p[3] # alignment

# follow-ups!! 
# How does accuracy (over all trials) differ between ages 
temp_acc_data<- accuracy_data %>%
  filter(condition=="same") %>%
  group_by(sub) %>%
  summarise(accuracy=mean(accuracy),age=mean(as.numeric(as.character(age))))
temp_acc_data
t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 5)$accuracy)
t.test(filter(temp_acc_data, age == 5)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 6)$accuracy)

# pairwise t-tests grouped by subject
accuracy_data %>%
  filter(age==4,condition=="same")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 4,condition=="same",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 4,condition=="same",alignment=="misaligned")$accuracy,paired = TRUE)
test
test$p.value*3 # correct the pval

accuracy_data %>%
  filter(age==5,condition=="same")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 5,condition=="same",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 5,condition=="same",alignment=="misaligned")$accuracy,paired = TRUE)
test
test$p.value*3 # correct the pval

accuracy_data %>%
  filter(age==6,condition=="same")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 6,condition=="same",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 6,condition=="same",alignment=="misaligned")$accuracy,paired = TRUE)
test
test$p.value*3 # correct the pval

# well how does accuracy (over aligned-same ) differ between ages 
temp_acc_data<- accuracy_data %>%
  filter(condition=="same", alignment=="misaligned") %>%
  group_by(sub) %>%
  summarise(accuracy=mean(accuracy),age=mean(as.numeric(as.character(age))))
temp_acc_data
t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 5)$accuracy)
t.test(filter(temp_acc_data, age == 5)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 6)$accuracy)

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
t.test(filter(temp_acc_data, age == 5)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 6)$accuracy)


# pairwise t-tests grouped by subject
accuracy_data %>%
  filter(age==4,condition=="different")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 4,condition=="different",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 4,condition=="different",alignment=="misaligned")$accuracy,paired = TRUE)
test
test$p.value*3 # correct the pval

# pairwise t-tests grouped by subject
accuracy_data %>%
  filter(age==5,condition=="different")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 5,condition=="different",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 5,condition=="different",alignment=="misaligned")$accuracy,paired = TRUE)
test
test$p.value*3 # correct the pval

# pairwise t-tests grouped by subject
accuracy_data %>%
  filter(age==6,condition=="different")%>%
  group_by(alignment) %>%
  summarise(accuracy=mean(accuracy))

test<-t.test(filter(accuracy_data, age == 6,condition=="different",alignment=="aligned")$accuracy,
       filter(accuracy_data, age == 6,condition=="different",alignment=="misaligned")$accuracy,paired = TRUE)
test

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
levels(rt_data_part2$gender) # f, m, o
levels(rt_data_part2$gender) <- c('non-m','m','non-m')
levels(rt_data_part2$gender) # non-m, m
# now view it 
rt_data_part2 %>%
  group_by(gender) %>% tally()/4

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
levels(accuracy_data_part2$gender) # f, m, o
levels(accuracy_data_part2$gender) <- c('non-m','m','non-m')
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

# how about the different trials?
my_anova = ezANOVA(
  data = filter(accuracy_data_part2,condition=="different")#accuracy_data_part2
  , dv = .(accuracy)
  , wid = .(sub)
  , within = .(alignment)#,condition)
  , between = .(age, gender)
)
print(my_anova)

# How does accuracy (over all trials) differ between ages 
temp_acc_data<- accuracy_data_part2 %>%
  filter(condition=="different") %>%
  group_by(sub) %>%
  summarise(accuracy=mean(accuracy),age=mean(as.numeric(as.character(age))))

t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 5)$accuracy)
t.test(filter(temp_acc_data, age == 5)$accuracy,filter(temp_acc_data, age == 6)$accuracy)
t.test(filter(temp_acc_data, age == 4)$accuracy,filter(temp_acc_data, age == 6)$accuracy)

