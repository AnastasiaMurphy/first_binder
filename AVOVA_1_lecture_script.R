#AVOVA 1 Workshop 8

#Between Participants ANOVA

#First load packages 
library(tidyverse) #for data tidying and wrangling
library(afex) #for conducting factorial ANOVA
library(emmeans) #for running follow-up tests on the ANOVA model

#Reading in data
my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/cond.csv")
head(my_data) #to view data, can see condition is a character and I need it to be coded as a factor

#mutating character to factor
my_data_tidied <- my_data %>%
  mutate(Condition = factor(Condition)) 
head(my_data_tidied) #can see it is now a factor as required

#summarizing data (summary statistics)
my_data_tidied %>%
  group_by(Condition) %>%
  summarise(mean = mean(Ability), sd = sd(Ability))

#visualising data 
set.seed(1234)
my_data_tidied %>% 
  ggplot(aes(x = Condition, y = Ability, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

#We have built a visualisation where we have plotted the raw data points using 
#the geom_jitter() function, and the shape of the distribution for each 
#condition using the geom_violin() function. We have also added some summary 
#data in the form of the Mean and Confidence Intervals around the Mean using 
#the stat_summary() function.

#building ANOVA Model 
#using aov_4() function in the afex package
#The syntax for ANOVA models in aov_4() is: aov_4(DV ~ IV + (1 | Participant), data = my_data_tidied). The ~ symbol means predicted by, the (1 | Participant) term corresponds to our random effect - we obviously can’t test all the participants in the world so have taken just a random sample from this population. Finally, we need to specify what dataset we are using by making that clear in the data = my_data_tidied bit of the model. We are going to map the output of the aov() function onto a variable I’m calling model. This means that the ANOVA results will be stored in this variable and will allow us to access them later.
model <- aov_4(Ability ~ Condition + (1 | Participant), data = my_data_tidied)

summary(model)

emmeans(model, pairwise ~ Condition)

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")

#Repeated Measures ANOVA 

#read in data
rm_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/rm_data.csv")
head(rm_data)

rm_data_tidied <- rm_data %>%
  mutate(Condition = factor(Condition))
head(rm_data_tidied)

#summary statistics
rm_data_tidied %>%
  group_by(Condition) %>%
  summarise(mean = mean(RT), sd = sd (RT))

#data visualisation 
#using fct_reorder() function to reorder based on RT.
rm_data_tidied %>%
  ggplot(aes(x = fct_reorder(Condition, RT), y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Condition", y = "RT (s)")

#building ANOVA Model 
rm_model <- aov_4(RT ~ Condition + (1 + Condition | Participant), data = rm_data_tidied)

summary(rm_model)

anova(rm_model)

emmeans(rm_model, pairwise ~ Condition, adjust = "Bonferroni")


#Factorial ANOVA
#ANOVA 2 way and 3 way - more than 1 factor and how the factors interact with eachother 


