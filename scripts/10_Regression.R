#load some packages
library(modelr)
library(tidyverse)
library(corrplot)
library(caret)
library(car)
#load 2019 pitching data
df <- read_csv('data/basketball.csv')
head(df)
str(df)
#Do some histograms/boxplot
df %>%
  ggplot(aes(Wins)) + geom_histogram(binwidth = 10)

df %>%
  ggplot(aes(log(Two_Point_Pct))) + geom_histogram(binwidth = .05)

df %>%
  ggplot(aes(log(Three_Point_Pct))) + geom_histogram(binwidth = 2)
df %>% 
  ggplot(aes(Wins)) + geom_boxplot()

df %>% 
  ggplot(aes(Two_Point_Pct)) + geom_boxplot()

df %>% 
  ggplot(aes(Three_Point_Pct)) + geom_boxplot()
names(df)

#Do some correlations
cor_df <- df %>% select(Wins:Two_Point_Pct)
str(cor_df)
correlations <- round(cor(cor_df),2)
correlations
corrplot(correlations,order="hclust") #show correlations

highCorr <- findCorrelation(correlations,cutoff=.9,names=TRUE) #find highly correlated predictors
highCorr

#vif

#Regression model
m1 <- lm(Wins~Three_Point_Pct+Two_Point_Pct,data=df)
m1
class(m1) #check the data type
summary(m1) #look at the regression stats

#put the regression in tidyverse code
m1 <- df %>% 
  lm(Wins~Three_Point_Pct+log(Two_Point_Pct),data=.)
m2 <- df %>% 
  lm(Wins~Three_Point_Pct+Two_Point_Pct,data=.)
summary(m1)
summary(m2)
library(broom)

#Look at a few things
m1 %>% tidy() %>% View() #look at b0 and b1

m1 %>% glance() %>% View() # look at the regression stats

m1 %>% augment() %>% View() #look at the output values

m1 %>% augment(data=df) %>% View() #bind the output values to the data


#use glance and just grab r-squared and p-value
m1 %>% 
  glance() %>% 
  select(adj.r.squared,p.value)

#grab only the data where p value is less than .05
m1 %>% 
  tidy() %>% 
  filter(p.value < .05)

m1 %>% 
  tidy() %>% 
  filter(p.value < .05)

df_model <- df %>%
  add_predictions(m1) 
View(df_model)
#look at some summary stats
summary(m1)
#look at residual plots
plot(m1)
outlierTest(m1)
anova(m2)
1232/(1232+1395.5+1568.5)
1395/(1232+1395.5+1568.5)
#add residuals to the model
df %>%
  add_residuals(m1)

#plot the residuals for all 4 models
df %>%
  gather_residuals(m1) %>% 
  ggplot(aes(Wins, resid)) +
  geom_bin2d() +
  geom_smooth() 

names(df)


