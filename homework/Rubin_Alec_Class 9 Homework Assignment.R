library(tidyverse)
library(modelr)
df <- read.csv("franchise_values.csv")
#df <- read.csv("data/franchise_values.csv") 
head(df)
str(df)
view(df)

#Q1

m1 <- df %>% 
  lm(Value~Revenue,data=.)
summary(m1)
MLB <- filter(df,League == "MLB")
view(MLB)
mMLB <- MLB %>% 
  lm(Value~Revenue,data=.)
summary(mMLB)
#jko: I don't think I showed this to you yet, but if you plot your model
#you'll get all the diagnoistic charts
plot(mMLB)

#Q1A: SST is 9691417.5 (computed by summing SSR and SSE)
#R-squared is 0.9592, which means 95.9% of the total variation of Value is
#explained by team revenue, indicating a strong correlation.

#Q1B: Because SST consists of SSR + SSE (none of which can be negative).

#Q2

#Q2A:
ggplot(MLB) +
  geom_point(mapping = aes(x=Revenue,y=Value))

#Q2B: The y-intercept (b0) is -841.9 and the slope (b1) is 7.79.

#Q2C: This means that each additional $1 million in a team's annual
#revenue corresponds with an expected increase of $7.79 million in that team's
#franchise value (with a starting value of -$841.9 million given a revenue of 0)

#Q2D: 
#Mean Value = -841.8773 + (350 x 7.7876)
#Predicted mean value = $1,883.77 million

#Q2E: I would say that there appears to be a tight relationship between annual
#revenue and team value (the more a team earns annually, the more we should
#expect to pay). If a team is being priced at a value less than their annual
#revenue would suggest, that is an investment opportunity worth exploring.

#Q3

NBA <- filter(df,League == "NBA")
#jo: i prefer to do the filtering this way
NBA <- df %>% 
  filter(League == "NBA")

view(NBA)

#Q3A:
ggplot(NBA) +
  geom_point(mapping = aes(x=Revenue,y=Value))

#jko: I prefer to use ggplot within the tidyverse piping. Makes it easier
#as you add to the plot
NBA %>% 
  ggplot(mapping = aes(x=Revenue,y=Value)) + geom_point()


#Q3B:
mNBA <- NBA %>% 
  lm(Value~Revenue,data=.)
summary(mNBA)


# The y-intercept (b0) is -1755.7 and the slope (b1) is 14.9977.

#Q3C: Each additional $1 million in annual revenue corresponds with an expected
#increase of $15 million for that team's value, using a starting point of
#-$1,756 million at $0 annual revenue.

#Q3D:
#Mean value = -$1,755.7339 + (350 x 14.9977)
#Predicted mean value = $3,493.461 million

#Q3E: There is a noticeable correlation between a team's annual revenue and
#their franchise value, although this relationship does not seem to be quite
#as tight as the one seen with the MLB. Based on the data on hand, this
#model seems most reliable for the lowest-earning teams (making less than
#$250 million per year), while predictions appear to be less accurate for teams
#with revenues above that mark. 

#Q4

NHL <- filter(df,League == "NHL")
view(NHL)

#Q4A:
ggplot(NHL) +
  geom_point(mapping = aes(x=Revenue,y=Value))

#Q4B:
mNHL <- NHL %>% 
  lm(Value~Revenue,data=.)
summary(mNHL)
#The y-intercept (b0) is -625.8889 and the slope (b1) is 9.0734.

#Q4C: Starting at a value of -$625.8889 million given $0 annual revenue, each 
#additional $1 million in team revenue corresponds with an expected increase
#of $9.0734 million for that team's value.

#Q4D:
#Mean Value = -625.8889 + (350 x 9.0734)
#Predicted mean value = $2,549.8011 million

#Q4E: Once again, a team's annual revenue appears to be a fairly reliable
#predictor of that team's overall value, so the highest earning teams will
#likely come with the highest price tag. That said, this relationship seems
#like it may not be as strictly linear in nature compared to other leagues,
#with low-earning teams on average only seeing marginal increases in franchise 
#value for each additional $1 million in revenue, while the impact of additional
#revenue seems to grow stronger for the highest-earners.

#Q5

NFL <- filter(df,League == "NFL")
view(NFL)

#Q5A:
ggplot(NFL) +
  geom_point(mapping = aes(x=Revenue,y=Value))

#Q5B:
mNFL <- NFL %>% 
  lm(Value~Revenue,data=.)
summary(mNFL)
#The y-intercept (b0) is 61.71 and the slope (b1) is 6.256.

#Q5C: Starting at a value of $61.71 million given $0 annual revenue, each
#additional $1 million in annual revenue corresponds with an expected increase
#of $6.256 million for that team's value.

#Q5D:
#Mean value = 61.71 + (350 x 6.256)
#Predicted mean value = $2,251.31 million

#Q5E: While there appears to be a positive relationship between a team's annual
#revenue and franchise value, revenue seems to be a less reliable predictor
#for NFL team values compared to what we see with other leagues. This model has
#a considerably lower r-squared compared to that of other leagues, as well as
#a higher standard error. I would also tell potential investors to be mindful
#of recent team relocations, as two of the biggest outliers (the Rams and 
#Raiders) both have values that are far greater than their annual revenues
#would predict, and both happen to be teams that moved cities in recent years.

#Q6

#Q6E1A: R-squared is 0.9592, which means 95.9% of the total variation of Value is
#explained by team revenue, indicating a very strong correlation.

#Q6E1B: 118.9

#Q6E1C: This model seems very useful for predicting MLB franchise values.

#Q6E2A: R-squared is 0.8945, which means 89.45% of the total variation of Value 
#is explained by team revenue, indicating a fairly strong correlation.

#Q6E2B: 335.7

#Q6E2C: This model seems fairly useful for predicting NBA franchise values, 
#although predictions seem to be strongest for teams earning below $250 million
#while being less accurate for teams earning above $250 million.

#Q6E3A:R-squared is 0.8982, which means 89.82% of the total variation of Value 
#is explained by team revenue, indicating a fairly strong correlation.

#Q6E3B: 110.6

#Q6E3C: This model seems fairly useful for predicting NHL franchise values, 
#although it may need to be tweaked to be most useful since the relationship
#between revenue and value seems like it may not be a strictly linear one.

#Q6E4A: R-squared is 0.6887, which means 68.87% of the total variation of Value 
#is explained by team revenue, indicating that there are likely unaccounted-for
#variables that impact franchise value.

#Q6E4B: 449.8

#Q6E4C: This model isn't great at predicting NFL franchise values, but it's
#better than nothing. There are other variables impacting franchise value
#that are not being accounted for in this current model.

#Q7: Some additional variables could include on-field performance (such as 
#championships and/or win percentage), location (such as size of the local 
#market), as well as longevity (both how long a team has existed as well as how 
#long they have been playing in their current market).