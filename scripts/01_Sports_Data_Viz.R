#install these packages if you haven't done so already
#install.packages("gridExtra")
#install.packages("tidyverse")
#install.packages("here")

#Load the packages 
library(tidyverse)
library(here)
library(gridExtra)

#Press CTRL-Enter to run your code

here()
#Read the data
df <- read_csv("data/nba_player_stats_1920.csv")
#  alt-dash to make that arrow
#alternative loading method
#df <- read_csv("C:/Users/jorenstein/Documents/PROJECT/SPRTPS5350data/nba_player_stats_1920.csv") 
#Look at your data

dim(df) # see the dimensions of your data in rows and columns (105 rows, 25 columns)
dim(full_df)  # see the dimensions of your data in rows and columns (207,058 rows, 6 columns)
names(df) # see the names of your columns
head(df) # see the first 6 rows of your data
tail(df) # see the last 6 rows of your data
str(df)
# Basic plotting in R
plot(df$TRB,df$BLK) 
# View the NBA player data
View(df)

names(df)
#Scatterplot of the NBA data in GGplot
ggplot(data = df) +
  geom_point(mapping = aes(x = FG_PCT, y = STL))

ggplot(df) +
  geom_point(aes(x=TRB, y=BLK,color=Pos))

#Aesthetics
ggplot(df) + geom_point(aes(x = TRB, y = BLK, color = Pos))
ggplot(df) + geom_point(aes(x = TRB, y = BLK, size = Pos))
ggplot(df) + geom_point(aes(x = TRB, y = BLK, shape = Pos))
ggplot(df) + geom_point(aes(x = `2P_PCT`, y = BLK, alpha = Pos)) #transparency of the datapoint
ggplot(df) + geom_point(aes(x = TRB, y = BLK, alpha = Pos))

#Set vs Map
ggplot(df) + geom_point(aes(x = TRB, y = BLK, color = "blue")) # color = green adds text to the legend
ggplot(df) + geom_point(aes(x = TRB, y = BLK), color = "blue") # color of the dot is now green

#Facets
small_df <- df %>% filter(Tm %in% c("BOS","DEN","MIL","ATL","SAS"))  #We'll cover filter shortly but all we're doing here is keeping a few teams worth of data
dim(small_df)
View(small_df)
q <- ggplot(small_df) + geom_point(aes(x = TRB, y = BLK))
q
q + facet_grid(. ~ Pos) #Position in the columns
q + facet_grid(Tm ~ .) #Teams in the rows
q + facet_grid (Tm ~ Pos) #Teams in the rows and Position in the columns
q + facet_wrap(~ Pos) #1D Ribbon wrapped into 2D

#What's similar about these plots?
#Same x-axis, same y-axis, same data
#What's different?
#geometric object (geom point vs geom smooth)

x <- ggplot(data = df) +
  geom_point(mapping = aes(x = TRB, y = BLK))
y <- ggplot(data = df) +
  geom_smooth(mapping = aes(x = TRB, y = BLK))
grid.arrange(x,y, ncol=2, nrow = 1)

#Make a boxplot
ggplot(df) +
  geom_boxplot(mapping = aes(x = Pos, y = AST))

#Order the positions in the typical order of PG -> C
df$Pos <- factor(df$Pos , levels=c("PG", "SG", "SF", "PF","C"))  

#Plot it again
ggplot(df) +
  geom_boxplot(mapping = aes(x = Pos, y = TRB))

#Add a layer
ggplot(df) +
  geom_point(mapping = aes(x = TRB, y = BLK)) +
  geom_smooth(mapping = aes(x = TRB, y = BLK))

ggplot(df) +
  geom_point(mapping = aes(x=TRB,y=BLK)) +
  geom_smooth(mapping = aes(x = TRB, y = BLK))

#### END APRIL 23 ####






#Global vs Local
df_guards <- df %>% filter(Pos %in% c("PG","SG")) #lets just use guards for this example
#Local -
ggplot(data = df_guards, mapping = aes(x = TRB, y = BLK)) +
  geom_point(mapping = aes(color = Pos)) + #Position color is only on geom_point
  geom_smooth()

ggplot(data = df_guards, mapping = aes(x = TRB, y = BLK)) +
  geom_point(mapping = aes(color = Pos)) +
  geom_smooth(mapping = aes(color = Pos), se = FALSE) #Draw a line based on the color. se = false removes error bars


ggplot(data = df_guards, mapping = aes(x = TRB, y = BLK)) +
  geom_point(mapping = aes(color = Pos)) +
  geom_smooth(mapping = aes(group = Pos), se = FALSE) #draws sep lines for PG and SG but lines are same color
#Global
ggplot(data = df_guards, mapping = aes(x = TRB, y = BLK,color=Pos)) + #Position color on all geoms
  geom_point() + 
  geom_smooth() 

#Draw a density plot
ggplot(data = df,
       mapping = aes(x = TRB, y = BLK, color = Pos)) +
  geom_density_2d(color = "black") +
  geom_point()

#Draw a bar chart
ggplot(data = df) +
  geom_bar(mapping = aes(x = Pos))
#this gives us count by position
df %>% group_by(Tm) %>% summarise(count=n()) %>% arrange(desc(count))
#Here is a count 
ggplot(data = small_df) +
  geom_bar(mapping = aes(x = Tm))

#Count by Position using stat = count
ggplot(data = df) +
  geom_bar(mapping = aes(x = Pos),
           stat = "count")

sample <- data.frame(x = c("A", "B", "C"), y = 1:3) #create a dataframe
sample #look at it 
ggplot(data = sample) +
  geom_bar(mapping = aes(x = x, y = y), stat = "identity") #identity plots exactly whats in the dataframe

#Color,Fill,Clarity
ggplot(df, mapping = aes(x = Pos)) +
  geom_bar(mapping = aes(color = Pos)) 
ggplot(df, mapping = aes(x = Pos)) +
  geom_bar(mapping = aes(fill = Pos))

ggplot(small_df, mapping = aes(x = Pos)) +
  geom_bar(mapping = aes(fill = Tm))
#Dodge
ggplot(small_df, mapping = aes(x = Pos)) +
  geom_bar(mapping = aes(fill = Tm),
           position = "dodge")

#Stack, Dodge, Identity, Fill
p <- ggplot(small_df, aes(x = Tm, fill = Pos))
q <- p + geom_bar(position = "stack")
r <- p + geom_bar(position = "dodge")
s <- p + geom_bar(position = "identity") #overlaps
t <- p + geom_bar(position = "fill") #uses proportion

grid.arrange(q,r,s,t, ncol=2, nrow = 2)

#jittering
p <- ggplot(df, aes(TRB, BLK))
p + geom_point()
p + geom_point() + geom_jitter() #moves each point by a tiny random amount

#coordinates
ggplot(df) +
  geom_bar(mapping = aes(x = Pos, fill = Pos)) +
  coord_polar()

#radar chart
ggplot(df) +
  geom_bar(mapping = aes(x = Pos, fill = Pos), width = 1) +
  coord_polar(theta = "y")

#histogram
ggplot(df) +
  geom_histogram(aes(BLK), binwidth = 0.1)

#zoom in on the histogram between 0 and 1 blocks and a count of 5 to 15 players
ggplot(df) +
  geom_histogram(aes(BLK), binwidth = 0.2) +
  coord_cartesian(xlim = c(0,1), ylim = c(5,15))

#density
ggplot(df, aes(x = BLK, color = Pos)) +
  geom_density()

#labels
p1 <- ggplot(df, aes(x = BLK, color = Pos)) +
  geom_density()

p1 + labs(title="Centers block more shots than other positions")

p1 + labs(title = "Centers block more shots than other positions",
          subtitle = "Point Guards block the least",
          caption = "Data: NBA Players 2019-20 Season")

p1 + labs(x = "Blocked Shots Per Game", y = "Density", color = "Position")

#themes
p1 + theme_bw()
p1 + theme_gray()
p1 + theme_dark()
p1 + theme_light()

#fancy themes
# install.packages("ggthemes") #if its not installed already
library(ggthemes)
p1 + theme_tufte()
p1 + theme_fivethirtyeight()
p1 + theme_economist()

names(df)
#scales

p2 <- ggplot(df, aes(AST,STL)) +
  geom_point(aes(color = STL))
p2 + scale_x_log10() + scale_y_log10() #take the log of your data
p2 + scale_color_distiller(palette = "Spectral") #use heat map colors

#Use Color Brewer Colors
library(RColorBrewer)
p2 + scale_color_distiller(palette = "Blues"
                           )
p1 + scale_color_brewer(palette = "Set1")



#we'll come back to this but i'm transforming the position into Guards, Wings and Bigs
df <- df %>% 
  mutate(New_Pos=if_else(Pos %in% c("PG","SG"),"Guards",if_else(Pos=="SF","Wings","Bigs"))) 

#Pull it all together          
library(ggthemes)
z <- ggplot(df, aes(x = BLK, y =  TRB)) +
  geom_point() +
  geom_smooth(aes(color = New_Pos), se = FALSE) +
  labs(title = "Bigs have highest upside on rebounds and blocks",
       subtitle = "Lines show estimate of mean values for each position",
       caption = "Source: NBA 19-20 Player Data",
       x = "Blocks",
       y = "Rebounds",
       color = "Position") +
  scale_color_brewer(palette = "Set1") +
  theme_tufte() 
z

ggsave("images/NBA_Rebounds_and_Blocks.png", width = 6, height = 4)
