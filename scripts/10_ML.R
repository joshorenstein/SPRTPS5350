#Load R packages
Packages <- c("here", "caret", "rpartScore", "tidyverse", "recipes","C50","yardstick","e1071","rattle")
lapply(Packages, library, character.only = TRUE)


df <- read_csv("data/nets.csv") #load data
names(df)
df$Outcome <- as.factor(df$Outcome) #change y variable to factor

a <- createDataPartition(df$Outcome, p = 0.7, list=FALSE)
training <- df[a,]
test <- df[-a,]

dim(training)
dim(test)
names(training)
#exploratory analysis
q <- ggplot(training, aes(x=eff_fg,y=Outcome)) +
  geom_boxplot() +
  labs(x = NULL,
       y = "Outcome")
q
names(training)
#keep datapoints that will be used in model
d_select <- training %>%
  select(Outcome:ft_fga)

names(d_select)

m1 <- glm(Outcome~.,data=d_select,family=binomial)
summary(m1)
anova(m1)
results <- training %>%
  mutate(glm_odds = predict(m1, training))
View(results)
#Convert the log odds to probability
results$prob <- exp(results$glm_odds)/(1+exp(results$glm_odds))
View(results)

#tuning controls and then model fits
fitControl <- trainControl(method = "repeatedcv",number=5,repeats=3,
                           summaryFunction = multiClassSummary,classProbs = TRUE,
                           savePredictions = TRUE)
??trainControl
#View(training)
set.seed(12345)
d_select %>% distinct(Outcome)
head(d_select)
names(d_select)
#Random Forest
rf_fit <- train(Outcome ~ ., data = d_select, 
                method = "rf",ntree=5000, 
                trControl = fitControl)

glm_fit <- train(Outcome ~ ., data = d_select, 
                method = "glm", 
                trControl = fitControl)

glmnet_fit <- train(Outcome ~ ., data = d_select, 
                 method = "glmnet", 
                 trControl = fitControl)

rpart_fit <- train(Outcome ~ ., data = d_select, 
                method = "rpart2", 
                trControl = fitControl)
summary(rpart_fit)
plot(rpart_fit$finalModel)
text(rpart_fit$finalModel)
fancyRpartPlot(rpart_fit$finalModel)

results <- training %>%
  mutate(Random_Forest = predict(rf_fit, training),
         Logistic_Reg = predict(glm_fit,training),
         CART = predict(rpart_fit,training),
         GLMNET = predict(glmnet_fit,training))
   
View(results)
# Evaluate the performance
g <- getTrainPerf(rf_fit)
m <- getTrainPerf(rpart_fit)
l <- getTrainPerf(glm_fit)
k <- getTrainPerf(glmnet_fit)
train_perf <- bind_rows(g,m,l,k)
train_perf  %>% arrange(TrainlogLoss)


# Create the new columns in the test set and see how it performs
testing_results <- test %>%
  mutate(Random_Forest = predict(rf_fit, test),
         Logistic_Reg = predict(glm_fit,test),
         CART = predict(rpart_fit,test),
         GLMNET = predict(glmnet_fit,test))

#create some metrics to evaluate test results
a <- metrics(testing_results, truth = Outcome, estimate = Random_Forest) %>% mutate(model="Random_Forest_test")
#b <- metrics(testing_results, truth = Outcome, estimate = Support_Vector) %>% mutate(model="SVM_test")
b <- metrics(results, truth = Outcome, estimate = Random_Forest) %>% mutate(model="Random_Forest_train")
c <- metrics(results, truth = Outcome, estimate  = CART) %>% mutate(model="Regression_Tree_train")
d <- metrics(testing_results, truth = Outcome, estimate  = CART) %>% mutate(model="Regression_Tree_test")
e <- metrics(results, truth = Outcome, estimate  = Logistic_Reg) %>% mutate(model="Logit_train")
f <- metrics(testing_results, truth = Outcome, estimate  = Logistic_Reg) %>% mutate(model="Logit_test")
g <- metrics(results, truth = Outcome, estimate  = GLMNET) %>% mutate(model="Glmnet_train")
h <- metrics(testing_results, truth = Outcome, estimate  = GLMNET) %>% mutate(model="Glmnet_test")

#see the train/test accuracy
bind_rows(a,b,c,d,e,f,g,h) %>% filter(.metric=="accuracy") %>% arrange(desc(model))

# Confusion matrix for knn
confusionMatrix(predict(rpart_fit,testing_results),
                testing_results$Outcome)

varImp(rf_fit,scale=TRUE)
varImp(rpart_fit,scale=TRUE)
