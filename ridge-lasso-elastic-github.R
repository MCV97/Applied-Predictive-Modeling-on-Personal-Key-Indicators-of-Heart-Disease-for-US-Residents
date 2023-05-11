getwd()
setwd("C:/Users/monic/OneDrive/Desktop/Tesis")

library("tidyverse")
library("caret")
library("tidymodels")
library("themis")
library("rpart.plot")
library("ranger")
library("ggpubr")
library("gglasso")
library("glmnet")
library("vip")

dat <- read_csv("Heart-Data2.csv")
dat[sapply(dat, is.character)] <- lapply(dat[sapply(dat, is.character)],
                                         as.factor)
#Change response to binary
dat$HeartDisease<-ifelse(dat$HeartDisease=="Yes",1,0)
#sum(dat$HeartDisease) #unbalanced 27373 of 319795 

#Change categorical variables to binary
dat$Smoking<-ifelse(dat$Smoking=="Yes",1,0)
dat$AlcoholDrinking<-ifelse(dat$AlcoholDrinking=="Yes",1,0)
dat$Stroke<-ifelse(dat$Stroke=="Yes",1,0)
dat$DiffWalking<-ifelse(dat$DiffWalking=="Yes",1,0)
dat$PhysicalActivity<-ifelse(dat$PhysicalActivity=="Yes",1,0)
dat$Asthma<-ifelse(dat$Asthma=="Yes",1,0)
dat$KidneyDisease<-ifelse(dat$KidneyDisease=="Yes",1,0)
dat$SkinCancer<-ifelse(dat$SkinCancer=="Yes",1,0)
dat$Sex <- ifelse(dat$Sex == "Female",1,0) #1=female, 0=male

#One-Hot Encoding
dmy <- dummyVars(" ~ .", data = dat, fullRank = T)
dat <- data.frame(predict(dmy, newdata = dat))
#dat$HeartDisease <- ifelse(dat$HeartDisease == "1", "Yes", "No")
dat$HeartDisease <- as.factor(dat$HeartDisease)
glimpse(dat)

#Specify factor levels
#dat$HeartDisease <- factor(dat$HeartDisease, levels = c("1","0"))


#Build Model
set.seed(123)
HD_split <- initial_split(dat, strata = HeartDisease)
HD_train <- training(HD_split)
HD_test <- testing(HD_split)



#Create Model Matrix
trainup<-upSample(x=HD_train, y=HD_train$HeartDisease)
trainup <- trainup[,-25]
X <- model.matrix(HeartDisease~., trainup)[,-1]
y <- trainup$HeartDisease

#Create lambda grid
grid <- 10^seq(2, -6, length = 50)
range(grid)


#########################RIDGE#################################
ridge.cv <- cv.glmnet(X, y, family = "binomial", 
                      type.measure = "auc",
                      lambda = grid,
                      alpha = 0)
plot(ridge.cv)
title("Ridge", line = 2.5)

#Get Final Model
ridge.mod <- glmnet(X, y, family = "binomial",
                    lambda = ridge.cv$lambda.1se,
                    type.measure = "auc",
                    alpha = 0)

# Make prediction on test data
x.test <- model.matrix(HeartDisease ~., HD_test)[,-1]
y.test <- HD_test$HeartDisease
prob <- ridge.mod %>% predict(newx = x.test, type = "response")
predicted.classes <- ridge.mod %>% predict(newx = x.test, type = "class")
predicted.classes <- as.factor(predicted.classes)

#Get AUC & Metrics
confusionMatrix(predicted.classes, HD_test$HeartDisease, positive = "1")
assess.glmnet(ridge.mod, newx = x.test, newy = y.test, family = "binomial")


#######################LASSO################################
lasso.cv <- cv.glmnet(X,y,family= "binomial",
                      type.measure =  "auc", 
                      lambda = grid,
                      alpha = 1)

plot(lasso.cv)
title("Lasso", line = 2.5)


#Get Final Model
lasso.mod1 <- glmnet(X, y, family = "binomial",
                     lambda = lasso.cv$lambda.min,
                     type.measure = "auc",
                     alpha = 1)
lasso.mod2 <- glmnet(X,y,family = "binomial",
                     lambda = lasso.cv$lambda.1se,
                     type.measure = "auc",
                     alpha = 1)

# Make prediction on test data
x.test <- model.matrix(HeartDisease ~., HD_test)[,-1]
y.test <- HD_test$HeartDisease
prob1 <- lasso.mod1 %>% predict(newx = x.test, type = "response")
predicted.classes <- lasso.mod1 %>% predict(newx = x.test, type = "class")
prob2 <- lasso.mod2 %>% predict(newx = x.test, type = "response")
predicted.classes2 <- lasso.mod2 %>% predict(newx = x.test, type = "class")


#Get AUC & Metrics (Choose model 1)
assess.glmnet(lasso.mod1, newx = x.test, newy = y.test, family = "binomial")
assess.glmnet(lasso.mod2, newx = x.test, newy = y.test, family = "binomial")
confusionMatrix(predicted.classes2, HD_test$HeartDisease, positive = "1")

#Variable Importance
library(vip)
vip_lasso <- vip(lasso.mod2, num_features = 25)+theme_bw()+
  ggtitle(" Logistic Lasso Regression Importance Plot")

#############################ELASTIC NET#####################
ela.cv.1 <- cv.glmnet(X,y,family= "binomial",
                      type.measure =  "auc", 
                      alpha = .1,
                      lambda = grid)
ela.cv.2 <- cv.glmnet(X,y,family= "binomial",
                      type.measure =  "auc", 
                      alpha = .2,
                      lambda = grid)
ela.cv.3 <- cv.glmnet(X,y,family= "binomial",
                      type.measure =  "auc", 
                      alpha = .3,
                      lambda = grid)
ela.cv.4 <- cv.glmnet(X,y,family= "binomial",
                      type.measure =  "auc", 
                      alpha = .4,
                      lambda = grid)
ela.cv.5 <- cv.glmnet(X,y,family= "binomial",
                      type.measure =  "auc", 
                      alpha = .5,
                      lambda = grid)
ela.cv.6 <- cv.glmnet(X,y,family= "binomial",
                      type.measure =  "auc", 
                      alpha = .6,
                      lambda = grid)
ela.cv.7 <- cv.glmnet(X,y,family= "binomial",
                      type.measure =  "auc", 
                      alpha = .7,
                      lambda = grid)
ela.cv.8 <- cv.glmnet(X,y,family= "binomial",
                      type.measure =  "auc", 
                      alpha = .8,
                      lambda = grid)
ela.cv.9 <- cv.glmnet(X,y,family= "binomial",
                      type.measure =  "auc", 
                      alpha = .9,
                      lambda = grid)

mod1 <- glmnet(X, y, family = "binomial",
               lambda = ela.cv.1$lambda.1se,
               type.measure = "auc",
               alpha = 0.1)
mod2 <- glmnet(X, y, family = "binomial",
               lambda = ela.cv.2$lambda.1se,
               type.measure = "auc",
               alpha = 0.2)
mod3 <- glmnet(X, y, family = "binomial",
               lambda = ela.cv.3$lambda.1se,
               type.measure = "auc",
               alpha = 0.3)
mod4 <- glmnet(X, y, family = "binomial",
               lambda = ela.cv.4$lambda.1se,
               type.measure = "auc",
               alpha = 0.4)
mod5 <- glmnet(X, y, family = "binomial",
               lambda = ela.cv.5$lambda.1se,
               type.measure = "auc",
               alpha = 0.5)
mod6 <- glmnet(X, y, family = "binomial",
               lambda = ela.cv.6$lambda.1se,
               type.measure = "auc",
               alpha = 0.6)
mod7 <- glmnet(X, y, family = "binomial",
               lambda = ela.cv.7$lambda.1se,
               type.measure = "auc",
               alpha = 0.7)
mod8 <- glmnet(X, y, family = "binomial",
               lambda = ela.cv.8$lambda.1se,
               type.measure = "auc",
               alpha = 0.8)
mod9 <- glmnet(X, y, family = "binomial",
               lambda = ela.cv.9$lambda.1se,
               type.measure = "auc",
               alpha = 0.9)

x.test <- model.matrix(HeartDisease ~., HD_test)[,-1]
y.test <- HD_test$HeartDisease
res1 <- assess.glmnet(mod1, newx = x.test, newy = y.test, family = "binomial")$auc
res2 <- assess.glmnet(mod2, newx = x.test, newy = y.test, family = "binomial")$auc
res3 <- assess.glmnet(mod3, newx = x.test, newy = y.test, family = "binomial")$auc
res4 <- assess.glmnet(mod4, newx = x.test, newy = y.test, family = "binomial")$auc
res5 <- assess.glmnet(mod5, newx = x.test, newy = y.test, family = "binomial")$auc
res6 <- assess.glmnet(mod6, newx = x.test, newy = y.test, family = "binomial")$auc
res7 <- assess.glmnet(mod7, newx = x.test, newy = y.test, family = "binomial")$auc
res8 <- assess.glmnet(mod8, newx = x.test, newy = y.test, family = "binomial")$auc
res9 <- assess.glmnet(mod9, newx = x.test, newy = y.test, family = "binomial")$auc
rr <- c(res1,res2,res3,res4,res5,res6,res7,res8,res9)
rrr <- data.frame(x = seq(.1,.9,by = .1), y = rr)

ggplot(rrr,aes(x=x,y=y))+geom_point()+theme_bw()+
  xlab(expression(alpha))+ylab("AUC")
#Highest AUC is when Alpha = .1


#CV WITH BEST ALPHA = 0.1
grid <- 10^seq(2, -6, length = 100)
ela.cv <-cv.glmnet(X,y,family= "binomial",
                   type.measure =  "auc", 
                   alpha = .1,
                   lambda = grid)
plot(ela.cv)
title("Elastic Net", line = 2.5)


#Get Final Model
ela.mod1 <- glmnet(X, y, family = "binomial",
                   lambda = ela.cv$lambda.min,
                   type.measure = "auc",
                   alpha = 0.1)
ela.mod2 <- glmnet(X,y,family = "binomial",
                   lambda = ela.cv$lambda.1se,
                   type.measure = "auc",
                   alpha = 0.1)

# Make prediction on test data
x.test <- model.matrix(HeartDisease ~., HD_test)[,-1]
y.test <- HD_test$HeartDisease
prob1 <- ela.mod1 %>% predict(newx = x.test, type = "response")
predicted.classes <- ela.mod1 %>% predict(newx = x.test, type = "class")
prob2 <- ela.mod2 %>% predict(newx = x.test, type = "response")
predicted.classes2 <- ela.mod2 %>% predict(newx = x.test, type = "class")


#Get AUC and Metrics
assess.glmnet(ela.mod1, newx = x.test, newy = y.test, family = "binomial")
assess.glmnet(ela.mod2, newx = x.test, newy = y.test, family = "binomial")
confusionMatrix(predicted.classes2, HD_test$HeartDisease, positive = "1")

