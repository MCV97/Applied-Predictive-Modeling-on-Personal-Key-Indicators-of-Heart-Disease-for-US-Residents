getwd()
setwd("C:/Users/monic/OneDrive/Desktop/Tesis")

library("tidyverse")
library("caret")
library("tidymodels")
library("themis")
library("rpart.plot")
library("ranger")


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

#Change order of levels
dat$HeartDisease <- factor(dat$HeartDisease, levels = c("1","0"))



#Build Model
set.seed(123)
HD_split <- initial_split(dat, strata = HeartDisease)
HD_train <- training(HD_split)
HD_test <- testing(HD_split)



#Verify Balanced or Imbalanced Scenario
HD_train %>% count(HeartDisease) #Very imbalanced

#10-Fold CV
set.seed(234)
HD_folds <- vfold_cv(HD_train, strata = "HeartDisease")

#Set Metrics
HD_metrics <- metric_set(roc_auc, j_index, accuracy, f_meas, 
                         precision, sensitivity, specificity)

#Handling Imbalanced Scenario 
HD_up_rec <- recipe(HeartDisease ~., data = HD_train) %>%
  step_upsample(HeartDisease)
HD_smote_rec <- recipe(HeartDisease ~., data = HD_train) %>%
  step_smote(HeartDisease)
HD_down_rec <- recipe(HeartDisease ~., data = HD_train) %>%
  step_downsample(HeartDisease)
#With nothing
HD_NA_rec <- recipe(HeartDisease~., data = HD_train)


##########LOGISTIC###################


#Logistic Specification
log_spec <- logistic_reg() %>%
  set_engine("glm")


#Get Results
#With no method

HD_wf <- workflow() %>%
  add_recipe(HD_NA_rec)

set.seed(345)
log_rs_NA <- HD_wf %>%
  add_model(log_spec) %>%
  fit_resamples(
    resamples = HD_folds,
    metrics = HD_metrics,
    control = control_resamples(save_pred = T, verbose = T)
  )
collect_metrics(log_rs_NA)
#FinalModel
final_res <- last_fit(HD_wf %>%
                        add_model(log_spec), 
                      HD_split, 
                      metrics = HD_metrics)
collect_metrics(final_res)

#upsampling
HD_wf <- workflow() %>%
  add_recipe(HD_up_rec)

set.seed(456)
log_rs_up <- HD_wf %>%
  add_model(log_spec) %>%
  fit_resamples(
    resamples = HD_folds,
    metrics = HD_metrics,
    control = control_resamples(save_pred = T, verbose = T)
  )
collect_metrics(log_rs_up)
#FinalModel
final_res <- last_fit(HD_wf %>%
                        add_model(log_spec), 
                        HD_split, 
                        metrics = HD_metrics)
collect_metrics(final_res)

#Smote
HD_wf <- workflow() %>%
  add_recipe(HD_smote_rec)

set.seed(567)
log_rs_smote <- HD_wf %>%
  add_model(log_spec) %>%
  fit_resamples(
    resamples = HD_folds,
    metrics = HD_metrics,
    control = control_resamples(save_pred = T, verbose = T)
  )
collect_metrics(log_rs_smote)
#FinalModel
final_res <- last_fit(HD_wf %>%
                        add_model(log_spec), 
                      HD_split, 
                      metrics = HD_metrics)
collect_metrics(final_res)

#downsampling
HD_wf <- workflow() %>%
  add_recipe(HD_down_rec)

set.seed(678)
log_rs_down <- HD_wf %>%
  add_model(log_spec) %>%
  fit_resamples(
    resamples = HD_folds,
    metrics = HD_metrics,
    control = control_resamples(save_pred = T, verbose = T)
  )
collect_metrics(log_rs_down)
#FinalModel
final_res <- last_fit(HD_wf %>%
                        add_model(log_spec), 
                      HD_split, 
                      metrics = HD_metrics)
collect_metrics(final_res)


##########Decision Tree###################


#Set Grid
tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 5)

#Model Specification
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

#Workflow
HD_wf <- workflow() %>%
  add_model(tree_spec)

#Results
#Upsampling
set.seed(456)
tree_rs_up <- tune_grid(
  HD_wf %>%
    add_recipe(HD_up_rec),
  HeartDisease ~.,
  resamples = HD_folds,
  grid = tree_grid,
  control = control_resamples(save_pred = T, verbose = T),
  metrics = HD_metrics
)
collect_metrics(tree_rs_up)
#Selecting Best ROC_AUC
dt_roc <- select_best(tree_rs_smote, "roc_auc")
final_dt_roc <- finalize_model(tree_spec, dt_roc)
HD_wf <- workflow() %>% add_recipe(HD_smote_rec)
HD_wf <- HD_wf %>% add_model(final_dt_roc)
final_dt <- finalize_workflow(
  HD_wf,
  dt_roc_
)
final_dt
final_res <- last_fit(final_dt, HD_split, metrics = HD_metrics)
collect_metrics(final_res)

#Smote
tree_rs_smote <- tune_grid(
  HD_wf %>%
    add_recipe(HD_smote_rec),
  HeartDisease ~.,
  resamples = HD_folds,
  grid = tree_grid,
  control = control_resamples(save_pred = T, verbose = T),
  metrics = HD_metrics
)
collect_metrics(tree_rs_smote)
#Selecting Best ROC_AUC
dt_roc <- select_best(tree_rs_smote, "roc_auc")
final_dt_roc <- finalize_model(tree_spec, dt_roc)
HD_wf <- workflow() %>% add_recipe(HD_smote_rec)
HD_wf <- HD_wf %>% add_model(final_dt_roc)
final_dt <- finalize_workflow(
  HD_wf,
  dt_roc_
)
final_dt
final_res <- last_fit(final_dt, HD_split, metrics = HD_metrics)
collect_metrics(final_res)

#Downsampling
tree_rs_down <- tune_grid(
  HD_wf %>%
    add_recipe(HD_down_rec),
  HeartDisease ~.,
  resamples = HD_folds,
  grid = tree_grid,
  control = control_resamples(save_pred = T, verbose = T),
  metrics = HD_metrics
)
collect_metrics(tree_rs_down)
#Selecting Best ROC_AUC
dt_roc <- select_best(tree_rs_smote, "roc_auc")
final_dt_roc <- finalize_model(tree_spec, dt_roc)
HD_wf <- workflow() %>% add_recipe(HD_smote_rec)
HD_wf <- HD_wf %>% add_model(final_dt_roc)
final_dt <- finalize_workflow(
  HD_wf,
  dt_roc_
)
final_dt
final_res <- last_fit(final_dt, HD_split, metrics = HD_metrics)
collect_metrics(final_res)

#No Method
tree_rs_NA <- tune_grid(
  HD_wf %>%
    add_recipe(HD_NA_rec),
  HeartDisease ~.,
  resamples = HD_folds,
  grid = tree_grid,
  control = control_resamples(save_pred = T, verbose = T),
  metrics = HD_metrics
)
collect_metrics(tree_rs_NA)
#Selecting Best ROC_AUC
dt_roc <- select_best(tree_rs_smote, "roc_auc")
final_dt_roc <- finalize_model(tree_spec, dt_roc)
HD_wf <- workflow() %>% add_recipe(HD_smote_rec)
HD_wf <- HD_wf %>% add_model(final_dt_roc)
final_dt <- finalize_workflow(
  HD_wf,
  dt_roc_
)
final_dt
final_res <- last_fit(final_dt, HD_split, metrics = HD_metrics)
collect_metrics(final_res)



