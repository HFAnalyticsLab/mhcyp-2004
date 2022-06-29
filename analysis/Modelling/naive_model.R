# see p32 of technical report on conduct model
# naive baseline model without weights

library(tidyverse)
library(survey)
library(foreign)
library(ROCR)
library(caret)
library(ppcor)
library(broom)
library(jtools)

## define functions for use later on

# intrinsic model performance
model_performance <- function(myModel, myData) { # this will always be the training set
  # R²
  pseudo_r2 <- 1 - (myModel$deviance / myModel$null.deviance)
  print(paste("Pseudo R²:", round(pseudo_r2, 3)))
  
  # Model p value
  nmod <- glm(anycd_ic ~ 1, family = "binomial", data = myData) # "null" mod
  myAnova <- anova(nmod, myModel, test = "Chisq")
  print(paste("Model p value:", signif(myAnova$`Pr(>Chi)`[-1], 3)))
}

# ROC curve
plot_roc <- function(myModel, myData) {
  if ("svyglm" %in% class(myModel)) {
    myPrediction <- as.data.frame(predict(myModel, myData, type = "response"))$response
  } else {
    myPrediction <- predict(myModel, myData, type = "response")  
  }
  pred <- prediction(myPrediction,
                     myData$anycd_ic,
                     label.ordering = c("No disorder", "Disorder present"))
  perf <- performance(pred,"tpr","fpr")
  plot(perf,colorize=TRUE)
  auc <- performance(pred, measure = "auc")
  print(paste("AUC:", round(auc@y.values[[1]], 2)))
}

# confusion matrix given different prediction thresholds
confusion_matrix <- function(myModel, myData, myThreshold = 0.5) {
  p <- predict(myModel, myData, type = "response")
  cd_or_nocd <- ifelse(p > myThreshold, 2, 1)
  p_class <- factor(cd_or_nocd,
                    labels = levels(myData[["anycd_ic"]]))
  confusionMatrix(p_class, myData[["anycd_ic"]], positive = "Disorder present")
}

# LOO CV by region
predict_region <- function(my_region, my_formula) {
  train <- filter(yr2004, gormain != my_region)
  test <- filter(yr2004, gormain == my_region)
  
  region_model <- glm(my_formula,
                      data = train,
                      family = "binomial")
  
  region_predictions <- predict(region_model, test, type = "response")
  mean(region_predictions)
}


## now let's do some modelling!
# estimate a basic model
baseline <- glm(anycd_ic ~ chldsex + chldage + simple_eth + completed_hhinc,
                data = train,
                family = "binomial")
summary(baseline)

simple <- glm(anycd_ic ~ chldsex + chldage + bame + hh_inc_over_400pw,
                data = train,
                family = "binomial")
summary(simple)
tidy(simple)
glance(simple) %>%
  bind_rows(glance(baseline))

model_performance(baseline, train)
model_performance(simple, train)

plot_roc(baseline, train)
plot_roc(simple, train)
plot_roc(baseline, test)
plot_roc(simple, test)

confusion_matrix(baseline, train, 0.1)
confusion_matrix(baseline, test, 0.1)
confusion_matrix(simple, test, 0.1)

# beefed-up model
beefed_up <- glm(anycd_ic ~ chldsex + chldage + simple_eth + completed_hhinc +
                   income_benefit + persistent_absence + sen.comlang + sen.menhealth + excluded,
                 data = train,
                 family = "binomial")
summary(beefed_up)

model_performance(beefed_up, train)
plot_roc(beefed_up, train)
plot_roc(beefed_up, test)

confusion_matrix(beefed_up, test)
confusion_matrix(beefed_up, test, 0.1)

# simple beefed-up model
beefed_up_simple <- glm(anycd_ic ~ chldsex + chldage + bame + hh_inc_over_400pw +
                   income_benefit + persistent_absence + sen.comlang + sen.menhealth + excluded,
                 data = train,
                 family = "binomial")
summary(beefed_up_simple)

model_performance(beefed_up_simple, train)
plot_roc(beefed_up_simple, train)
plot_roc(beefed_up_simple, test)

confusion_matrix(beefed_up_simple, test)
confusion_matrix(beefed_up_simple, test, 0.1)

# beefed-up 2 - even more features
beefed_up2 <- glm(anycd_ic ~ chldsex + chldage + simple_eth + completed_hhinc +
                   income_benefit + persistent_absence + sen.comlang + sen.menhealth + excluded +
                   lone.parent + at.least.4.children + no.qualifications + hh.unemployment.rate + social.tenants + disability.benefits,
                 data = train,
                 family = "binomial")
summary(beefed_up2)

model_performance(beefed_up2, train)
plot_roc(beefed_up2, train)
plot_roc(beefed_up2, test)

confusion_matrix(beefed_up2, test)
confusion_matrix(beefed_up2, test, 0.2)

# simple beefed-up model with *some* of the extra features
beefed_up_simple2 <- glm(anycd_ic ~ chldsex + chldage + bame + hh_inc_over_400pw +
                          income_benefit + persistent_absence + sen.comlang + sen.menhealth + excluded +
                          social.tenants + disability.benefits,
                        data = train,
                        family = "binomial")
summary(beefed_up_simple2)

model_performance(beefed_up_simple2, train)
plot_roc(beefed_up_simple2, train)
plot_roc(beefed_up_simple2, test)

confusion_matrix(beefed_up_simple2, test)
confusion_matrix(beefed_up_simple2, test, 0.1)

# fully interacted - overfit!
interacted <- glm(anycd_ic ~ chldsex * chldage * bame * hh_inc_over_400pw, # * income_benefit * persistent_absence * sen.comlang * sen.menhealth * excluded,
                 data = train,
                 family = "binomial")
summary(interacted)

model_performance(interacted, train)
plot_roc(interacted, train)
plot_roc(interacted, test)

confusion_matrix(interacted, test)
confusion_matrix(interacted, test, 0.1)

# survey weights
svy2004 <- svydesign(id = ~1, weights = ~weightgr, data = train)
beefed_up_simple2_survey <- svyglm(anycd_ic ~ chldsex + chldage + bame + hh_inc_over_400pw +
                                 income_benefit + persistent_absence + sen.comlang + sen.menhealth + excluded +
                                 social.tenants + disability.benefits,
                               design = svy2004,
                               family = "binomial") #, rescale = TRUE)
summary(beefed_up_simple2_survey)
# sen.comlang and age less significant, truancy more significant

plot_roc(beefed_up_simple2, test)
plot_roc(beefed_up_simple2_survey, test)
# but using survey weights has an absolutely marginal effect on the predictions

confusion_matrix(beefed_up_simple2_survey, test, 0.1)

# round 3!! taking beefed_up_simple2 and trying 3 new variables
beefed_up_simple3 <- glm(anycd_ic ~ chldsex + chldage + bame + hh_inc_over_400pw +
                           income_benefit + persistent_absence + sen.comlang + sen.menhealth + excluded +
                           social.tenants + disability.benefits +
                           chldsmokes + contact_with_specialist + behind_on_spelling,
                         data = train,
                         family = "binomial")
summary(beefed_up_simple3)

model_performance(beefed_up_simple3, train)
plot_roc(beefed_up_simple3, train)
plot_roc(beefed_up_simple3, test)

confusion_matrix(beefed_up_simple3, test)
confusion_matrix(beefed_up_simple3, test, 0.2)

# output summary
export_summs(simple, beefed_up_simple3, pvals = FALSE)
export_summs(simple, beefed_up_simple3, to.file = "docx", file.name = "test.docx")

# formulas for use below
baseline_formula <- "anycd_ic ~ chldsex + chldage + simple_eth + completed_hhinc"
beefed_up_formula <- "anycd_ic ~ chldsex + chldage + simple_eth + completed_hhinc + income_benefit + persistent_absence + sen.comlang + sen.menhealth + excluded"
beefed_up2_formula <- "anycd_ic ~ chldsex + chldage + simple_eth + completed_hhinc + income_benefit + persistent_absence + sen.comlang + sen.menhealth + excluded + lone.parent + at.least.4.children + no.qualifications + hh.unemployment.rate + social.tenants + disability.benefits"
beefed_up_simple2_formula <-"anycd_ic ~ chldsex + chldage + bame + hh_inc_over_400pw + income_benefit + persistent_absence + sen.comlang + sen.menhealth + excluded + social.tenants + disability.benefits"
beefed_up_simple3_formula <-"anycd_ic ~ chldsex + chldage + bame + hh_inc_over_400pw + income_benefit + persistent_absence + sen.comlang + sen.menhealth + excluded + social.tenants + disability.benefits + chldsmokes + contact_with_specialist + behind_on_spelling"
interacted_formula <- "anycd_ic ~ chldsex * chldage * simple_eth * completed_hhinc * income_benefit * persistent_absence * sen.comlang * sen.menhealth * excluded"


# let's try LOO CV by region and see how that predicts...
my_regions <- unique(yr2004$gormain)
my_region <- my_regions[1]

map_dbl(my_regions, predict_region, my_formula = baseline_formula)
map_dbl(my_regions, predict_region, my_formula = beefed_up_formula)
map_dbl(my_regions, predict_region, my_formula = beefed_up_simple2_formula)

regional_predictions <- data.frame(
  region = my_regions,
  predicted_rate = map_dbl(my_regions, predict_region, beefed_up_simple3_formula) # alter the formula here
) %>%
  inner_join(
    yr2004 %>%
      dplyr::select(gormain, anycd_ic) %>%
      mutate(observed_cd = if_else(anycd_ic == "No disorder", 0, 1)) %>%
      group_by(gormain) %>%
      summarise(observed_rate = mean(observed_cd)),
    by = c("region" = "gormain")
  )

regional_predictions %>%
  pivot_longer(cols = c(2, 3), names_to = "type") %>%
  ggplot(aes(x = region, y = value, fill = type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(name = "Rate",
                     labels = scales::percent_format()) +
  geom_hline(yintercept = mean(if_else(yr2004$anycd_ic == "No disorder", 0, 1)))

regional_predictions %>%
  summarise_all(mean)

regional_predictions %>%
  mutate(error = observed_rate - predicted_rate,
         absolute_error = abs(error),
         squared_error = error ^ 2) %>%
  summarise(mse = mean(squared_error),
            mae = mean(absolute_error)) %>%
  mutate(rmse = sqrt(mse))
# baseline: RMSE of 1.6% by region, MAE of 1.0%
# beefed-up: RMSE of 1.2% by region, MAE of 0.9%
# beefed-up simple 3: RMSE 1.2%, MAE of 0.8%

ggplot(regional_predictions, aes(x = observed_rate, y = predicted_rate)) +
  geom_point()


# what's the RMSE of guessing the mean?
regional_predictions %>%
  mutate(avg_rate = mean(if_else(yr2004$anycd_ic == "No disorder", 0, 1)),
         error = observed_rate - avg_rate,
         absolute_error = abs(error),
         squared_error = error ^ 2) %>%
  summarise(mse = mean(squared_error),
            mae = mean(absolute_error)) %>%
  mutate(rmse = sqrt(mse))
# RMSE of 1.3% guessing average, MAE of 0.9%
# but is that because of variation in the region size - could using the survey weights address this?

regional_predictions %>%
  mutate(avg_rate = mean(if_else(yr2004$anycd_ic == "No disorder", 0, 1)),
         pred_error = round(abs(observed_rate - predicted_rate), 4),
         avg_error = round(abs(observed_rate - avg_rate), 4)) %>% View()

yr2004 %>%
  count(gormain)
# NW and SE big, NE and Wales small


