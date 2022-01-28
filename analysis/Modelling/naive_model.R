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

main <- read.spss("\\\\thf-rds-fsvm01\\ode_data\\Analytics\\Tom\\MHCYP 2004\\UKDA-5269-spss\\spss\\spss12\\cpm9904.sav",
                  to.data.frame = TRUE)

# filter and make any grouped variables etc. (feature engineering)
yr2004 <- main %>%
  filter(sampyear == 2004) %>%
  mutate(conduct.disorder = if_else(anycd_ic == "Disorder present", 1, 0),
         male = if_else(chldsex == "Male", 1, 0),
         age_over_10 = chldage - 10,
         ethgpc1 = as.character(ethgpc1),
         hhinc2 = as.character(hhinc2),
         simple_eth = case_when(
           str_detect(ethgpc1, "Black") ~ "Black",
           ethgpc1 %in% c("Pakistani", "Bangladeshi", " Indian") ~ "Asian",
           is.na(ethgpc1) ~ "Other",
           TRUE ~ ethgpc1),
         bame = if_else(simple_eth == "White", 0, 1),
         completed_hhinc = if_else(is.na(hhinc2), "Refused income", hhinc2),
         hh_inc_over_400pw = if_else(hhinc2 %in% c("Over 770", "600.00-770.00", "500.00-599.00", "400.00-499.00"), 1, 0)
         )

# add extra features
yr2004$persistent_absence <- 0
yr2004$persistent_absence[which(yr2004$absence == "16 days and over")] <- 1
yr2004$income_benefit <- 0
yr2004$income_benefit[which(yr2004$ben2q1 %in% c("Income Support ^MIG_Txt", "Job Seekers  Allowance(JSA)", "^PC_Txt") | # I *think* these are the 3 income benefits
                              yr2004$ben2q2 %in% c("Income Support ^MIG_Txt", "Job Seekers  Allowance(JSA)", "^PC_Txt") |
                              yr2004$txcred1 %in% c("Working Tax Credit (excluding any childc", "Child Tax Credit (including any childcar") |
                              yr2004$txcred2 %in% c("Working Tax Credit (excluding any childc", "Child Tax Credit (including any childcar"))] <- 1
yr2004$sen.comlang <- ifelse(is.na(yr2004$tneedsc), 1, yr2004$tneedsc) - 1
yr2004$sen.menhealth <- ifelse(is.na(yr2004$tneedsb), 1, yr2004$tneedsb) - 1
yr2004$excluded <- abs(ifelse(is.na(yr2004$excever), 1, yr2004$excever) - 2)

# extra features - 2nd round
yr2004$lone.parent <- ifelse(is.na(yr2004$lonepar), 0, 1)
yr2004$at.least.4.children <- ifelse(yr2004$numch18 >= 4, 1, 0)
yr2004$no.qualifications <- ifelse(!is.na(yr2004$anyquals) & yr2004$anyquals == "Yes", 0, 1)
yr2004 <- yr2004 %>%
  mutate(temp.p1.working = case_when(
    str_detect(ecostat2, "^WORKING") ~ 1,
    ecostat2 == "UNEMPLOYED" ~ 0,
    TRUE ~ NA_real_),
    temp.p2.working = case_when(
      str_detect(pecosta2, "^WORKING") ~ 1,
      ecostat2 == "UNEMPLOYED" ~ 0,
      TRUE ~ NA_real_),
    temp.sum = rowSums(across(starts_with("temp.p")), na.rm = TRUE)
  ) %>%
  rowwise() %>%
  mutate(temp.count = 2 - sum(is.na(c_across(starts_with("temp.p"))))) %>%
  ungroup() %>%
  mutate(temp.hh.employment_rate = if_else(temp.count == 0, 0, temp.sum / temp.count),
         hh.unemployment.rate = 1 - temp.hh.employment_rate) %>%
  select(-starts_with("temp."))
yr2004$social.tenants <- ifelse(!is.na(yr2004$tenure2) & yr2004$tenure2 == "SOCIAL SECTOR TENANTS", 1, 0)
yr2004$disability.benefits <- 0
yr2004$disability.benefits <- ifelse(!is.na(yr2004$disben1) & yr2004$disben1 != "None of these", 1, yr2004$disability.benefits)
yr2004$disability.benefits <- ifelse(!is.na(yr2004$disben2) & yr2004$disben2 != "None of these", 1, yr2004$disability.benefits)

# extra features - 3rd round
yr2004$chldsmokes <- ifelse(is.na(yr2004$c3e1) | yr2004$c3e1 == "No", 0, 1)
yr2004 <- yr2004 %>%
  mutate(contact_with_specialist = case_when(
    whhelp01 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp02 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp03 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp04 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp05 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp06 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp07 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp08 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp09 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp10 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp11 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp12 %in% c("Social worker", "Someone working in special educational s", "Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    TRUE ~ 0)
  )
yr2004$behind_on_spelling <- ifelse(!is.na(yr2004$da1c) & yr2004$da1c %in% c("some difficulty", "marked difficulty"), 1, 0)

# some problematic feature engineering here - bit of lumping unknown ethnicity to other
# worse is how we deal with grouped income and lots of missing values

# how to adjust survey weights to incorporate adjustment factor
# to adjust unweighted estimates, just replace weightsc with 1 throughout
foo <- yr2004 %>%
  count(anycd_ic, wt = weightsc)
bar <- prop.test(foo$n[2], foo$n[1] + foo$n[2])

weighted_prevalence <- bar$estimate
true_prevalence <- bar$estimate * 1.13

yr2004$myweight <- ifelse(yr2004$anycd_ic == "Disorder present", yr2004$weightsc * true_prevalence / weighted_prevalence, yr2004$weightsc * (1 - true_prevalence) / (1 - weighted_prevalence))
rm(foo, bar, weighted_prevalence, true_prevalence)

# do a train/test split to assess performance
table(yr2004$anycd_ic)
set.seed(5269)
train <- yr2004 %>%
  sample_frac(0.8)
test <- anti_join(yr2004, train, by = "studyno")

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

model_performance(baseline, train)
model_performance(simple, train)

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

plot_roc(baseline, train)
plot_roc(simple, train)
plot_roc(baseline, test)
plot_roc(simple, test)

confusion_matrix <- function(myModel, myData, myThreshold = 0.5) {
  p <- predict(myModel, myData, type = "response")
  cd_or_nocd <- ifelse(p > myThreshold, 2, 1)
  p_class <- factor(cd_or_nocd,
                    labels = levels(myData[["anycd_ic"]]))
  confusionMatrix(p_class, myData[["anycd_ic"]], positive = "Disorder present")
}

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

predict_region <- function(my_region, my_formula) {
  train <- filter(yr2004, gormain != my_region)
  test <- filter(yr2004, gormain == my_region)
  
  region_model <- glm(my_formula,
                      data = train,
                      family = "binomial")
  
  region_predictions <- predict(region_model, test, type = "response")
  mean(region_predictions)
}

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


