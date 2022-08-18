# load and transform the data
# run this before any of the EDA / modelling scripts

library(tidyverse)
library(survey)
library(foreign)

main <- read.spss("cpm9904.sav", to.data.frame = TRUE)

# filter and make any grouped variables etc. (feature engineering)
yr2004 <- main %>%
  filter(sampyear == 2004) %>%
  mutate(conduct.disorder = if_else(anycd_ic == "Disorder present", 1, 0),
            weekly_hhldinc = case_when(
             hhldinc == "Less than 1000" ~ 500/52,
             hhldinc == "2,000 to 2,999" ~ 2499.5/52,
             hhldinc == "3,000 to 3,999" ~ 3499.5/52,
             hhldinc == "4,000 to 4,999" ~ 4499.5/52,
             hhldinc == "5,000 to 5,999" ~ 5499.5/52,
             hhldinc == "6,000 to 6,999" ~ 6499.5/52,
             hhldinc == "7,000 to 7,999" ~ 7499.5/52,
             hhldinc == "8,000 to 8,999" ~ 8499.5/52,
             hhldinc == "9,000 to 9,999" ~ 9499.5/52,
             hhldinc == "10,000 to 10,999" ~ 10499.5/52, 
             hhldinc == "11,000 to 11,999"~ 11499.5/52,
             hhldinc == "12,000 to 12,999"~ 12499.5/52,
             hhldinc == "13,000 to 13,999"~ 13499.5/52,
             hhldinc == "14,000 to 14,999"~ 14499.5/52,
             hhldinc == "15,000 to 17,499"~ 16249.5/52,
             hhldinc == "17,500 to 19,999"~ 18749.5/52,
             hhldinc == "20,000 to 24,999"~ 22499.5/52,
             hhldinc == "25,000 to 29,999"~ 27499.5/52,
             hhldinc == "30,000 to 39,999"~ 34999.5/52,
             hhldinc == "40,000 or more"~ 40000/52),
         group_A = numadult ,
         group_B = numchild ,
         group_A1 = if_else(chldage >= 14, group_A + 1 , group_A) ,
         group_B1 = if_else(chldage >= 14, group_B - 1, group_B) ,
         under_14s = group_B1*7/8,
         over_14s = group_A1 + group_B1/8,
         male = if_else(chldsex == "Male", 1, 0),
         age_over_10 = chldage - 10,
         over_11 = if_else(chldage > 10 , 1, 0),
         ethgpc1 = as.character(ethgpc1),
         hhinc2 = as.character(hhinc2),
         simple_eth = case_when(
           str_detect(ethgpc1, "Black") ~ "Black",
           ethgpc1 %in% c("Pakistani", "Bangladeshi", " Indian") ~ "Asian",
           is.na(ethgpc1) ~ "Other",
           TRUE ~ ethgpc1),
         bame = if_else(simple_eth == "White", 0, 1),
         completed_hhinc = if_else(is.na(hhinc2), "Refused income", hhinc2),
         hh_inc_over_400pw = if_else(hhinc2 %in% c("Over 770", "600.00-770.00", "500.00-599.00", "400.00-499.00"), 1, 0),
         equivalised_income = weekly_hhldinc / (under_14s*0.2 + ((over_14s+1)*0.33))) %>%
         select(-c(group_A,group_A1, group_B, group_B1)) 


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

# adding income_deprived flag

yr2004 <- yr2004 %>% 
  mutate(income_deprived = if_else( income_benefit == 1 | equivalised_income <218 , 1, 0) )

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

# 4th round!
yr2004 <- yr2004 %>%
  mutate(contact_with_primary_care = case_when(
    whhelp01 == "Your GP, family doctor or practice nurse" ~ 1,
    whhelp02 == "Your GP, family doctor or practice nurse" ~ 1,
    whhelp03 == "Your GP, family doctor or practice nurse" ~ 1,
    whhelp04 == "Your GP, family doctor or practice nurse" ~ 1,
    whhelp05 == "Your GP, family doctor or practice nurse" ~ 1,
    whhelp06 == "Your GP, family doctor or practice nurse" ~ 1,
    whhelp07 == "Your GP, family doctor or practice nurse" ~ 1,
    whhelp08 == "Your GP, family doctor or practice nurse" ~ 1,
    whhelp09 == "Your GP, family doctor or practice nurse" ~ 1,
    whhelp10 == "Your GP, family doctor or practice nurse" ~ 1,
    whhelp11 == "Your GP, family doctor or practice nurse" ~ 1,
    whhelp12 == "Your GP, family doctor or practice nurse" ~ 1,
    TRUE ~ 0)
  )

yr2004 <- yr2004 %>%
  mutate(contact_with_mhs = case_when(
    whhelp01 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp02 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp03 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp04 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp05 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp06 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp07 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp08 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp09 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp10 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp11 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    whhelp12 %in% c("Someone specialising in child mental hea", "Someone specialising in adult mental hea") ~ 1,
    TRUE ~ 0)
  )

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
