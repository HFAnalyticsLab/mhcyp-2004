# this script is to explore specific behavioural factors, beyond the basic demographics

library(tidyverse)
library(survey)

# weights based on age, sex & region
prop.table(table(yr2004$anycd_ic))

# main risk factors: https://dub01.online.tableau.com/#/site/gstc-tableau/views/AdolescentMentalHealth_16075088310220/Methodology?:iid=1
# can't get birth weight, or Early Years Foundation level of development, or educational attainment
# IMD not available, but is in 2017

# expulsion - excever == "Yes"
table(yr2004$anycd_ic, yr2004$excever) %>% prop.table(margin = 2)
table(yr2004$anycd_ic, yr2004$excever) %>% prop.table()

# truancy - absence / absence1 for Yes/No
table(yr2004$anycd_ic, yr2004$absence1) %>% prop.table(margin = 2)
table(yr2004$anycd_ic, yr2004$absence) %>% prop.table(margin = 2)
yr2004$persistent_absence <- 0
yr2004$persistent_absence[which(yr2004$absence == "16 days and over")] <- 1
table(yr2004$anycd_ic, yr2004$persistent_absence) %>% prop.table(margin = 2)

# benefits (proxy for FSM & IDACI) ben2q1 / ben2q2 IN () OR txcred1/2
# should be a rate like unemployment rate?
table(yr2004$txcred1, useNA = "ifany") # much more tax credits back then
yr2004$income_benefit <- 0
yr2004$income_benefit[which(yr2004$ben2q1 %in% c("Income Support ^MIG_Txt", "Job Seekers  Allowance(JSA)", "^PC_Txt") | # I *think* these are the 3 income benefits
      yr2004$ben2q2 %in% c("Income Support ^MIG_Txt", "Job Seekers  Allowance(JSA)", "^PC_Txt") |
      yr2004$txcred1 %in% c("Working Tax Credit (excluding any childc", "Child Tax Credit (including any childcar") |
      yr2004$txcred2 %in% c("Working Tax Credit (excluding any childc", "Child Tax Credit (including any childcar"))] <- 1

table(yr2004$anycd_ic, yr2004$income_benefit) %>% prop.table(margin = 2)

# special educational needs - communication & language - tneedsc
table(yr2004$anycd_ic, ifelse(is.na(yr2004$tneedsc), 1, yr2004$tneedsc)) %>% # treat missings as nos
  prop.table(margin = 2) # 1 is No, 2 is Yes
yr2004$sen.comlang <- ifelse(is.na(yr2004$tneedsc), 1, yr2004$tneedsc) - 1
table(yr2004$anycd_ic, yr2004$sen.comlang) %>% prop.table(margin = 2)


# special educational needs - emotional & mental health - tneedsb
table(yr2004$anycd_ic, yr2004$tneedsb)
table(yr2004$anycd_ic, ifelse(is.na(yr2004$tneedsb), 1, yr2004$tneedsb)) %>%
  prop.table(margin = 2)
yr2004$sen.menhealth <- ifelse(is.na(yr2004$tneedsb), 1, yr2004$tneedsb) - 1
table(yr2004$anycd_ic, yr2004$tneedsb) %>% prop.table(margin = 2)

# any special educational needs
table(yr2004$anycd_ic, yr2004$tneeds) %>% prop.table()

# SEN & school exclusions
table(yr2004$excever, yr2004$tneeds) %>% prop.table()

# could also try urban / rural classification - metropolitan counties
table(yr2004$gorgrp)
table(yr2004$anycd_ic, yr2004$gorgrp) %>% prop.table(margin = 2)

table(ifelse(yr2004$gorgrp %in% c("London inner", "London outer", "Other MET England", "Glasgow"), "Urban", "Rural"))
table(yr2004$anycd_ic, ifelse(yr2004$gorgrp %in% c("London inner", "London outer", "Other MET England", "Glasgow"), "Urban", "Rural")) %>%
  prop.table(margin = 2)
# least helpful of all these!

## SECOND ROUND
# single parents
table(yr2004$dvmardf, useNA = "ifany")
table(yr2004$marstat, yr2004$dvmardf, useNA = "ifany")
table(yr2004$lonepar, useNA = "ifany")

yr2004$lone.parent <- ifelse(is.na(yr2004$lonepar), 0, 1)
table(yr2004$lone.parent, useNA = "ifany")

table(yr2004$anycd_ic, yr2004$lone.parent) %>%
  prop.table(margin = 2)

# family size
table(yr2004$anycd_ic, yr2004$dvhsize, useNA = "ifany") %>%
  prop.table(margin = 2) # doesn't correlate to simple household size
table(yr2004$anycd_ic, yr2004$numch18, useNA = "ifany") %>%
  prop.table(margin = 2) # rates worse for 4+ children in household
table(yr2004$dvhsize, yr2004$numch18)

yr2004$at.least.4.children <- ifelse(yr2004$numch18 >= 4, 1, 0)
table(yr2004$at.least.4.children, useNA = "ifany")

table(yr2004$anycd_ic, yr2004$at.least.4.children) %>%
  prop.table(margin = 2)

# parents no qualifications
table(yr2004$anyquals, useNA = "ifany") # can't find any field for 2nd parent
yr2004$no.qualifications <- 1
yr2004$no.qualifications <- ifelse(yr2004$anyquals == "Yes" | is.na(yr2004$anyquals), 0, yr2004$no.qualifications) # assume missings DO have quals

table(yr2004$no.qualifications, useNA = "ifany")

table(yr2004$anycd_ic, yr2004$no.qualifications) %>%
  prop.table(margin = 2)

# parents both unemployed - good marker but we'll never get open data on that
# better to calculate an "unemployment rate" for each household based on the first 2 adults
table(yr2004$pdvjb12m, useNA = "ifany") # lots of missings
table(yr2004$pdvjb122, useNA = "ifany")
table(yr2004$inckind1, useNA = "ifany")
table(yr2004$inckind2, useNA = "ifany")
table(yr2004$dvilo3, useNA = "ifany")
table(yr2004$pdvilo3, useNA = "ifany")
table(yr2004$ecostat2, useNA = "ifany")
table(yr2004$pecosta2, useNA = "ifany")

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
         hh.unemployment_rate = 1 - temp.hh.employment_rate) %>%
  select(-starts_with("temp."))

table(yr2004$anycd_ic, yr2004$hh.unemployment_rate) %>%
  prop.table(margin = 2)

# social rent
table(yr2004$ten1)
table(yr2004$tenure)
table(yr2004$tenure2)
table(yr2004$llord)

yr2004$social.tenants <- ifelse(!is.na(yr2004$tenure2) & yr2004$tenure2 == "SOCIAL SECTOR TENANTS", 1, 0)
table(yr2004$anycd_ic, yr2004$social.tenants) %>%
  prop.table(margin = 2)
table(yr2004$social.tenants, useNA = "ifany")

# disability benefits - do simple 0/1 for now - but maybe should be a rate
table(yr2004$disben1, useNA = "ifany")
table(yr2004$disben2, useNA = "ifany")
yr2004$disability.benefits <- 0
yr2004$disability.benefits <- ifelse(!is.na(yr2004$disben1) & yr2004$disben1 != "None of these", 1, yr2004$disability.benefits)
yr2004$disability.benefits <- ifelse(!is.na(yr2004$disben2) & yr2004$disben2 != "None of these", 1, yr2004$disability.benefits)

table(yr2004$disability.benefits, useNA = "ifany")

# just checking - ZERO cases where 1st parent not on dis bens but 2nd parent is
yr2004$disben2[which(is.na(yr2004$disben1) | yr2004$disben1 == "None of these")] %>%
  unique()

# neighbourhood deprivation - they used ACORN, but it's not in the dataset
# step children - apparently a good predictor, but is there open data on this??

# third round!!!
# smoking
table(yr2004$chldage, yr2004$c3e1, useNA = "ifany")
table(yr2004$anycd_ic, yr2004$c3e1, useNA = "ifany") %>% prop.table(2)
yr2004$chldsmokes <- ifelse(is.na(yr2004$c3e1) | yr2004$c3e1 == "No", 0, 1)
table(yr2004$chldsmokes)

# in contact with mental health services
table(yr2004$anycd_ic, yr2004$whhelp01, useNA = "ifany") %>% prop.table(2) # these go up to whhelp12 to cover all possible bases
table(yr2004$anycd_ic, yr2004$contact_with_specialist, useNA = "ifany") %>% prop.table(2)
table(yr2004$any_ic, yr2004$contact_with_specialist, useNA = "ifany") %>% prop.table(2)

# spelling
table(yr2004$da1c, useNA = "ifany")
table(yr2004$behind_on_spelling, useNA = "ifany")
table(yr2004$anycd_ic, yr2004$behind_on_spelling) %>% prop.table(2)

# SEN - needs vars from naive_model
table(yr2004$anycd_ic, yr2004$tneedsc)
yr2004 %>%
  count(sen.comlang, anycd_ic) %>%
  group_by(sen.comlang) %>%
  mutate(prop = prop.table(n),
         total = sum(n)) %>%
  filter(anycd_ic == "Disorder present")

plot_cd_rates(yr2004, sen.comlang) +
  scale_x_continuous(name = "Special needs: communication & language",
                     labels = c("No", "", "", "", "Yes"),
                     breaks = c(0, 0, 0, 0, 1))

plot_cd_rates(yr2004, sen.menhealth) +
  scale_x_continuous(name = "Special needs: behavioural & emotional",
                     labels = c("No", "", "", "", "Yes"),
                     breaks = c(0, 0, 0, 0, 1))

yr2004 %>%
  count(sen.menhealth, anycd_ic) %>%
  group_by(sen.menhealth) %>%
  mutate(prop = prop.table(n),
         total = sum(n)) %>%
  filter(anycd_ic == "Disorder present")

yr2004 %>%
  count(sen.comlang, sen.menhealth, anycd_ic) %>%
  group_by(sen.comlang, sen.menhealth) %>%
  mutate(prop = prop.table(n),
         total = sum(n)) %>%
  filter(anycd_ic == "Disorder present") %>%
  mutate(lower = prop.test(n, total)$conf.int[1],
         upper = prop.test(n, total)$conf.int[2],
         sen.comlang = if_else(sen.comlang == 1, "SEN C&L", "No SEN C&L"),
         sen.menhealth = if_else(sen.menhealth == 1, "SEN B&E", "No SEN B&E")) %>%
  dplyr::select(sen.comlang, sen.menhealth, lower, prop, upper) %>%
  ggplot(aes(x = sen.comlang, y = prop, colour = sen.comlang)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, NA),
                     name = "Proportion") +
  labs(title = "Rates of behavioural disorder", colour = NULL, x = NULL) +
  facet_wrap(~sen.menhealth, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside")
