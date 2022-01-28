library(tidyverse)
library(survey)
library(foreign)

main <- read.spss("\\\\thf-rds-fsvm01\\ode_data\\Analytics\\Tom\\MHCYP 2004\\UKDA-5269-spss\\spss\\spss12\\cpm9904.sav",
                  to.data.frame = TRUE)

# weights based on age, sex & region
prop.table(table(main$anycd_ic[main$sampyear == 2004]))

main %>%
  filter(sampyear == 2004) %>%
  group_by(anycd_ic) %>%
  summarise(n = sum(weightgr)) %>%
  mutate(prop = prop.table(n))

# what's the distribution of conduct disorders?
main %>%
  filter(sampyear == 2004) %>%
  count(gormain, anycd_ic) %>%
  group_by(gormain) %>%
  mutate(prop = prop.table(n)) %>%
  filter(anycd_ic == "Disorder present") %>%
  ggplot(aes(x = gormain, y = prop)) +
  geom_col()

# significant differences across regions?
main %>%
  filter(sampyear == 2004) %>%
  count(gormain, anycd_ic) %>%
  group_by(gormain) %>%
  mutate(prop = prop.table(n),
         total = sum(n)) %>%
  filter(anycd_ic == "Disorder present") %>%
  mutate(lower = prop.test(n, total)$conf.int[1],
         upper = prop.test(n, total)$conf.int[2]) %>%
  select(gormain, lower, prop, upper) %>%
  ggplot(aes(x = gormain, y = prop)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, NA),
                     name = "Proportion") +
  labs(title = "Rates of behavioural disorder")

plot_cd_rates <- function(myData, myVar) {
  myData %>%
    count({{myVar}}, anycd_ic) %>%
    group_by({{myVar}}) %>%
    mutate(prop = prop.table(n),
           total = sum(n)) %>%
    filter(anycd_ic == "Disorder present") %>%
    mutate(lower = prop.test(n, total)$conf.int[1],
           upper = prop.test(n, total)$conf.int[2]) %>%
    select({{myVar}}, lower, prop, upper) %>%
    ggplot(aes(x = {{myVar}}, y = prop)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, NA),
                       name = "Proportion") +
    labs(title = "Rates of behavioural disorder")
}

main %>%
  filter(sampyear == 2004) %>%
  plot_cd_rates(gormain)

# using adjustment factor
yr2004 %>%
  filter(gormain == "London") %>%
  count(gormain, anycd_ic, wt = myweight) %>%
  group_by(gormain) %>%
  mutate(prop = prop.table(n))

yr2004 %>%
  #filter(gormain == "London") %>%
  count(gorgrp, anycd_ic, wt = myweight) %>%
  group_by(gorgrp) %>%
  mutate(prop = prop.table(n))


# just London and UK-wide
cd_uk_lon <- main %>%
  filter(sampyear == 2004) %>%
  count(anycd_ic) %>%
  mutate(myRegion = "UK") %>%
  bind_rows(main %>%
              filter(gormain == "London") %>%
              count(anycd_ic) %>%
              mutate(myRegion = "London")) %>%
  select(myRegion, anycd_ic, n) %>%
  group_by(myRegion) %>%
  mutate(prop = prop.table(n))

ggplot(cd_uk_lon %>%
         filter(anycd_ic == "Disorder present"), aes(x = myRegion, y = prop)) +
  geom_col()

# sex & ethnicity
main %>%
  filter(sampyear == 2004) %>%
  plot_cd_rates(chldsex)

plot_cd_rates(main, ethchld)

main %>%
  filter(sampyear == 2004) %>%
  mutate(ethgpc1 = as.character(ethgpc1),
         simple_eth = case_when(
    str_detect(ethgpc1, "Black") ~ "Black",
    ethgpc1 %in% c("Pakistani", "Bangladeshi", " Indian") ~ "Asian",
    TRUE ~ ethgpc1
  )) %>%
  plot_cd_rates(simple_eth)

main %>%
  filter(sampyear == 2004) %>%
  mutate(ethgpc1 = as.character(ethgpc1),
         simple_eth = case_when(
           str_detect(ethgpc1, "Black") ~ "Black",
           ethgpc1 %in% c("Pakistani", "Bangladeshi", " Indian") ~ "Asian",
           TRUE ~ ethgpc1),
         bame = if_else(simple_eth == "White", 0, 1)) %>%
  plot_cd_rates(bame)

# age
table(main$chldage[main$sampyear == 2004])

main %>%
  filter(sampyear == 2004) %>%
  plot_cd_rates(chldage)

main %>%
  filter(sampyear == 2004) %>%
  plot_cd_rates(chld4grp)

main %>%
  filter(sampyear == 2004) %>%
  plot_cd_rates(chld2grp)

age.model <- glm(anycd_ic ~ chldage,
                 family = "binomial",
                 data = main,
                 subset = sampyear == 2004)
summary(age.model)
plot(age.model)

# household income, but no IMD - there is ACORN instead
table(main$hhinc2[main$sampyear == 2004])

main %>%
  filter(sampyear == 2004) %>%
  plot_cd_rates(hhinc2)

# can I interpolate these values? fill the mid-point or a random value?

# gross income - just the responding parent
table(main$grossinc[main$sampyear == 2004])

main %>%
  filter(sampyear == 2004) %>%
  plot_cd_rates(grossinc)

main %>%
  filter(sampyear == 2004) %>%
  mutate(completed_hhinc = if_else(is.na(hhinc2), "Refused income", as.character(hhinc2))) %>%
  plot_cd_rates(completed_hhinc)

main %>%
  filter(sampyear == 2004) %>%
  mutate(completed_hhinc = if_else(is.na(hhinc2), "Refused income", as.character(hhinc2)),
         hh_inc_over_400pw = if_else(hhinc2 %in% c("Over 770", "600.00-770.00", "500.00-599.00", "400.00-499.00"), 1, 0)) %>%
  plot_cd_rates(hh_inc_over_400pw)

# run the first block of naive_model to get yr2004
yr2004 %>%
  mutate(anycd_ic = if_else(anycd_ic == "Disorder present", 1, 0)) %>%
  group_by(bame, hh_inc_over_400pw) %>%
  summarise(cd_rate = sum(anycd_ic) / n()) # high for poor, white children, low for the rest

# headline rates of CD, unweighted rates
main %>%
  filter(sampyear == 2004) %>%
  filter(chldage > 10) %>%
  filter(gormain == "London") %>%
  filter(gorgrp == "London inner") %>%
  mutate(cd = as.integer(anycd_ic) - 1) %>%
  summarise(cd_rate = mean(cd))

main %>%
  filter(sampyear == 2004) %>%
  mutate(cd = as.integer(anycd_ic) - 1) %>%
  summarise(cd_rate = mean(cd))