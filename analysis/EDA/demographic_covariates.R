# this script is to look at variation in BD rates across basic demographic groups (sex, ethnicity, income)

library(tidyverse)
library(survey)
library(foreign)

# define a function to plot rates & confidence intervals across different groups
plot_cd_rates <- function(myData, myVar, myWeight = NULL) {
  myData %>%
    count({{myVar}}, anycd_ic, wt = {{myWeight}}) %>%
    group_by({{myVar}}) %>%
    mutate(prop = prop.table(n),
           total = sum(n)) %>%
    filter(anycd_ic == "Disorder present") %>%
    mutate(lower = prop.test(n, total)$conf.int[1],
           upper = prop.test(n, total)$conf.int[2]) %>%
    dplyr::select({{myVar}}, lower, prop, upper) %>%
    ggplot(aes(x = {{myVar}}, y = prop)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, NA),
                       name = "Proportion") +
    labs(title = "Rates of behavioural disorder")
}

# sex & ethnicity
yr2004 %>%
  plot_cd_rates(chldsex) +
  labs(x = "Sex of child")

plot_cd_rates(main, ethchld)

yr2004 %>%
  plot_cd_rates(simple_eth) +
  labs(x = "Ethnicity")

yr2004 %>%
  plot_cd_rates(bame) +
  labs(x = "Ethnicity")

# by detailed ethnicity - complicated!
# first get all the combinations of ethnic group and CD status - this is so we can fill zeros for missing groups
inner_join(
  yr2004 %>%
    dplyr::select(ethgpc1) %>%
    distinct(),
  yr2004 %>%
    dplyr::select(anycd_ic) %>%
    distinct(),
  by = character()
) %>%
  left_join(yr2004 %>%
              count(ethgpc1, anycd_ic),
            by = c("ethgpc1", "anycd_ic")) %>%
  mutate(n = coalesce(n, 0)) %>%
  group_by(ethgpc1) %>%
  mutate(prop = prop.table(n),
         total = sum(n)) %>% # runs as far as this!
  filter(anycd_ic == "Disorder present") %>%
  filter(!is.na(ethgpc1)) %>%
  mutate(lower = prop.test(n, total)$conf.int[1],
         upper = prop.test(n, total)$conf.int[2]) %>%
  dplyr::select(ethgpc1, lower, prop, upper) %>%
  ggplot(aes(x = ethgpc1, y = prop)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, NA),
                     name = "Proportion") +
  labs(title = "Rates of behavioural disorder", x = "Ethnicity")

# age
table(yr2004$chldage)

yr2004 %>%
  plot_cd_rates(chldage)

yr2004 %>%
  plot_cd_rates(chld4grp)

yr2004 %>%
  plot_cd_rates(chld2grp) +
  labs(x = "Age group")

age.model <- glm(anycd_ic ~ chldage,
                 family = "binomial",
                 data = main,
                 subset = sampyear == 2004)
summary(age.model)
plot(age.model)

# household income, but no IMD - there is ACORN instead (but not found in the dataset?)
table(yr2004$hhinc2)

yr2004 %>%
  plot_cd_rates(hhinc2)

# can I interpolate these values? fill the mid-point or a random value?

# gross income - just the responding parent
table(yr2004$grossinc)

yr2004 %>%
  plot_cd_rates(grossinc)

yr2004 %>%
  plot_cd_rates(completed_hhinc)

yr2004 %>%
  filter(completed_hhinc != "Refused income") %>%
  mutate(hh_inc_over_400pw = ifelse(hh_inc_over_400pw, "Over £400", "Under £400")) %>%
  plot_cd_rates(hh_inc_over_400pw) +
  labs(x = "Weekly household income") +
  scale_x_discrete(limits = rev)

yr2004 %>%
  mutate(anycd_ic = if_else(anycd_ic == "Disorder present", 1, 0)) %>%
  group_by(bame, hh_inc_over_400pw) %>%
  summarise(cd_rate = sum(anycd_ic) / n()) # high for poor, white children, low for the rest

# headline rates of CD, unweighted rates
yr2004 %>%
  filter(chldage > 10) %>%
  filter(gormain == "London") %>%
  filter(gorgrp == "London inner") %>%
  mutate(cd = as.integer(anycd_ic) - 1) %>%
  summarise(cd_rate = mean(cd))

yr2004 %>%
  mutate(cd = as.integer(anycd_ic) - 1) %>%
  summarise(cd_rate = mean(cd))
