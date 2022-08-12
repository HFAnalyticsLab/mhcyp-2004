# this script is to look at healthcare use by those with behavioural disorders

library(tidyverse)
library(survey)

# contact with primary care
table(yr2004$any_ic, yr2004$contact_with_primary_care) %>% prop.table(margin = 1)
table(yr2004$anycd_ic, yr2004$contact_with_primary_care) %>% prop.table(margin = 1)

# so what % have a BD *and* see the GP about it?
table(yr2004$anycd_ic, yr2004$contact_with_primary_care) %>% prop.table()

# by age
table(yr2004$chldage, yr2004$anycd_ic, yr2004$contact_with_primary_care) %>% prop.table()

yr2004 %>%
  count(chldage, anycd_ic, contact_with_primary_care) %>%
  group_by(chldage) %>%
  mutate(gp_bd_rate = n / sum(n),
         subsample = sum(n)) %>%
  filter(anycd_ic == "Disorder present") %>%
  filter(contact_with_primary_care == 1) %>%
  select(-anycd_ic, -contact_with_primary_care) %>%
  mutate(lower = prop.test(n, subsample)$conf.int[1],
         upper = prop.test(n, subsample)$conf.int[2])
  

# BD vs. ADHD contact rates with primary care
yr2004 %>%
  filter(anycd_ic == "Disorder present" | hyper_ic == "Disorder present") %>%
  count(anycd_ic, hyper_ic, contact_with_primary_care) %>%
  group_by(anycd_ic, hyper_ic) %>%
  mutate(gp_rate = n / sum(n)) %>%
  filter(contact_with_primary_care == 1)

# contact with a specialist
table(yr2004$anycd_ic, yr2004$contact_with_specialist) %>% prop.table(margin = 1)

# contact with MHS
table(yr2004$anycd_ic, yr2004$contact_with_mhs) %>% prop.table(margin = 1)

# contact with GP & MHS
table(yr2004$anycd_ic, yr2004$contact_with_primary_care & yr2004$contact_with_mhs) %>% prop.table(margin = 1)

# so what % have a BD *and* see both the GP & MHS about it?
table(yr2004$anycd_ic, yr2004$contact_with_primary_care & yr2004$contact_with_mhs) %>% prop.table()
