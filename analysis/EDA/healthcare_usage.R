# this script is to look at healthcare use by those with behavioural disorders

library(tidyverse)
library(survey)

# contact with primary care
table(yr2004$any_ic, yr2004$contact_with_primary_care) %>% prop.table(margin = 1)
table(yr2004$anycd_ic, yr2004$contact_with_primary_care) %>% prop.table(margin = 1)

# so what % have a BD *and* see the GP about it?
table(yr2004$anycd_ic, yr2004$contact_with_primary_care) %>% prop.table()

# contact with a specialist
table(yr2004$anycd_ic, yr2004$contact_with_specialist) %>% prop.table(margin = 1)

# contact with MHS
table(yr2004$anycd_ic, yr2004$contact_with_mhs) %>% prop.table(margin = 1)

# contact with GP & MHS
table(yr2004$anycd_ic, yr2004$contact_with_primary_care & yr2004$contact_with_mhs) %>% prop.table(margin = 1)

# so what % have a BD *and* see both the GP & MHS about it?
table(yr2004$anycd_ic, yr2004$contact_with_primary_care & yr2004$contact_with_mhs) %>% prop.table()
