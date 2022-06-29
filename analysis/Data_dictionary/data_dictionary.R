library(tidyverse)
library(survey)
library(foreign)

main <- read.spss("\\\\thf-rds-fsvm01\\ode_data\\Analytics\\Tom\\MHCYP 2004\\UKDA-5269-spss\\spss\\spss12\\cpm9904.sav",
                  to.data.frame = TRUE)

# 2 cross sections, no longitudinal component
main %>%
  group_by(sampyear) %>%
  summarise(nrows = n(),
            ncases = n_distinct(studyno))

# extract the field descriptions
defs <- data.frame(
  name = attributes(main)$name,
  description = attributes(main)$variable.labels
) %>%
  remove_rownames()

# get the possible values
colnames(main)

get_unique_values <- function(x) {
  if (x %in% c("studyno", "wt9904sc", "weightgr", "weightsc")) {
    # exclude these because basically unique to each row
    NA
  } else {
    main[[x]] %>%
      unique() %>%
      sort() %>%
      as.character() %>%
      .[1:50] # cap number of values at 50
  }
}

unique_values <- map(colnames(main), get_unique_values) %>%
  map_dfr(~as_data_frame(t(.)))

bind_cols(defs, unique_values) %>%
  write_csv("./analysis/data_dictionary.csv")
