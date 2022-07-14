# trying to understand SDQ scores recorded in the dataset

table(main$d4p, main$sampyear, useNA = "ifany") # D4 considerate feelings

table(main$d32, main$sampyear, useNA = "ifany") # Parent SDQ: OTHSDQ1 D4 Considerate of other people's feelings
table(main$d63, main$sampyear, useNA = "ifany") # Parent SDQ: OTHSDQ2 D4 Considerate of other people's feelings

table(main$cb4, main$sampyear, useNA = "ifany") # YP SDQ i try to be nice to other people, i care about their feelings

table(main$tconsid, main$sampyear, useNA = "ifany") # Considerate of other peoples feelings. < THIS MUST BE THE TEACHER SDQ

# Questions:
#   - Parent SDQ - d4p onwards
#   - Parent SDQ for *other* child - d32 onwards & d63 onwards
#   - Young person SDQ - cb4 onwards
#   - Teacher SDQ - tconsid onwards
#   - What data from parents was used in 1999? Most of ^these fields^ missing
table(main$d15, main$sampyear, useNA = "ifany") # D15 is child fights or bullies in the SDQ


