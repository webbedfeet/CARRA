## LN ascertainment
##
## There is a variable, SLICC00, that is putatively the right variable
## We're going to ascertain based on WHO and ISNRPS values
##
pacman::p_load(char=c('tidyverse','janitor','broom'))

all_subjects <- vroom(here('data/raw/all_rows_data_2020-01-31_1545.csv'),
                      col_select = c(subjectId, visit = folderName)) %>%
  distinct() %>%
  mutate(folderName = fct_relevel(visit, 'Baseline','6 month','12 month','18 month','24 month')) %>%
  select(-folderName) %>%
  clean_names()

slicc_info <- vroom(here('data/raw/slicc_data_2020-01-31_1545.csv'),
                    col_select = c(subjectId, visit = folderName,
                                   SLICC00)) %>%
  clean_names()

ln_visit <- vroom(here('data/raw/vis_data_2020-01-31_1545.csv'),
                  col_select = c(subjectId, folderName, LUPUSNEP)) %>%
  clean_names() %>%
  rename(visit = folder_name)

raw_biopsy <- vroom('data/raw/biopsy_data_2020-01-31_1545.csv') %>%
  clean_names() %>%
  select(subject_id, visit = folder_name, event_index,
         matches('[who|isnrps][2-6]$')) %>%
  mutate(LN = rowSums(.[,-(1:3)])) %>%
  mutate(LN3 = rowSums(.[,c('isnrps3','who3')]),
         LN4 = rowSums(.[,c('isnrps4','who4')]),
         LN5 = rowSums(.[,c('isnrps5','who5')])) %>%
  mutate_at(vars(starts_with('LN')), ~ifelse(. > 0, 1, 0))

saveRDS(raw_biopsy, file = here('data/rda/biopsy_classes.rds'), compress=T)
