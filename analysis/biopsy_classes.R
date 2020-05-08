## LN ascertainment
##
## There is a variable, SLICC00, that is putatively the right variable
## We're going to ascertain based on WHO and ISNRPS values
##
pacman::p_load(char=c('tidyverse','janitor','broom', 'naniar',
                      'here'))

all_subjects <- vroom(here('data/raw/all_rows_data_2020-01-31_1545.csv'),
                      col_select = c(subjectId, visit = folderName)) %>%
  distinct() %>%
  mutate(folderName = fct_relevel(visit, 'Baseline','6 month','12 month','18 month','24 month')) %>%
  select(-folderName) %>%
  clean_names(case='snake')

slicc_info <- vroom(here('data/raw/slicc_data_2020-01-31_1545.csv'),
                    col_select = c(subjectId, visit = folderName,
                                   SLICC00)) %>%
  clean_names(case='snake')

ln_visit <- vroom(here('data/raw/vis_data_2020-01-31_1545.csv'),
                  col_select = c(subjectId, folderName, LUPUSNEP)) %>%
  clean_names(case = 'snake') %>%
  rename(visit = folder_name)

raw_biopsy <- vroom('data/raw/biopsy_data_2020-01-31_1545.csv') %>%
  clean_names(case='snake') %>%
  select(subject_id, visit = folder_name, event_index, biopdtc_yyyy, biopsdtc_yyyy,
         matches('[who|isnrps][2-6]$'))

### Check that missing data for biopsy is all or nothing
assertthat::are_equal(
  sort(unique(rowSums(is.na(select(raw_biopsy, isnrps2:who6))))),
  c(0,10))

raw_biopsy <- raw_biopsy %>%
  filter_at(vars(isnrps2:who6), complete.cases) %>%
  mutate(LN = rowSums(.[,-(1:3)])) %>%
  mutate(LN3 = rowSums(.[,c('isnrps3','who3')]),
         LN4 = rowSums(.[,c('isnrps4','who4')]),
         LN5 = rowSums(.[,c('isnrps5','who5')])) %>%
  mutate_at(vars(starts_with('LN')), ~ifelse(. > 0, 1, 0)) %>%
  # Create 3 exclusive classes: LN 3/4 only, LN 3/4+5, and LN5 only
  mutate(LN34 = ifelse((LN3==1 | LN4==1) & LN5==0, 1, 0),
         LN345 = ifelse((LN3==1 & LN5==1) | (LN4==1 & LN5==1), 1, 0),
         LN50 = ifelse(LN5==1 & LN3==0 & LN4==0, 1, 0))

saveRDS(raw_biopsy, file = here('data/rda/biopsy_classes.rds'), compress=T)
