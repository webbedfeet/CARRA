#+ setup
pacman::p_load(char = c('tidyverse','janitor','vroom'))

#+ data_ingestion
all_subjects <- readRDS(here('data/rda/all_subjects.rds')) # Has id, visit, and event index
demographic <- readRDS(here('data/rda/demographic.rds')) # subject id, race, age
short_outcomes <- readRDS(here('data/rda/short_outcomes.rds')) %>%
  mutate(visit = as.character(visit)) # urinanalysis
visits <- vroom(here('data/raw/visit.list_data_2020-01-31_1545.csv')) %>%
  clean_names(case='snake') # visit date and age @ visit
raw_biopsy <- readRDS(here('data/rda/biopsy_classes1.rds')) # visit-based biopsy results
ln_classes <- readRDS(here('data/rda/LN_classes1.rds')) # person-level LN status

#+ munging

## Fix event_index for subjects where the biopsy date doesn't have an event_index value
## raw_biopsy %>% filter(is.na(event_index), !is.na(biopsdtc_yyyy))
all_subjects$event_index[all_subjects$subject_id==139 &
                           all_subjects$visit=='6 month'] <- 2.5
all_subjects$event_index[all_subjects$subject_id==225 &
                           all_subjects$visit=='Unsch'] <- 2
all_subjects$event_index[all_subjects$subject_id==597 &
                           all_subjects$visit=='Unsch'] <- 1.5

## Identify first visit when LN==1
## This comes from raw_biopsy, since that gives us visit-level information

first_ln <- all_subjects %>%
  filter(!is.na(event_index)) %>%
  left_join(
    raw_biopsy %>%
      select(subject_id, visit, LN)
  ) %>%
  filter(LN == 1) %>%
  group_by(subject_id) %>%
  filter(event_index==min(event_index)) %>%
  ungroup() %>%
  distinct() %>%
  select(subject_id, first_index = event_index)

prin4 <- all_subjects %>%
  filter(!is.na(event_index)) %>%
  left_join(
    demographic %>%  # race
      select(subject_id, white:othrace)
  ) %>%
  left_join(
    short_outcomes %>%  # GFR and other measures
      mutate(visit = as.character(visit)) %>%
      select(-event_index)
  ) %>%
  left_join(
    ln_classes %>%  # LN classes (person)
      select(subject_id, LN,LN34:LN50)
  ) %>%
  left_join(
    first_ln   # First visit of LN dx
  ) %>%
  filter(!is.na(first_index)) %>%
  relocate(first_index, .after=event_index) %>%
  group_by(subject_id) %>%
  filter(event_index >= first_index) %>% # keep rows after first_index
  ungroup()

saveRDS(prin4, here('data/rda/prin4.rds'))


