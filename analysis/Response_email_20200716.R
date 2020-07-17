#' ---
#' title: Response to 2020-07-16 e-mail
#' author: Abhijit Dasgupta, PhD
#' date: "`r format(Sys.time(), '%B %d, %Y %I:%m %p')`"
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: true
#'       smooth_scroll: false
#'     theme: sandstone
#'     highlight: zenburn
#'     code_folding: hide
#' ---
#'
#+ preamble, echo=T, results='hide', message=F, warning=F
# Preamble ----------------------------------------------------------------
library(pacman)
p_load(char = c('readxl','tidyverse','data.table','glue','fs','here',
                'knitr','kableExtra', 'vroom', 'janitor'))
source(here('lib/R/pval_scientific.R'))


knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE,
                      cache = F)
# all_subjects <- vroom(here('data/raw/all_rows_data_2020-01-31_1545.csv'),
#                       col_select = c(subjectId, visit = folderName, eventIndex)) %>%
#   distinct() %>%
#   mutate(folderName = fct_relevel(visit, 'Baseline','6 month','12 month','18 month','24 month')) %>%
#   select(-folderName) %>%
#   clean_names()
# saveRDS(all_subjects, here('data/rda/all_subjects.rds'), compress=T)
all_subjects <- readRDS(here('data/rda/all_subjects.rds'))
#'
#+ data_date_version, echo=F
# Documenting version of data we're using ---------------------------------
data_date <- tibble(fname = dir_ls(here('data/raw'), glob = '*.zip')) %>%
  separate(fname, c('p1','p2','dt','tm'), sep = '_', remove = T) %>% # Grabbing metadata from file names
  select(-p1, -p2) %>%
  mutate(tm = str_remove(tm, '.zip'),
         tm = as.numeric(tm)) %>% #,
         # tm = substr(as.POSIXct(sprintf('%4.0f', tm), format = "%H%M"), 12,16)) %>%
  arrange(desc(dt), desc(tm)) %>% # arrange in descending order
  slice(1) # Take the first row, i.e. the latest date

#' **Data version:** The version of data we're using is from `r glue_data(data_date, '{dt} {substr(as.POSIXct(as.character(tm), format="%H%M"),12,16)}')`.
#+ data_dict, echo = F
# reading data dictionary -------------------------------------------------

data_dict <- read_excel(here('background/f-6-337-13136911_ZH2vHUc6_111319_CARRA_Registry_11.0_DataDictionary_DCRI_SxxX.xlsx')) %>%
  clean_names()


# Table one ---------------------------------------------------------------

## Could you provide us with a demographics breakdown so we can do a table 1 with
# Age at enrollment
# Gender
# Ethnicity
# Time since diagnosis

age <- vroom(here('data/raw/visit.list_data_2020-01-31_1545.csv')) %>%
  clean_names() %>%
  select(subject_id, event_type, visage) %>%
  filter(event_type=='Baseline') %>%
  distinct()

demographics <- readRDS(here('data/rda/demographic.rds'))

dat <- age %>% left_join(demographics, by='subject_id') %>%
  mutate(time_since_dx = visage - dxdage)

races <- dat %>%
  select(white, black, asian, amerind, mideast, nathwn, othrace) %>%
  rowwise() %>%
  mutate(present = sum(c_across(everything()))) %>%
  mutate(missing = ifelse(present==0, 1,0)) %>%
  mutate(mixed = ifelse(present >1, 1, 0)) %>%
  select(-present) %>% ungroup() %>%
  bind_cols(subject_id=dat$subject_id)

races[races$mixed==1, select(races, white:othrace) %>% names()] = 0

races %>%
  pivot_longer(cols = -subject_id,
               names_to = 'race', values_to = 'indic') %>%
  filter(indic != 0) %>%
  mutate(race = str_to_title(race)) %>%
  mutate(race = ifelse(race=='Missing', NA, race)) %>%
  mutate(race = fct_infreq(race)) %>%
  mutate(race = fct_lump_min(race, min=15, other_level = 'Other race')) %>%
  tabyl(race) %>%
  adorn_pct_formatting()

races2 <- races %>%
  pivot_longer(cols = c(-subject_id),
               names_to = 'race', values_to = 'indic') %>%
  filter(indic != 0) %>%
  mutate(race = str_to_title(race),
         race = ifelse(race == 'Missing', NA, race),
         race = fct_infreq(race),
         race = fct_lump_min(race, min=15, other_level = 'Other race'))


tab_race <- races %>%
  pivot_longer(cols = c(-subject_id),
               names_to = 'race', values_to = 'indic') %>%
  filter(indic != 0) %>%
  mutate(race = str_to_title(race),
         race = ifelse(race == 'Missing', NA, race),
         race = fct_infreq(race),
         race = fct_lump_min(race, min=15, other_level = 'Other race')) %>%
  tabyl(race) %>%
  adorn_pct_formatting()

tab_race_eth <- races %>%
  bind_cols(hispanic = dat$hispanic) %>%
  pivot_longer(cols = c(-subject_id, -hispanic),
               names_to = 'race', values_to = 'indic') %>%
  filter(indic != 0) %>%
  mutate(hispanic = ifelse(hispanic==1, 'Hispanic','Non-hispanic')) %>%
  mutate(race = str_to_title(race),
         race = ifelse(race == 'Missing', NA, race),
         race = fct_infreq(race),
         race = fct_lump_min(race, min=15, other_level = 'Other race')) %>%
  tabyl(race, hispanic) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns()

dat1 <- dat %>% left_join(races2 %>% select(-indic)) %>%
  mutate(hispanic = factor(ifelse(hispanic==1, 'Hispanic', 'Non-hispanic')))
label(dat1$sex) = 'Sex'
label(dat1$visage) = "Age at enrollment"
label(dat1$time_since_dx) = "Time since diagnosis"
label(dat1$race) = 'Race'
label(dat1$hispanic) = 'Ethnicity'

units(dat1$time_since_dx) = 'years'
units(dat1$visage) = 'years'

table1::table1(~ visage + sex + race + hispanic + time_since_dx, data=dat1)
