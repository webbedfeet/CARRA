#' ---
#' title: Response to 2020-07-16 e-mail
#' author: Abhijit Dasgupta, PhD
#' date: "`r format(Sys.time(), '%B %d, %Y %I:%m %p')`"
#' output:
#'   html_document:
#'     toc: false
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: true
#'       smooth_scroll: false
#'     theme: cosmo
#'     highlight: zenburn
#'     css: style.css
#'
#' ---
#'
#+ preamble, include=FALSE
# Preamble ----------------------------------------------------------------
library(pacman)
p_load(char = c('readxl','tidyverse','data.table','glue','fs','here',
                'knitr','kableExtra', 'vroom', 'janitor'))
source(here('lib/R/pval_scientific.R'))


knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
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

#' ### Demographic summary
#'
#' ::: {.query}
#' Could you provide us with a demographics breakdown so we can do a table 1 with
#'
#' + Age at enrollment
#' + Gender
#' + Ethnicity
#' + Time since diagnosis
#'
#' :::
#+
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

races_tab <- races %>%
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

library(table1)
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

#' ### LN cases
#'
#' ::: {.query}
#'  The total number of LN cases were reported as 234 and 235 different places throughout the data. Could you help clarify this?
#' :::
#'
#' From the raw biopsy data we have the following table:
#+
raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds'))
LN_subjects <- raw_biopsy %>%
  group_by(subject_id) %>%
  summarize(LN_ind = any(LN==1, na.rm=T))
tabyl(LN_subjects, LN_ind) %>%
  adorn_pct_formatting() %>%
  adorn_totals() %>%
  rename('LN Status' = LN_ind) %>%
  kable() %>%
  kable_styling(full_width = FALSE)


# Discrepancy in N --------------------------------------------------------
#' In the table for Principle 4 you are seeing 234 patients with LN. This is because
#' for that analysis we were looking at number of visits after LN diagnosis. The
#' information available for subject 597 showed that diagnosis was at an unscheduled
#' visit, and where that visit was temporally between baseline, 6 month and 12 month visit was not available. So this subject was removed from that analysis since the
#' number of visits post diagnosis could not be computed for that subject.
#'
#' This has now been noted in the original report
#'

# Principle 2 table -------------------------------------------------------
#' ###  Principle 2 issue
#'
#' ::: {.query}
#' For principle 2,  the total of LN is 217 instead of 235. I think this wasn’t updated when we added in the rest of the nephritis patients (class I and II) to principle 1
#' :::
#'
#' This wasn't quite the problem. I had truncated the table to 2 or fewer years, and
#' so several individuals were omitted from this table. It has now been fixed in the original report (summaries.html, attached)
#'

# kaplan meier ------------------------------------------------------------


#' ### Kaplan Meier
#'
#' ::: {.query}
#' For principle 2, can we do a kaplan meyer curve for time to nephritis?
#' :::
#'
#' Yes, I can, though it will be a bit choppy due to the fact that we only have
#' years since diagnosis, and not more granular data. Added to summaries.html
#'
