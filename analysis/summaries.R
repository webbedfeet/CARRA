#' ---
#' title: CARRA Analysis
#' author: Abhijit Dasgupta, PhD
#' date: "`r format(Sys.time(), '%B %d, %Y %I:%m %p')`"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     theme: cerulean
#'     highlight: espresso
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
all_subjects <- vroom(here('data/raw/all_rows_data_2020-01-31_1545.csv'),
                      col_select = c(subjectId, visit = folderName, eventIndex)) %>%
  distinct() %>%
  mutate(folderName = fct_relevel(visit, 'Baseline','6 month','12 month','18 month','24 month')) %>%
  select(-folderName) %>%
  clean_names()
saveRDS(all_subjects, here('data/rda/all_subjects.rds'), compress=T)
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


write.table(data_dict, sep = '\t', file = here('background/f-6-337-13136911_ZH2vHUc6_111319_CARRA_Registry_11.0_DataDictionary_DCRI_SxxX.tsv'))


# Principle 1 -------------------------------------------------------------
#' ## Principle 1 : 20% to 75% of children with SLE will develop nephritis
#' From the data dictionary, this question is answered in the variable SLICC00
#'
#' I have verified that this definition is compatible with the raw data when
#' we use both WHO and ISNRPS criteria.
#'
#' Note: Subject has lupus nephritis if any of WHO 2-6 or ISNRPS 2-6 are positive
#'
slicc_info <- vroom(here('data/raw/slicc_data_2020-01-31_1545.csv'),
                    col_select = c(subjectId, visit = folderName,
                                   SLICC00)) %>%
  clean_names()
raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds')) # see biopsy_classes.R
total_ln <- raw_biopsy %>%
  group_by(subject_id) %>%
  summarize(LN = ifelse(any(LN==1, na.rm=T), 1, 0)) %>%
  ungroup()

total_ln %>%
  mutate(LN = ifelse(LN==1, 'Positive','Negative')) %>%
  tabyl(LN) %>%
  adorn_pct_formatting() %>%
  set_names(c('Lupus nephritis','N','Percent')) %>%
  kable(caption = 'Frequency of lupus nephritis') %>%
  kable_styling(full_width = F)


#' ## Principle 2: 82% of LN in cSLE develops within the first year of diagnosis and 92% within 2 years
#'
#+ echo = T
# Principle 2 -------------------------------------------------------------
# raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds'))
# baseline_LN <- all_subjects %>% left_join(raw_biopsy) %>%
#   filter(visit == 'Baseline', !is.na(LN)) %>%
#   summarize(LN=sum(LN))
#
# firstyr_LN <- all_subjects %>% left_join(raw_biopsy) %>%
#   filter(visit %in% c('Baseline','3 month','6 month', '9 monht',
#                       '12 month'), !is.na(LN)) %>%
#   group_by(subject_id) %>%
#   summarize(LN = ifelse(any(LN==1, na.rm=T), 1, 0)) %>%
#   ungroup() %>%
#   summarize(LN=sum(LN))
#
# total_ln <- raw_biopsy %>%
#   group_by(subject_id) %>%
#   summarize(LN = ifelse(any(LN==1, na.rm=T), 1, 0)) %>%
#   ungroup()
# tribble(~Time, ~N,
#                'Baseline', baseline_LN %>% pull(LN),
#                'Within first year', firstyr_LN %>% pull(LN),
#                'Total', total_ln %>% summarize(LN = sum(LN)) %>% pull(LN)
# ) %>%
#   mutate(Percent = 100* N/max(N)) %>%
#   kable(caption = 'Proportion of LN seen within first year', digits=2) %>%
#   kable_styling(full_width = F)
#'
#' We are going to look at actual date of SLE diagnosis and the
#' date of biopsy. We'll restrict to individuals with LN. We'll also assume that
#' biopsy dates prior to date of lupus diagnosis are aberrant and should be considered
#' the same time as the diagnosis date.
#'
#' A limitation of this analysis is that we only have year of diagnosis and year of
#' biopsy, not the actual dates for identifiability reasons. So the differences in calendar years
#' may represent periods longer than a year, depending on when the actual dates of the
#' diagnosis and biopsy were.
#'
pdis <- vroom(here('data/raw/pdisease_data_2020-01-31_1545.csv')) %>%
  clean_names() %>%
  select(subject_id, dxdt_yyyy) %>%
  group_by(subject_id) %>%
  summarize(dxdt = min(dxdt_yyyy, na.rm=T)) %>%
  ungroup()
raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds')) %>%
  select(subject_id, visit, event_index, biopdtc_yyyy, biopsdtc_yyyy, LN) %>%
  filter(LN==1) %>%
  group_by(subject_id) %>%
  summarize(biopdtc = min(biopdtc_yyyy, na.rm=T), biopsdtc = min(biopsdtc_yyyy, na.rm=T),
            LN = ifelse(any(LN==1, na.rm=T), 1,0)) %>%
  ungroup()
biopsy <- all_subjects %>% select(subject_id) %>% distinct() %>% left_join(raw_biopsy) %>% left_join(pdis) %>%
  mutate(time_to_pos_biopsy = biopsdtc - dxdt) %>%
  filter(LN==1)

biopsy %>% count(time_to_pos_biopsy) %>% mutate(prob = 100*cumsum(n)/sum(n)) %>%
  filter(time_to_pos_biopsy <=2) %>%
  kable(col.names = c('Years', 'N', 'Cumulative probability'),
        caption = 'Time from diagnosis to positive biopsy',
        digits=2) %>%
  kable_styling(full_width = F)

# Principle 3 -------------------------------------------------------------
#' ## Principle 3: Membranous (class V) LN more often presents with nephrotic syndrome than proliferative LN (class III or IV)
#'
#' Definition of LN classes:
#'
#' 1. Class V = WHO-5 or ISNRPS-5
#' 1. Class III = WHO-3 or ISNRPS-3
#' 1. Class IV = WHO-4 or ISNRPS-4
#'
#' The definition of nephrotic syndrome is as follows:
#'
#' 1. The presence of nephrotic range proteinuria, which is a urine protein:creatinine ratio > 1mg/mg or if there is a 24 hour urine instead of a urine protein:creatinine ratio (different docs check it differently), it would be a 24 hour protein excretion greater than 3.5 g/24 hours.
#' 2. Hypoalbuminemia (an albumin less than 3 g/dL)
#' 3. On examination, documentation of edema
#'
#' ### Poor availability of data
#' To evaluate this principle we need information on protein:creatinine ratios in urine, albumin
#' levels and documentation of edema. First of all, it does not appear that **albumin was collected**,
#' at least it is not available in the CARRA database.
#'
#' Second, there is a serious lack of urine protein:creatinine ratios. See the
#' separate report, which highlights the fact that only around 25-30% of subjects had
#' urine protein:creatinine ratios available at any visit.
#'
#' These two issues preclude us from looking at nephrotic syndrome as an outcome for any
#' analysis.

#' ## Principle 4: Short term renal outcomes are worse in blacks
# Principle 4 -------------------------------------------------------------
make_visit_index <- function(d){
  d %>% filter(!(visit %in% c('Common Forms','Medications', 'Event'))) %>%
    mutate(visit=fct_relevel(visit, 'Baseline','6 month')) %>%
    mutate(visit_index = event_index) %>%
    mutate(visit_index = ifelse(visit == 'Unsch' & !is.na(event_index),
                                event_index - 0.5, event_index)) %>%
    mutate(visit_index = ifelse(!is.na(visit) & is.na(visit_index),
                                as.numeric(visit)-0.5, visit_index))
}

all_subjects <- readRDS(here('data/rda/all_subjects.rds'))
demographic <- readRDS(here('data/rda/demographic.rds')) # computed below
short_outcomes <- readRDS(here('data/rda/short_outcomes.rds'))
raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds')) %>%
  select(subject_id:event_index, starts_with("LN"))
prin4 <- all_subjects %>%
  left_join(demographic %>%
              select(white:othrace, subject_id)) %>%
  left_join(short_outcomes) %>%
  left_join(raw_biopsy) %>%
  distinct() %>%
  make_visit_index()


#' Based on conversations, we will consider only LN patients and take their first
#' visit with confirmed LN as the baseline time for the GFR change analysis. We will look at
#' race, baseline GFR level, as well as LN classes, as stratifying variables
#'
prin4$event_index[prin4$subject_id==139 & prin4$visit=='6 month'] <- 2.5
baseline_time <- prin4 %>%
  filter(LN == 1, !is.na(LN)) %>%
  group_by(subject_id) %>%
  filter(event_index == min(event_index)) %>%
  ungroup() %>%
  select(subject_id, visit, baseline_time = event_index) %>%
  distinct()
#' There are `r nrow(baseline_time)` individuals with
#' confirmed LN.
#'
prin4 <- prin4 %>% left_join(baseline_time) %>%
  filter(!is.na(baseline_time), !is.na(LN)) %>%
  group_by(subject_id) %>%
  filter(event_index >= baseline_time) %>%
  ungroup() %>%
  distinct()

## GFR

d <- prin4 %>%
  filter(!is.na(gfr_class)) %>%
  group_by(subject_id) %>%
  filter(event_index == min(event_index) | event_index==max(event_index)) %>%
  ungroup() %>%
  select(subject_id, event_index, gfr_class) %>%
  distinct() %>%
  mutate(event_index = ifelse(event_index == 1, 'first','last'))
d %>% count(subject_id) %>% filter(n > 1) %>%
  left_join(d) %>% distinct() %>%
  spread(event_index, gfr_class) %>%
  filter(!is.na(last)) %>%
  tabyl(first, last) %>%
  adorn_percentages('all') %>%
  adorn_pct_formatting() %>%
  adorn_title('combined') %>%
  kable(caption = 'Percentage by GFR stage transition') %>%
  kable_styling()

d %>% count(subject_id) %>% filter(n > 1) %>% left_join(d) %>% distinct() %>%
  spread(event_index, gfr_class) %>%
  filter(!is.na(last)) %>%
  tabyl(first, last) %>%
  adorn_percentages('row') %>%
  adorn_pct_formatting() %>% adorn_ns() %>%
  adorn_title('combined') %>%
  kable(caption = 'Percentage by GFR stage transition') %>%
  kable_styling()

## Remission

prin4 %>% filter(visit == 'Baseline') %>%
  tabyl(remission) %>%
  adorn_pct_formatting() %>%
  kable(caption = '"Normal" status at baseline') %>%
  kable_styling()
## Omit subjects who are normal at baseline
prin4 %>%
  filter(!is.na(remission)) %>%
  filter(subject_id %in% (prin4 %>% filter(visit == 'Baseline', remission=='No') %>%
                            pull(subject_id))) %>%
  tabyl(remission, black, visit) %>% adorn_percentages('col') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  bind_rows(.id = 'visit') %>%
  mutate(visit = fct_relevel(visit, 'Baseline','6 month','12 month','18 month')) %>%
  arrange(visit) %>%
  filter(visit != 'Baseline') %>%
  rename(`Non-black` = `0`, Black = `1`) %>%
  kable(caption = 'Proportion getting to remission by time and race') %>%
  kable_styling()


#' ## Principle 5: Short term renal outcomes are worse in patients who present with GFR < 60mL/min/1.73 m2 and/or nephrotic-range proteinuria (> 1 protein/creatinine ratio)
# Principle 5 -------------------------------------------------------------

#' ## Principle 6: Rituximab has been used as a steroid-sparing agent for induction in proliferative LN (LN vs no-LN, 3-4 vs 5)
#'
#' Rituximab use: IMMMED = 30
#' MEDCATON = 30
# Principle 6 -------------------------------------------------------------

raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds'))
all_rows <- vroom(here('data/raw/all_rows_data_2020-01-31_1545.csv'))
ritux <- all_rows %>% filter(conceptValue=='Rituximab (Rituxan)') %>%
  clean_names() %>%
  select(subject_id, ritux = concept_value) %>%
  distinct()
tab_rtx_ln <- all_subjects %>% left_join(raw_biopsy) %>% left_join(ritux) %>%
  filter(!is.na(LN)) %>%
  mutate(Rituximab = ifelse(is.na(ritux), 'No','Yes'),
         LN = ifelse(LN==1, 'Pos','Neg')) %>%
  tabyl(Rituximab, LN)


tab_rtx_ln %>%
  adorn_percentages('col') %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns() %>%
  adorn_title('combined') %>%
  knitr::kable(caption = 'Rituximab use between LN and non-LN subjects') %>%
  kable_styling()
#'
#' This is statistically significant, with the $\chi^2$ test p-value being
#' `r pval_scientific(chisq.test(tab_rtx_ln)$p.value)`.
#'
tbl_rit_class <- all_subjects %>%
  left_join(raw_biopsy) %>%
  filter(!is.na(LN)) %>%
  group_by(subject_id) %>%
  summarize_at(vars(starts_with("LN")), ~ifelse(any(.==1,na.rm=T),1, 0)) %>%
  left_join(ritux) %>%
  mutate(Rituximab = ifelse(is.na(ritux), 'No', 'Yes')) %>%
  mutate(LN34 = ifelse(LN3==1|LN4==1, 1, 0)) %>%
  select(subject_id, LN34, LN5, Rituximab) %>%
  gather(Class, value, LN34, LN5) %>%
  filter(value==1) %>%
  tabyl(Rituximab, Class)

tbl_rit_class %>%
  adorn_percentages('col') %>%
  adorn_pct_formatting() %>%
  adorn_title('combined') %>%
  rename(`LN3/4` = LN34) %>%
  kable(caption = 'Rituximab use by LN class') %>%
  kable_styling()

#' This is not statistically significant (p-value = `r chisq.test(tbl_rit_class)$p.value`)


#'
#' ### Is there differences in age/gender for people getting Rituximab
demographic <- vroom(here('data/raw/dem_data_2020-01-31_1545.csv')) %>%
  clean_names() %>%
  select(subject_id, rheumage, sex, dxdage,
         white, black, asian, amerind, hispanic,
         mideast, noanswer, nathwn, othrace, residenc)
saveRDS(demographic, here('data/rda/demographic.rds'), compress=T)
bl <- demographic %>% left_join(slicc_info) %>%
  rename(ritux = slicc00) %>%
  mutate(ritux = ifelse(ritux==1, 'Yes','No'))

bl %>% group_by(ritux) %>%
  summarize(`Median age` = median(rheumage),
            IQR = IQR(rheumage)) %>%
  kable(caption = 'Rituximab use by age') %>%
  kable_styling()

#' This is not significant
#' (Wilcoxon test p-value = `r format(wilcox.test(rheumage~ritux, data=bl)$p.value, digits=2)`)

tab_rit_gender <- bl %>%
  rename(Sex=sex, Rituximab = ritux) %>%
  tabyl(Sex, Rituximab)
tab_rit_gender %>%
  adorn_percentages('col') %>%
  adorn_pct_formatting() %>%
  adorn_title('combined') %>%
  kable(caption = 'Rituximab use by gender') %>%
  kable_styling()

#' This is not statistically significant
#' ($\chi^2$ test p-value = `r format(chisq.test(tab_rit_gender)$p.value, digits=2)`)

#' # Session information
#'
#' This analysis was done using `r R.version$version.string` and the following packages
#+ packages, results = 'asis', echo = FALSE
pkgs <- p_loaded() %>% sort()
d <- tibble(Package = pkgs) %>%
  mutate(Version = map(Package, p_ver) %>% map_chr(as.character))
bl <- glue::glue_data(d, '{Package} ({Version})') %>% paste(collapse = '; ')
cat(bl)

