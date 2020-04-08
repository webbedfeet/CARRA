#' ---
#' title: CARRA Analysis
#' author: Abhijit Dasgupta, PhD
#' date: "`r format(Sys.time(), '%B %d, %Y %I:%m %p')`"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     theme: journal
#'     code_folding: hide
#' ---
#'
#+ preamble, include = FALSE
# Preamble ----------------------------------------------------------------
library(pacman)
p_load(char = c('readxl','tidyverse','data.table','glue','fs','here',
                'knitr','kableExtra', 'vroom', 'janitor'))
source(here('lib/pval_scientific.R'))
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      cache = F)
all_subjects <- vroom(here('data/raw/all_rows_data_2020-01-31_1545.csv'),
                      col_select = c(subjectId, visit = folderName)) %>%
  distinct() %>%
  mutate(folderName = fct_relevel(visit, 'Baseline','6 month','12 month','18 month','24 month')) %>%
  select(-folderName) %>%
  clean_names()
#'
#+ data_date_version
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

# reading data dictionary -------------------------------------------------

data_dict <- read_excel(here('background/f-6-337-13136911_ZH2vHUc6_111319_CARRA_Registry_11.0_DataDictionary_DCRI_SxxX.xlsx'))
names(data_dict) = make.names(names(data_dict))

write.table(data_dict, sep = '\t', file = here('background/f-6-337-13136911_ZH2vHUc6_111319_CARRA_Registry_11.0_DataDictionary_DCRI_SxxX.tsv'))

#' ## Principle 1 : 20% to 75% of children with SLE will develop nephritis
#+ results='hide', echo = F
slicc_info <- vroom(here('data/raw/slicc_data_2020-01-31_1545.csv'),
                    col_select = c(subjectId, visit = folderName,
                                   SLICC00)) %>%
  clean_names()
raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds')) # see biopsy_classes.R
total_ln <- raw_biopsy %>%
  group_by(subject_id) %>%
  summarize(LN = ifelse(any(LN==1, na.rm=T), 1, 0)) %>%
  ungroup()
#' From the data dictionary, this question is answered in the variable SLICC00
#'
#' I have verified that this definition is compatible with the raw data when
#' we use both WHO and ISNRPS criteria.
#'
#' Note: Subject has lupus nephritis if any of WHO 2-6 or ISNRPS 2-6 are positive
#'
total_ln %>%
  mutate(LN = ifelse(LN==1, 'Positive','Negative')) %>%
  tabyl(LN) %>%
  adorn_pct_formatting() %>%
  set_names(c('Lupus nephritis','N','Percent')) %>%
  kable(caption = 'Frequency of lupus nephritis') %>%
  kable_styling(full_width = F)

#' ## Principle 2: 82% of LN in cSLE develops within the first year of diagnosis and 92% within 2 years
#'
#+ echo = F
raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds'))
baseline_LN <- all_subjects %>% left_join(raw_biopsy) %>%
  filter(visit == 'Baseline', !is.na(LN)) %>%
  summarize(LN=sum(LN))

firstyr_LN <- all_subjects %>% left_join(raw_biopsy) %>%
  filter(visit %in% c('Baseline','3 month','6 month', '9 monht',
                      '12 month'), !is.na(LN)) %>%
  group_by(subject_id) %>%
  summarize(LN = ifelse(any(LN==1, na.rm=T), 1, 0)) %>%
  ungroup() %>%
  summarize(LN=sum(LN))

tribble(~Time, ~N,
               'Baseline', baseline_LN %>% pull(LN),
               'Within first year', firstyr_LN %>% pull(LN),
               'Total', total_ln %>% summarize(LN = sum(LN)) %>% pull(LN)
) %>%
  mutate(Percent = 100* N/max(N)) %>%
  kable(caption = 'Proportion of LN seen within first year', digits=2) %>%
  kable_styling(full_width = F)
#'
#'


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



#' ## Principle 4: Short term renal outcomes are worse in blacks of African American heritage

#' ## Principle 5: Short term renal outcomes are worse in patients who present with GFR < 60mL/min/1.73 m2 and/or nephrotic-range proteinuria (> 1 protein/creatinine ratio)

#' ## Principle 6: Rituximab has been used as a steroid-sparing agent for induction in proliferative LN (LN vs no-LN, 3-4 vs 5)
#'
#' Rituximab use: IMMMED = 30
#' MEDCATON = 30

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


