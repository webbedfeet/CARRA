#' ---
#' title: CARRA Analysis
#' author: Abhijit Dasgupta, PhD
#' date: "`r format(Sys.time(), '%B %d, %Y %I:%m %p')`"
#' output_format: html_document
#' ---
#'
#+ preamble, include = FALSE
library(readxl)
library(tidyverse)
library(data.table)
library(glue)
library(fs)
library(here)
library(knitr)
library(kableExtra)

knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      cache = TRUE)
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

#' ## Data exploration and validation
#'
#' The biopsy data is crucial to evaluating lupus nephritis (LN), but apparently
#' has some discrepancies.
#'
#' The data from the biopsy file apparently are from two sources:
#+
biopsy <- data.table::fread(here('data/raw/biopsy_data_2020-01-31_1545.csv'))
kable(biopsy %>% count(formName)) %>%
  kable_styling(full_width = FALSE)

#' <hr/>
#' ## Principle 1 : 20% to 75% of children with SLE will develop nephritis
#'
#' From the data dictionary, this question is answered in SLICC
#' Re-define Yes as
#' Either/or WHO2-6, ISNRPS2-6 = LN


glue_data(data_dict %>% filter(Variable.Name=='SLICC00'),
          'Variable {Variable.Name}: {Variable.Description}')

all_rows <- read_csv(here('data/raw/all_rows_data_2020-01-31_1545.csv'))
all_rows %>% filter(varName=='SLICC00') %>%
  count(conceptValue) %>%
  mutate(perc = n/sum(n)*100)

#' ## Principle 2: 82% of LN in cSLE develops wihin the first year of diagnosis and 92% within 2 years
#'
#' ###  Data validation
#'
#' 1. Subjects 200 and 553 have no biopsy data (as evidenced by the biopsy data file),
#'    but in the all_rows data their SLICC00 = 0, meaning no LN. How is this validated?
#' 1.


# New definition of LN:
LN_ISNRPS <- all_rows %>% filter(str_detect(varName, 'ISNRPS[2-6]$'))
LN_ISNRPS <- LN_ISNRPS %>% group_by(subjectId, eventIndex) %>%
  summarize(ISNRPS_pos = any(conceptValue=='1')) %>%
  right_join(LN_ISNRPS %>% select(subjectId, eventIndex, eventId, folderName, startDate) %>% distinct()) %>%
  ungroup()

LN_WHO = all_rows %>% filter(str_detect(varName, 'WHO[2-6]$'))
LN_WHO <- LN_WHO %>% group_by(subjectId, eventIndex) %>%
  summarize(WHO_pos = any(conceptValue=='1')) %>%
  right_join(LN_WHO %>% select(subjectId, eventIndex, eventId, folderName, startDate) %>% distinct()) %>%
  ungroup()

LN_pos <-  LN_ISNRPS %>% full_join(LN_WHO) %>%
  mutate(LN_pos = ISNRPS_pos | WHO_pos) %>%
  select(-ISNRPS_pos, -WHO_pos) %>%
  mutate(folderName = as.factor(folderName)) %>%
  mutate(folderName = fct_relevel(folderName, 'Baseline','3 month','6 month',
                                  '12 month','18 month','24 month','30 month','36 month'))
LN_pos %>%
  count(folderName, LN_pos) %>%
  spread(LN_pos, n) %>%
  rename('pos' = `TRUE`, 'neg' = `FALSE`) %>%
  mutate(pos_perc = pos / (pos + neg)*100) %>%
  mutate(cum_pos = cumsum(ifelse(is.na(pos), 0, pos)),
         cum_neg = cumsum(ifelse(is.na(neg), 0, neg))) %>%
  mutate(cum_perc = cum_pos/(cum_pos+cum_neg)*100) %>%
  rename('visit'='folderName') %>%
  select(visit, neg, pos, pos_perc, cum_perc) %>%
  kable() %>%
  kable_styling()

#' ## Principle 3: Membranous (class V) LN more often presents with nephrotic
#' syndrome than proliferative LN (class III or IV)
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

compute_classes <- function(N){
  vars <- c(paste0("WHO",N), paste0("ISNRPS",N))
  vname <- c('LN_class'); names(vname) = paste0('LN',N)
  out <- all_rows %>%
    filter(varName %in% vars) %>%
    spread(varName, conceptValue) %>%
    distinct() %>%
    mutate(LN_class = ifelse(eval(expr(!!sym(vars[1]) =='1' | !!sym(vars[2]) == '1')), 1, 0)) %>%
    select(subjectId, folderName, LN_class) %>%
    rename(!!!vname)
  return(out)
}

LN_classes <- map(2:5, compute_classes) %>%
  Reduce(left_join, .)

## There seem to be some discrepancies in terms of unique data for individuals. Going
## diving into the biopsy data
biopsy <- data.table::fread(here('data/raw/biopsy_data_2020-01-31_1545.csv'))
dim(distinct(biopsy[,c('subjectId','eventId')])) # 1834 2
dim(distinct(biopsy[,c('subjectId','folderName')])) # 1820 2




#' ## Principle 4: Short term renal outcomes are worse in blacks of African American heritage

#' ## Principle 5: Short term renal outcomes are worse in patients who present with GFR < 60mL/min/1.73 m2
#' and/or nephrotic-range proteinuria (> 1 protein/creatinine ratio)

#' ## Principle 6: Rituximab has been used as a steroid-sparing agent for induction
#' in proliferative LN (LN vs no-LN, 3-4 vs 5)
#'
#' Rituximab use: IMMMED = 30
#' MEDCATON = 30

#' Is there differences in age/gender for people getting Rtx.

