#' ---
#' title: CARRA Analysis
#' author: Abhijit Dasgupta, PhD
#' date: "`r format(Sys.time(), '%B %d, %Y %I:%m %p')`"
#' output:
#'   rmdformats::material:
#'     css: style.css
#'     #toc: true
#'     #toc_depth: 3
#'     #toc_float: true
#'     #theme: architect
#'     #highlight: zenburn
#'     self_contained: true
#'     code_folding: hide
#' ---
#'
#+ preamble, echo=F, results='hide', message=F, warning=F
# Preamble ----------------------------------------------------------------
library(pacman)
p_load(char = c('readxl','tidyverse','data.table','glue','fs','here',
                'knitr','kableExtra', 'vroom', 'janitor'))
#source(here('lib/R/pval_scientific.R'))
for(f in dir_ls(here("lib/R"), glob = "*.R")){
  message(paste("Loading", basename(f)))
  source(f)
}

knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE,
                      cache = T)
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


write.table(data_dict, sep = '\t', file = here('background/f-6-337-13136911_ZH2vHUc6_111319_CARRA_Registry_11.0_DataDictionary_DCRI_SxxX.tsv'))


# Principle 1 -------------------------------------------------------------
#' # Principle 1 : 20% to 75% of children with SLE will develop nephritis
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


#' # Principle 2: 82% of LN in cSLE develops within the first year of diagnosis and 92% within 2 years
#'
#+ echo = F
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
  mutate(time_to_pos_biopsy = biopsdtc - dxdt)

# LN_pos <-  LN_ISNRPS %>% full_join(LN_WHO) %>%
#   mutate(LN_pos = ISNRPS_pos | WHO_pos) %>%
#   select(-ISNRPS_pos, -WHO_pos) %>%
#   mutate(folderName = as.factor(folderName)) %>%
#   mutate(folderName = fct_relevel(folderName, 'Baseline','3 month','6 month',
#                                   '12 month','18 month','24 month','30 month','36 month'))
# LN_pos %>%
#   count(folderName, LN_pos) %>%
#   spread(LN_pos, n) %>%
#   rename('pos' = `TRUE`, 'neg' = `FALSE`) %>%
#   mutate(pos_perc = pos / (pos + neg)*100) %>%
#   mutate(cum_pos = cumsum(ifelse(is.na(pos), 0, pos)),
#          cum_neg = cumsum(ifelse(is.na(neg), 0, neg))) %>%
#   mutate(cum_perc = cum_pos/(cum_pos+cum_neg)*100) %>%
#   rename('visit'='folderName') %>%
#   select(visit, neg, pos, pos_perc, cum_perc) %>%
#   kable() %>%
#   kable_styling()
biopsy %>%
  filter(LN==1) %>%
  mutate(time_to_pos_biopsy = ifelse(time_to_pos_biopsy==-1, 0,
                                     time_to_pos_biopsy)) %>%
  mutate(time_to_pos_biopsy = factor(time_to_pos_biopsy),
         time_to_pos_biopsy = fct_other(time_to_pos_biopsy,
                                        keep = as.character(-1:2),
                                        other_level = '3+')) %>%
  tabyl(time_to_pos_biopsy) %>%
  mutate(`Cumulative percent` = cumsum(percent)) %>%
  rename('Years' = time_to_pos_biopsy) %>%
  adorn_pct_formatting() %>%
  adorn_totals() %>%
  mutate(across(3:4, ~str_remove(.x, '-'))) %>%
  kable(caption = 'Time from diagnosis to positive biopsy') %>%
  kable_styling(full_width=FALSE)

visits <- vroom(here('data/raw/vis_data_2020-01-31_1545.csv'),
                col_select = c('subjectId','startDate','folderName','VISITDTC_YYYY'),
                .name_repair = 'universal') %>%
  clean_names()
timelines <- visits %>%
  group_by(subject_id) %>%
  summarize(start_date = unique(start_date), end_date = max(visitdtc_yyyy, na.rm=T))
timelines <- timelines %>%
  left_join(biopsy %>% select(subject_id, biopsdtc, LN, dxdt)) %>%
  mutate(last_date = pmin(end_date, biopsdtc, na.rm=T),
         LN = ifelse(is.na(LN), 0, LN)) %>%
  mutate(time_to_LN = last_date - dxdt) %>%
  filter(time_to_LN >= 0)


s2 <- survival::survfit(survival::Surv(time_to_LN, LN)~1,
                        data=timelines %>% filter(LN==1))
survminer::ggsurvplot(s2, risk.table = FALSE, conf.int = TRUE,
                      fun = 'event',
                      palette = 'lancet',
                      legend='none',
                      xlab='Time since diagnosis (years)',
                      ylab = "Probabilty of developing lupus nephritis")
#'
#' The following also shows the Kaplan-Meier curve for all subjects, with regard to LN incidence.
#'
s1 <- survival::survfit(survival::Surv(time_to_LN, LN)~1, data=timelines)
survminer::ggsurvplot(s1, risk.table = FALSE, conf.int = TRUE,
                      fun = 'event',
                      palette = 'lancet',
                      legend='none',
                      xlab='Time since diagnosis (years)',
                      ylab = "Probabilty of developing lupus nephritis")


#' > In this analysis there were 4 individuals who had a negative time
#' > between diagnosis and LN biopsy. I changed that time to 0, assuming
#' > that we're dealing with rounding error

# Principle 3 -------------------------------------------------------------
#' # Principle 3: Membranous (class V) LN more often presents with nephrotic syndrome than proliferative LN (class III or IV)
#'
#' Definition of LN classes:
#'
#' 1. Class III = WHO-3 or ISNRPS-3
#' 1. Class IV = WHO-4 or ISNRPS-4
#' 1. Class V = WHO-5 or ISNRPS-5
#'
#' Based on conversations (5/8/2020), we will create 3 mutually exclusive classes
#' for LN for this analysis. These are
#'
#' 1. Class III/IV only
#' 1. Class III/IV + V
#' 1. Class V only
#'

# This has been modified in biopsy_classes.R and in now included in the
# raw_biopsy dataset

#'
#' The definition of nephrotic syndrome is as follows:
#'
#' 1. The presence of nephrotic range proteinuria, which is a urine protein:creatinine ratio > 1mg/mg or if there is a 24 hour urine instead of a urine protein:creatinine ratio (different docs check it differently), it would be a 24 hour protein excretion greater than 3.5 g/24 hours.
#' 2. Hypoalbuminemia (an albumin less than 3 g/dL)
#' 3. On examination, documentation of edema
#'
#'
#' ### Poor availability of data
#'
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
#'
#' ### UPC analysis based on available data
#'
#' > In the following analysis, we took data from the first visit
#' > where LN was confirmed, which may be the baseline visit.
#' >
#' > There were 8 individuals who had different results in 2 biopsies;
#' > they were III/IV only in one and V only in another. I have placed
#' > them in the III/IV + V category
urine <- vroom(here('data/raw/urine_data_2020-01-31_1545.csv'))
urine <- urine %>%
  select(subjectId, visit = folderName, DIPSTICK, PROT30, URINRATO, RATIOUNT, SPOTURIN) %>%
  clean_names()

raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds')) %>%
  select(subject_id, visit, LN, LN34, LN345, LN50)
LN_subjects <- raw_biopsy %>%
  group_by(subject_id) %>%
  summarize(LN_ind = any(LN==1, na.rm=T)) %>%
  filter(LN_ind)
d <- urine %>% select(subject_id, visit, urinrato, ratiount, spoturin) %>%
  right_join(raw_biopsy) %>% semi_join(LN_subjects) %>%
  mutate(spoturin = ifelse(spoturin=='Not Done', NA, spoturin))

d <- d %>%
  mutate(visit_no = as.numeric(str_remove(visit, ' month'))) %>%
  mutate(visit_no = ifelse(visit=='Baseline', 0, visit_no),
         visit_no = ifelse(visit=='Unsch', 100, visit_no)) %>%
  mutate(visit = fct_reorder(visit, visit_no))

d1 <- d %>% filter(LN==1) %>%
  group_by(subject_id) %>%
  filter(visit_no == min(visit_no)) %>%
  mutate(across(LN34:LN50, ~ifelse(any(.x==1),1,0))) %>%
  ungroup() %>%
  distinct() %>%
  mutate(LN345 = ifelse(LN34==1 & LN50==1, 1, LN345),
         LN34 = ifelse(LN345==1, 0, LN34),
         LN50 = ifelse(LN345==1, 0, LN50),
         Other = ifelse(LN34==0 & LN345==0 & LN50==0, 1, 0))

d2 <- d1 %>%
  pivot_longer(
    cols = c(LN34:LN50, Other),
    names_to = 'LN_class',
    values_to = 'indic') %>%
  filter(indic==1)

#' #### Discrete UPC outcome

d2 %>%
  mutate(LN_class = factor(LN_class),
         LN_class = fct_recode(LN_class,
                               'Class III/IV only'='LN34',
                               'Class III/IV + V'='LN345' ,
                               'Class V only'='LN50' )) %>%
  rename('UPC in last 30 days' = spoturin) %>%
  table1::table1(~`UPC in last 30 days` | LN_class, data=.)

#' ##### Testing
#' If we take only the available data, we can do a bit of statistical testing.

out2 <- tabyl(d2 %>%
                rename('LN class' = 'LN_class',
                       'UPC in last 30 days' = spoturin)
              , `LN class`, `UPC in last 30 days`, show_na=FALSE)
out2 %>%   adorn_percentages() %>%
  adorn_totals('col') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  mutate(`LN class` = factor(`LN class`),
         `LN class` = fct_recode(`LN class`,
                                 'Class III/IV only'='LN34',
                                 'Class III/IV + V'='LN345' ,
                                 'Class V only'='LN50' )) %>%
  mutate(Test = c("Fisher's test",
                  paste('P-value =',round(fisher.test(out2)$p.value,2)),
                  '','')) %>%
  adorn_title() %>%
  kable(caption = 'Frequency distribution of UPC (discrete) by LN class using available data') %>%
  row_spec(1, bold=TRUE) %>%
  kable_styling()

#' #### Continuous UPC outcome

d2 %>%
  mutate(LN_class = factor(LN_class),
         LN_class = fct_recode(LN_class,
                               'Class III/IV only'='LN34',
                               'Class III/IV + V'='LN345' ,
                               'Class V only'='LN50' )) %>%
  rename('LN class' = 'LN_class',
         'UPC ratio' = urinrato) %>%
  table1::table1(~`UPC ratio` | `LN class`, data=.)


d2_anova = d2 %>% filter(LN_class != 'Other') %>%
  rename('LN class' = LN_class) %>%
  lm(urinrato ~ `LN class`, data=.) %>% broom::glance()
#' We can also perform an ANOVA analysis using available data to see whether
#' there are any differences in UPC ratio betwen the LN classes III/IV, III/IV + V,
#' and V only. This gives a p-value of `r round(d2_anova$p.value,2)`.
#'


#' ### Frequency distribution of LN classes
#'
#' As a descriptive analysis, we present the frequency distribution
#' of LN classes in this dataset
#'
#+ ln_classes

raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds'))
LN_classes <- raw_biopsy %>%
  group_by(subject_id) %>%
  summarise_at(vars(starts_with("LN")),
               ~ifelse(any(. == 1), 1, 0)) %>%
  ungroup()
saveRDS(LN_classes, here('data/rda/LN_classes.rds'), compress=T)

LN_classes %>% mutate(LN=ifelse(is.na(LN), 'No', 'Yes')) %>%
  tabyl(LN) %>%
  adorn_totals() %>%
  adorn_pct_formatting() %>%
  kable(caption = 'Lupus nephritis frequency') %>%
  kable_styling()

# There are edge cases where multiple biopsies have led to different
# LN classes. I'm taking the "any" approach in that the person's final
# class is based on all biopsies and any classes resulting from those
# biopsies
LN_classes <- LN_classes %>%
  filter(!is.na(LN)) %>%
  mutate(
    LN34 = ifelse((LN3==1 | LN4==1) & (LN5==0), 1, 0),
    LN345 = ifelse((LN3==1 & LN5==1) | (LN4==1 & LN5==1), 1, 0),
    LN50 = ifelse(LN5==1 & LN3==0 & LN4==0, 1, 0)
  )
LN_classes %>%
  select(LN34:LN50) %>%
  rename(`III/IV` = LN34,
         `III/IV + V` = LN345,
         `V only` = LN50) %>%
  pivot_longer(cols = everything(), names_to='LN Class', values_to = 'Indicator') %>%
  filter(Indicator==1) %>%
  tabyl(`LN Class`) %>%
  adorn_totals() %>%
  adorn_pct_formatting() %>%
  kable(caption = 'Frequency distribution of LN classes') %>%
  kable_styling()

#'
#' # Principle 4: Short term renal outcomes are worse in blacks
# Principle 4 -------------------------------------------------------------
#' Based on conversations, we will consider only LN patients and take their
#' first visit with confirmed LN as the baseline time for the GFR change
#' analysis. We will look at race, baseline GFR level, as well as LN classes,
#' as stratifying variables
#'
#+ outcomes
all_subjects <- readRDS(here('data/rda/all_subjects.rds'))
demographic <- readRDS(here('data/rda/demographic.rds')) # computed below
short_outcomes <- readRDS(here('data/rda/short_outcomes.rds')) %>%
  mutate(visit = as.character(visit))
visits <- vroom(here('data/raw/visit.list_data_2020-01-31_1545.csv')) %>%
  clean_names(case='snake') %>%
  select(subject_id, event_index, visit = event_type, visit_date)

raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds')) %>%
  select(subject_id:event_index, starts_with('biop'), starts_with("LN")) %>%
  filter(LN==1) %>%
  nest(-subject_id)
filter_fn <- function(d){
  if(nrow(d)>1){
    d <- d %>% filter(event_index == min(event_index, na.rm=T))
  }
  d <- d %>% select(-starts_with('biop')) %>%
    group_by(visit) %>%
    summarize_at(vars(starts_with('LN')), max, na.rm=T) %>%
    distinct() %>%
    ungroup()
  return(d)
}
raw_biopsy <- raw_biopsy %>%
  mutate(newdata = map(data, filter_fn)) %>%
  select(-data) %>%
  unnest(cols = c(newdata))

## Fix event_index for subject 139, 6 month visit
all_subjects$event_index[all_subjects$subject_id==139 &
                           all_subjects$visit=='6 month'] <- 2.5
all_subjects$event_index[all_subjects$subject_id==225 &
                           all_subjects$visit=='Unsch'] <- 2


prin4 <- all_subjects %>%
  left_join(demographic %>%
              select(white:othrace, subject_id)) %>%
  left_join(short_outcomes %>% select(-event_index)) %>%
  left_join(raw_biopsy) %>%
  filter(str_detect(visit, regex('Baseline|month|Unsch'))) %>%
  distinct()

prin4 <- prin4 %>%
  mutate(first_ln = ifelse(LN==1, 1, 0)) %>%
  filter_at(vars(creatval:first_ln), any_vars(!is.na(.))) %>%
  filter(!is.na(event_index))

## Only LN patients
ln_ids <- prin4 %>% filter(LN==1) %>% pull(subject_id) %>% unique()
prin4 <- prin4 %>%
  filter(subject_id %in% ln_ids) %>%
  mutate(first_ln = ifelse(first_ln==0, NA, first_ln)) %>%
  group_by(subject_id) %>%
  arrange(event_index) %>%
  fill(first_ln, .direction='down') %>%
  ungroup() %>%
  filter(!is.na(first_ln))

saveRDS(prin4, here('data/rda/prin4.rds'))

all_rows <- vroom(here('data/raw/all_rows_data_2020-01-31_1545.csv'))
LN_classes <- map(2:5, compute_classes) %>%
  Reduce(left_join, .)

#' There are a total of `r length(unique(raw_biopsy$subject_id[raw_biopsy$LN==1]))` LN
#' positive subjects in this study. Among these individuals, many are missing
#' information on outcomes, or multiple visits post-diagnosis of LN. For example,
#' if we look at availability of data by visit among people who are LN+, for
#' the visits on or after their LN diagnosis, we get this picture:
#'
#+ outcome_missing
prin4 %>%
  mutate(visit = as.factor(visit),
         visit = fct_relevel(visit, 'Baseline','6 month')) %>%
  group_by(visit) %>%
  summarize_at(vars(creat_status:remission), naniar::pct_miss) %>%
  ungroup() %>%
  kable(digits = 1, caption = 'Percent missing data by outcome and visit') %>%
  kable_styling()

#' Also, 20 percent of LN+ subjects had only 1 available visit, so any change
#' is not observable
prin4 %>% count(subject_id) %>% tabyl(n) %>%
  mutate(n = as.character(n)) %>%
  adorn_totals() %>%
  mutate(percent = 100*percent) %>%
  kable(caption = "Frequency of the number of visits per subject post LN diagnosis",
        col.names = c('Number of visits','Frequency','Percent'),
        digits=2) %>%
  kable_styling(full_width = F)

#' > The information available for subject 597 showed that diagnosis
#' > was at an unscheduled visit, and where that visit was temporally
#' > between baseline, 6 month and 12 month visit was not available. So
#' > this subject was removed from the analysis since the number of
#' > visits post diagnosis could not be definitively computed for
#' > that subject.
#'
#' ### GFR changes
#'
#+ gfr_change_black

gfr_id <- prin4 %>%
  filter(!is.na(gfr_class)) %>%
  count(subject_id) %>%
  filter(n>1) %>%
  pull(subject_id)

prin4 %>%
  filter(subject_id %in% gfr_id) %>%
  filter(!is.na(gfr_class)) %>%
  arrange(subject_id,event_index) %>%
  group_by(subject_id) %>%
  mutate(first_visit = visit[event_index == min(event_index)],
         last_visit = visit[event_index==max(event_index)],
         egfr_visit = ifelse(event_index == min(event_index), 'First','Last'),
         black = ifelse(black==1, 'Black','Non-black')) %>%
  filter(event_index==min(event_index)|event_index==max(event_index)) %>%
  ungroup() %>%
  select(subject_id, black, first_visit, last_visit,
         egfr_visit, gfr_class) %>%
  spread(egfr_visit, gfr_class) -> tmp

tabs <- tabyl(dat=tmp, First, Last, black) %>% adorn_totals('col') %>% adorn_percentages('row') %>% adorn_pct_formatting() %>% adorn_ns() %>% adorn_title()

kable(tabs$Black, caption = 'Change in GFR stage among blacks') %>%
  kable_styling()
kable(tabs$`Non-black`, caption = 'Change in GFR stage among non-blacks') %>%
  kable_styling()

#+ permutation, cache=TRUE
## Permutation test

if(!file.exists('worse.rds')){
  worse = rep(0, 1000)
  set.seed(1034)
  for(i in 1:5000){
    # print(i)
    worse[i] <- tmp %>%
      mutate(bl = sample(black)) %>%
      filter(bl=='Black') %>%
      count(First, Last) %>%
      filter(First=='Stage 1', Last != 'Stage 1') %>%
      pull(n) %>%
      sum()
  }
  saveRDS(worse,'worse.rds')
} else{
  worse <- readRDS('worse.rds')
}

#' We can perform a permutation test to see if blacks do worse than non-blacks
#' insofar as the chance of worsening GFR state if the initial GFR state was Stage 1
#' at time of LN diagnosis. Using 5000 permutations of black status, we find
#' that the permutation test gives a p-value of `r round(mean(worse >= 4), 2)`,
#' thus showing some evidence that blacks tend to worsen at a higher rate than
#' non-blacks.
#'
#' We can also look at the actual eGFR values to see if there is a difference
#' in eGFR overall between the time of LN diagnosis and when they are last seen
#'

prin4 %>% filter(subject_id %in% gfr_id) %>%
  filter(!is.na(eGFR)) %>%
  arrange(subject_id,event_index) %>%
  group_by(subject_id) %>%
  mutate(first_visit = visit[event_index == min(event_index)],
         last_visit = visit[event_index==max(event_index)],
         egfr_visit = ifelse(event_index == min(event_index), 'First','Last'),
         black = ifelse(black==1, 'Black','Non-black')) %>%
  filter(event_index==min(event_index)|event_index==max(event_index)) %>%
  ungroup() %>%
  select(subject_id, black, first_visit, last_visit, egfr_visit, eGFR) %>%
  spread(egfr_visit, eGFR) %>%
  mutate(egfr_change = Last - First) %>%
  tableone::CreateTableOne(vars = c('egfr_change'), strata = c('black')) %>%
  print(nonnormal = c('egfr_change'), printToggle=F) %>%
  tableone::kableone() %>%
  kable_styling()

#' This shows no evidence overall that eGFR changes from time of diagnosis.
#' This is consistent with the previous result which shows that the vast
#' majority of LN patients stay in the same eGFR stage after LN diagnosis.
#'
#' ### Remission
#' We are defining remission by the following 2 criteria:
#'
#' - Creatinine within normal range
#' - urine red blood cells\<10/high powered field

tb <- prin4 %>% group_by(subject_id) %>%
  filter(event_index==min(event_index)) %>% tabyl(remission)

#' At diagnosis, this can be assessed for `r sum(tb$n[1:2])` patients, which is
#' `r round(100*sum(tb$percent[1:2]),2)`% of all
#' patients. Of those for whom remission state is observed,
#' `r tb$n[2]` or
#' `r round(100*tb$valid_percent[2],2)`%
#' entered the study in the remission state. Separating between blacks and non-blacks:

prin4 %>% mutate(black = ifelse(black==1, 'Yes','No')) %>%
  group_by(subject_id) %>%
  filter(event_index==min(event_index)) %>%
  ungroup() %>%
  tabyl(black, remission) %>%
  adorn_percentages('col') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title() %>%
  kable() %>%
  kable_styling()

#'
#' We will now just look at individuals who were not in remission state at diagnosis

remission_id <- prin4 %>%
  group_by(subject_id) %>%
  filter(event_index==min(event_index), !is.na(remission), remission=='No') %>%
  ungroup() %>%
  pull(subject_id)

#' Of these individuals, `r round(100*(prin4 %>% filter(subject_id %in% remission_id) %>% count(subject_id) %>% summarize(mean(n>1)) %>% pull()),2)`%
#' have at least one subsequent visit. We'll investigate these subjects for subsequent remission. The following
#' table shows the frequency distribution of individuals who subsequently got to remission state at some point for blacks and
#' non-blacks.
#'

remission_id_mult <- prin4 %>% filter(subject_id %in% remission_id) %>%
  count(subject_id) %>% filter(n>1) %>% pull(subject_id)
tbl_black_remission <- prin4 %>% filter(subject_id %in% remission_id_mult) %>%
  mutate(black = ifelse(black==1, 'Yes','No')) %>%
  group_by(subject_id) %>%
  mutate(remission = ifelse(any(remission=='Yes', na.rm=T), 'Yes','No')) %>%
  ungroup() %>%
  select(subject_id, black, remission) %>%
  distinct() %>%
  tabyl(black, remission)
tbl_black_remission %>%
  adorn_percentages('col') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title() %>%
  kable() %>%
  kable_styling()

#' This is not statistically significant using Fisher's test
#'
#' ### Dialysis, transplant and ESRD
#'
#' All these outcomes are sparse in this data set, even on restricting to
#' subjects who have been diagnosed with lupus nephritis

prin4 %>%
  tabyl(dialysis) %>%
  mutate_at(vars(contains('percent')), ~100*.x) %>%
  kable(caption = 'Frequency distribution for dialysis', digits=2) %>%
  kable_styling(full_width = F)

prin4 %>%
  tabyl(transplant) %>%
  mutate_at(vars(contains('percent')), ~100*.x) %>%
  kable(caption = 'Frequency distribution for transplant', digits=2)%>%
  kable_styling(full_width = F)

prin4 %>%
  tabyl(esrd) %>%
  mutate_at(vars(contains('percent')), ~100*.x) %>%
  kable(caption = 'Frequency distribution for ESRD', digits=2)%>%
  kable_styling(full_width = F)


#' # Principle 5: Short term renal outcomes are worse in patients who present with GFR < 60mL/min/1.73 m2 and/or nephrotic-range proteinuria (> 1 protein/creatinine ratio)
# Principle 5 -------------------------------------------------------------

#+ echo = FALSE
prin4 <- readRDS(here('data/rda/prin4.rds'))
# We start with prin4, which only contains subjects with LN and starts
# their time at time of diagnosis

prin4 %>% group_by(subject_id) %>%
  filter(event_index ==  min(event_index)) %>% # Just look at visit where LN+ diagnosed
  ungroup() %>%
  tabyl(gfr_class) %>%
  mutate_at(vars(contains('percent')), ~100*.x) %>%
  kable(caption = 'Distribution of GFR class at time of LN diagnosis',
        digits=2) %>%
  kable_styling()

#' We see that 93% of the available GFR classes are in Stage 1, and
#' only 7% are worse than Stage 1.
#'
#' ### GFR changes
#'
#+ gfr_change

gfr_id <- prin4 %>%
  filter(!is.na(gfr_class)) %>%
  count(subject_id) %>%
  filter(n>1) %>%
  pull(subject_id)

prin4 %>%
  filter(subject_id %in% gfr_id) %>%
  filter(!is.na(gfr_class)) %>%
  arrange(subject_id,event_index) %>%
  group_by(subject_id) %>%
  filter(event_index==min(event_index)|event_index==max(event_index)) %>%
  mutate(first_visit = visit[event_index == min(event_index)],
         last_visit = visit[event_index==max(event_index)],
         egfr_visit = ifelse(event_index == min(event_index), 'First','Last'),
         black = ifelse(black==1, 'Black','Non-black')) %>%
  ungroup() %>%
  select(subject_id, black, first_visit, last_visit,
         egfr_visit, gfr_class) %>%
  spread(egfr_visit, gfr_class) -> tmp

tabyl(dat=tmp, First, Last) %>% adorn_percentages('row') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title() %>%
  kable(caption = 'Changes in GFR class between diagnosis time/baseline and last visit') %>%
  kable_styling()


#' So, no one starting in Stage 2 or 3 gets worse, while 4% of people
#' starting in Stage 1 do get worse.
#'

prin4 %>%
  filter(subject_id %in% gfr_id) %>%
  filter(!is.na(gfr_class)) %>%
  arrange(subject_id,event_index) %>%
  group_by(subject_id) %>%
  filter(event_index==min(event_index)|event_index==max(event_index)) %>%
  mutate(first_visit = visit[event_index == min(event_index)],
         last_visit = visit[event_index==max(event_index)],
         egfr_visit = ifelse(event_index == min(event_index), 'First','Last'),
         black = ifelse(black==1, 'Black','Non-black')) %>%
  ungroup() %>%
  select(subject_id, black, first_visit, last_visit,
         egfr_visit,eGFR) %>%
  spread(egfr_visit, eGFR) %>%
  mutate(gfr_change = Last-First) %>%
  mutate(first_stage2 = ifelse(First <= 60, 'Stage 2+','Stage 1'))-> tmp1

tmp2 <- tmp1 %>% gather(visit, value, First, Last)
tmp2 <- tmp2 %>%
  mutate(visit = factor(paste(visit, 'visit'))) %>%
  mutate(x_pos = as.numeric(visit) + runif(nrow(.),-0.1, 0.1))


ggplot(tmp2, aes(x = x_pos, y = value, color = first_stage2))+
  geom_line(aes(group = subject_id), alpha = 0.2, size=0.7,
            show.legend = FALSE)+
  geom_point(shape=15, stroke=1,
             show.legend = FALSE) +
  geom_hline(yintercept = 60, linetype=2, alpha = 0.5) +
  facet_wrap(~first_stage2)+
  scale_x_continuous('', breaks = c(1,2), labels = c('First visit','Last visit')) +
  scale_y_continuous(bquote('eGFR level (mL / min / 1.73'~m^2~')'),
                     breaks = c(0,60,100, 200, 300))+
  coord_cartesian(xlim = c(0.5,2.5)) +
  labs(color = 'First visit stage') +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(face='bold', size=12))+
  ggsci::scale_color_npg()

#'
#' ### Remission

prin4 %>% filter(subject_id %in% remission_id_mult) %>%
  group_by(subject_id) %>%
  mutate(gets_to_remission = ifelse(any(remission=='Yes', na.rm=T),1,0)) %>%
  filter(event_index == min(event_index)) %>%
  ungroup() %>%
  select(subject_id, event_index, gfr_class, gets_to_remission) %>%
  filter(!is.na(gfr_class)) %>%
  rename(Remission = gets_to_remission, `GFR class` = gfr_class) %>%
  mutate(Remission = ifelse(Remission==1, 'Yes','No')) %>%
  rename(`Ever in remission` = Remission) %>%
  tabyl(`Ever in remission`, `GFR class`) -> tab_gfr_remission

tab_gfr_remission %>%
  adorn_percentages('col') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title() %>%
  kable(caption = 'Chance of getting to remission by GFR class at LN diagnosis') %>%
  kable_styling()

#' A Fishers exact test gives a p-value of
#' `r tab_gfr_remission %>% fisher.test() %>% broom::tidy() %>% pull(p.value)`.
#'
#' ### Dialysis, transplant and ESRD
#'
#' As we saw earlier, we don't have sufficient information on these outcomes
#' for this subset of subject who are LN-positive to assess how
#' GFR stage at diagnosis is associated with them.
#'
#' # Principle 6: Rituximab has been used as a steroid-sparing agent for induction in proliferative LN (LN vs no-LN, 3-4 vs 5)
#'
#' Rituximab use: IMMMED = 30
#' MEDCATON = 30
#'
#' ### Rituximab use by nephritis class
#+ rtx
# Principle 6 -------------------------------------------------------------

raw_biopsy <- readRDS(here('data/rda/biopsy_classes.rds')) # Updated for LN34+5 class
all_rows <- vroom(here('data/raw/all_rows_data_2020-01-31_1545.csv'))
ritux <- all_rows %>% filter(conceptValue=='Rituximab (Rituxan)') %>%
  clean_names() %>%
  select(subject_id, ritux = concept_value) %>%
  distinct()
## LN_classes was created in Principle 3
LN_classes <- readRDS(here('data/rda/LN_classes.rds'))
tab_rtx_ln <- LN_classes  %>%
  left_join(ritux) %>%
  mutate(Rituximab = ifelse(is.na(ritux), 'No','Yes'),
         LN = ifelse(is.na(LN),'Neg', 'Pos')) %>%
  tabyl(Rituximab, LN)
tab_rtx_ln %>%
  adorn_percentages('col') %>%
  adorn_totals('row') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title() %>%
  knitr::kable(caption = 'Rituximab use between LN and non-LN subjects') %>%
  kable_styling()

#'
#' This is statistically significant, with the $\chi^2$ test p-value being
#' `r pval_scientific(chisq.test(tab_rtx_ln)$p.value)`.
#'
#+ rtx_ln
tbl_rit_class <- LN_classes %>%
  left_join(ritux) %>%
  mutate(Rituximab = ifelse(is.na(ritux), 'No', 'Yes')) %>%
  filter(!is.na(LN)) %>%
  select(subject_id, LN34:LN50, Rituximab) %>%
  rename(`III/IV` = LN34,
         `III/IV + V` = LN345,
         `V only` = LN50) %>%
  gather(Class, value, `III/IV`:`V only`) %>%
  filter(value==1) %>%
  tabyl(Class, Rituximab)

tbl_rit_class %>%
  adorn_percentages('col') %>%
  adorn_totals() %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title() %>%
  kable(caption = 'Rituximab use by LN class') %>%
  kable_styling()

#' This is not statistically significant (p-value = `r chisq.test(tbl_rit_class)$p.value`)
#'
#' ### Is there differences in age/gender for people getting Rituximab
#'
#+ rtx_age
demographic <- readRDS(here('data/rda/demographic.rds'))
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
#'
#+ rtx_gender

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
#'
#' ### Differences in GFR by Rituximab use
#'
#' We will look at LN+ patients and their GFR at time of diagnosis or at
#' baseline, and compare the GFR levels for Rituximab users and non-users
#'
#+ rtx_gfr
# prin4 is the dataset we will use, looking at the time of diagnosis,
# and then linking with Rituximab use

opts <- options(knitr.kable.NA='')
rtx_gfr <- prin4 %>% group_by(subject_id) %>%
  filter(event_index == min(event_index)) %>%
  ungroup() %>%
  left_join(ritux) %>%
  mutate(Rituximab = ifelse(is.na(ritux),'No','Yes'))
rtx_gfr %>%
  group_by(Rituximab) %>%
  summarize(N = sum(!is.na(eGFR)),
            Mean = mean(eGFR, na.rm=T),
            Median = median(eGFR, na.rm=T),
            SD = sd(eGFR, na.rm=T),
            IQR = IQR(eGFR, na.rm=T)) %>%
  mutate(`P-value` = c(wilcox.test(eGFR~Rituximab, data=rtx_gfr)$p.value, NA)) %>%
  kable(caption = 'GFR summaries by Rituximab use among LN patients;
        p-value based on Wilcoxon test',
        digits=2) %>%
  kable_styling(full_width = F)

options(opts)

#+ rtx_gfr_plot, fig.width=7
ggplot(rtx_gfr, aes(x = Rituximab, y = eGFR))+
  geom_violin(draw_quantiles = 0.5)+
  ggpubr::stat_compare_means(method='wilcox.test',
                             label.x.npc = 0.8)+
  scale_y_log10() +
  theme_classic() +
  labs(x = 'Rituximab use', y = 'eGFR')+
  ggtitle('Distribution of eGFR by Rituximab use among LN patients')

#' > The violin plots have the medians marked. Hypothesis testing to test if the
#' > change in eGFR was the same in the two groups was performed using a
#' > Wilcoxon rank-sum test, with the two-sided alternative.

#'
#'### Rituximab and concurrent medications
#'
#+ rtx_meds

meds <- vroom(here('data/raw/meds_data_2020-01-31_1545.csv'),
              col_select = c(subjectId, MEDCATON)) %>%
  clean_names() %>%
  mutate(medication = str_to_title(medcaton)) %>%
  select(-medcaton)

tab_rtx_med <- LN_classes %>% left_join(ritux) %>% left_join(meds) %>%
  filter(!is.na(LN)) %>%
  filter(medication != 'Rituximab (Rituxan)') %>%
  mutate(ritux = ifelse(is.na(ritux), 'No','Yes')) %>%
  tabyl(medication, ritux)
o <- order(tab_rtx_med$Yes)

n_ritux = nrow(ritux)
n_noritux = nrow(LN_classes) - nrow(ritux)

library(glue)
tab_rtx_med %>%
  mutate(perc_yes = Yes/n_ritux*100,
         perc_no = No/n_noritux*100,
         out_yes = glue("{round(perc_yes,2)}% ({Yes})"),
         out_no = glue("{round(perc_no,2)}% ({No})")) %>%
  slice_max(perc_yes, n = 10) %>%
  select(medication, out_no, out_yes) %>%
  set_names(c('Medication',glue('No Ritux (N = {n_noritux})'),
              glue('With Ritux (N = {n_ritux})'))) %>%
  kable(caption = 'Top 10 drugs among RTX users and
        corresponding proportions among non-RTX patients') %>%
  kable_styling()


# out_rtx_med <- tab_rtx_med %>%
#   adorn_percentages('col') %>%
#   adorn_pct_formatting() %>%
#   adorn_ns() %>%
#   slice(rev(o))
#
# tab_rtx_med %>%
#   adorn_percentages('col') %>%
#   adorn_pct_formatting() %>%
#   adorn_ns() %>%
#   slice(rev(o)) %>%
#   slice(1:10) %>%
#   kable(caption = 'Top 10 drugs among RTX users and
#         corresponding proportions among non-RTX patients') %>%
#   kable_styling()

tab_rtx_med  %>%
  mutate(perc_yes = Yes/n_ritux*100,
         perc_no = No/n_noritux*100) %>%
    mutate(pct_diff = perc_yes - perc_no) %>%
  slice_max(abs(pct_diff), n=10) %>%
  rename(Difference = pct_diff) %>%
  # select(medication, perc_no, perc_yes, Difference) %>%
  mutate(across(perc_yes:Difference, ~paste0(round(.x,2),'%'))) %>%
  mutate(perc_no = glue("{perc_no} ({No})"),
         perc_yes = glue('{perc_yes} ({Yes})')) %>%
  select(medication, perc_no, perc_yes, Difference) %>%
  kable(caption = 'Top 10 drugs in difference of usage between RTX and non-RTX',
        col.names = c('Medication',glue('No Ritux (N = {n_noritux})'),
                      glue('With Ritux (N = {n_ritux})'),
                      'Difference')) %>%
  kable_styling()

# openxlsx::write.xlsx(list('Medications' = out_rtx_med,
#                           'Differences' = out_rtx_med_diff),
#                     file = here('data/raw/rtx_meds.xlsx'))
#'
#'
#' ## Session information
#'
#' This analysis was done using `r R.version$version.string` and the following packages
#+ packages, results = 'asis', echo = FALSE
pkgs <- p_loaded() %>% sort()
d <- tibble(Package = pkgs) %>%
  mutate(Version = map(Package, p_ver) %>% map_chr(as.character))
bl <- glue::glue_data(d, '{Package} ({Version})') %>% paste(collapse = '; ')
cat(bl)

