#' ---
#' title: Response to 2020-07-16 e-mail
#' author: Abhijit Dasgupta, PhD
#' date: "`r format(Sys.time(), '%B %d, %Y %I:%m %p')`"
#' output:
#'   prettydoc::html_pretty:
#'     theme: architect
#'     #toc: false
#'     #toc_depth: 3
#'     #toc_float:
#'       #collapsed: true
#'       #smooth_scroll: false
#'     #theme: yeti
#'     highlight: zenburn
#'     css: style.css
#'     self_contained: true
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

#' ##
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
#' ::: {.query}
#' For principle 2,  the total of LN is 217 instead of 235. I think this wasn’t updated when we added in the rest of the nephritis patients (class I and II) to principle 1
#' :::
#'
#' This wasn't quite the problem. I had truncated the table to 2 or fewer years, and
#' so several individuals were omitted from this table. It has now been fixed in the original report (summaries.html, attached)
#'

# kaplan meier ------------------------------------------------------------


#' ::: {.query}
#' For principle 2, can we do a kaplan meyer curve for time to nephritis?
#' :::
#'
#' Yes, I can, though it will be a bit choppy due to the fact that we only have
#' years since diagnosis, and not more granular data. Added to summaries.html
#'

#' ### UPC information
#'
#' ::: {.query}
#' For principle 3, can you please report the UPC by class both the numerical and categorical variable? I know the data is limited but we’d like to report what we have
#' :::
#'
#' This analysis has been added to Principle 3 in summary.html
#'
#' ::: {.query}
#' For principle 4, the GFR table total is 234 not 235 including NA. Where did the one patient go?
#' :::
#'
#' This has been answered above
#'
#' ::: {.query}
#' For principle 5, what is the p-value for the change in GFR? Can we do this by collapsing state 2 and state 3 into one group and compare GFR >60 vs <60?
#' :::
#'
#' This is an ill-posed problem. What is the hypothesis test you're asking for? If
#' you're testing H~0~: *No difference between Stage 1 and Stage 2+3 in what proportion get worse* vs H~1~: *Stage 2+3 gets worse more often than Stage 1*,
#' which is how I interpret the principle, then
#' you have a problem. People starting in Stage 2+3 cannot get worse.
#'
#' You could consider if eGFR values worsen on average for non-Stage 1 patients
#' compared to Stage 1 patients.
#'
prin4 <- readRDS(here('data/rda/prin4.rds'))

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
         egfr_visit,eGFR) %>%
  spread(egfr_visit, eGFR) %>%
  mutate(gfr_change = Last-First) %>%
  mutate(first_stage2 = ifelse(First <= 60, 'Stage 2+','Stage 1'))-> tmp

#+ eval=FALSE
ggplot(tmp, aes(x = first_stage2, gfr_change))+
  geom_violin(draw_quantiles = 0.5)+
  ggpubr::stat_compare_means(method='wilcox.test',
                             method.args = list(alternative = 'less'),
                             label.x = 0.5)+
  theme_minimal()+
  labs(x = 'Stage at first visit',
       y = 'eGFR change between first and last visit')

# This plot clearly shows that individuals who start at Stage 2+ (eGFR <= 60)
# have strong **improvement** by their last visit.
#
# > The violin plots have the medians marked. Hypothesis testing to test if the
# > change in eGFR was the same in the two groups was performed using a
# > Wilcoxon rank-sum test, with the alternative hypothesis being that median eGFR
# > among Stage 2 subjects is **less than** median eGFR among Stage 1 subjects.
#
#+ eval=TRUE
tmp2 <- tmp %>% gather(visit, value, First, Last)
tmp2 <- tmp2 %>%
  mutate(visit = factor(paste(visit, 'visit'))) %>%
  mutate(x_pos = as.numeric(visit) + runif(nrow(.),-0.1, 0.1))


ggplot(tmp2, aes(x = x_pos, y = value, color = first_stage2))+
  geom_line(aes(group = subject_id), alpha = 0.2, size=0.7)+
  geom_point(shape=15, stroke=1) +
  geom_hline(yintercept = 60, linetype=2, alpha = 0.5)+
  scale_x_continuous('', breaks = c(1,2), labels = c('First visit','Last visit'))+
  scale_y_continuous(bquote('eGFR level (mL / min / 1.73'~m^2~')'),
                     breaks = c(0,60,100, 200, 300))+
  coord_cartesian(xlim = c(0.5,2.5))+
  labs(color = 'First visit stage')+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(face='bold', size=12))+
  ggsci::scale_color_npg()

#' ::: {.query}
#' For principle 6, for the top medications- which patients are being compared? What is the denominator?
#' :::
#'
#' Thanks for bringing this up. It looks like I didn't quite do the right thing
#' before (hence your confusion). I've updated the tables in summary.html. Denominators for each entity
#' is the number of Ritux users or non-Ritux subjects as the case might be for each
#' column. This is what makes sense to me. So the interpretation of the *With Ritux* entries is the proportion of Ritux users who also took the drug in question.
