#' ---
#' title: Evaluation of short term outcomes
#' author: Abhijit Dasgupta
#' output:
#'   html_document:
#'     theme: sandstone
#'     highlight: zenburn
#'     code_folding: hide
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#' ---
#'
#' # Definitions
#'
#' - GFR will be separated into states for analysis:
#'   - State 1: \>60ml/min/1.73 m2
#'   - State 2: 30-60ml/min/1.73 m2
#'   - State 3: \<30ml/min1.73 m2
#' - Assess change in GFR- initial GFR vs most recent GFR
#' - Occurrence of remission:
#' - Creatinine within normal range:
#'
#'   Age                              Creatinine (serum)
#' -------------------------------- -------------------- --------------
#' Child (\<12 years old)           0.3-0.7 mg/dL        27-62 umol/L
#' Adolescent (12-17 years old)     0.5-1.0 mg/dL        44-88 umol/L
#' Adult (18 years old and older)   0.6-1.1 mg/dL        53-97 umol/L
#'
#' -   urine protein:creatinine ratio\<0.2
#' -   urine red blood cells\<10/high powered field
#' -   Occurrence of end-stage renal disease, transplant, and/or dialysis.
#'
#' ## Creatinine conversions
#'
#' To convert umol/l to mg/dl, multiply by 0.0113. To convert mg/dl to umol/l, multiply by 88.4.
#' > I think it would be safe to say that values that are under 10 should be mg/dL and
#' > greater than 10 should be umol/L. Laura, do you agree?
#'
#' # Availability in data
#+ preamble, include = F
abhiR::reload()
knitr::opts_chunk$set(message = F, warning = F)
data_dict <- readRDS(here('data','rda','data_dict.rds'))
all_subjects <- vroom(here('data/raw/all_rows_data_2020-01-31_1545.csv'),
                      col_select = c(subjectId, visit = folderName)) %>%
  distinct() %>%
  mutate(folderName = fct_relevel(visit, 'Baseline','6 month','12 month','18 month','24 month')) %>%
  select(-folderName) %>%
  clean_names()

#' ## GFR
#'
#' GFR is present as eGFR within the labs data
#+ gfr, echo = FALSE
data_dict %>% filter(str_detect(variable_description, 'GFR'), data_type=='Text') %>%
  select(Variable = variable_name, Description=variable_description,
         Type = data_type) %>%
  kable() %>%
  kable_styling(full_width = TRUE)
#'Availability of GFR data
# raw GFR availability ----------------------------------------------------
lab_blood <- vroom(here('data/raw/labs_data_2020-01-31_1545.csv'),
                   col_select = c(subjectId, visit = folderName, eventIndex, EGFRVAL)) %>%
  clean_names() %>% right_join(all_subjects)
lab_blood %>%
  filter(visit %in% c('Baseline',paste(c(6,12,18,24), 'month'))) %>%
  mutate(visit = fct_relevel(visit, 'Baseline','6 month')) %>%
  mutate(miss_egfr = is.na(egfrval)) %>%
  tabyl(visit, miss_egfr) %>%
  adorn_percentages('row') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  rename('EGFR Present' = 'FALSE',
         'EGFR Missing' = 'TRUE') %>%
  adorn_title(row_name='Visit', col_name='Data status') %>%
  kable() %>%
  kable_styling() %>%
  row_spec(0:1, bold=T)
#' In terms of looking at changes in EGFR over time, we can see how many
#' individuals have more than one valid EGFR value
perc2frac <- function(x) as.numeric(str_remove(x,'%'))/100

out <- lab_blood %>%
  filter(visit %in% c('Baseline',paste(c(6,12,18,24),'month'))) %>%
  group_by(subject_id) %>%
  summarise(n_entries = sum(!is.na(egfrval))) %>%
  tabyl(n_entries) %>%
  mutate(cumperc = cumsum(percent)) %>%
  adorn_totals() %>%
  mutate(n = as.character(n)) %>%
  adorn_pct_formatting() %>%
  set_names('Available EGFR per subject','N','Percent','Cum Percent')
out[nrow(out),ncol(out)] <- ''
kable(out) %>%
  kable_styling() %>%
  row_spec(nrow(out), bold=T)
#' We see that only `r 100*(1-perc2frac(out[2,4]))`% of subjects have more than
#' one EGFR value; these are the only individuals on which we could potentially
#' evaluate improvement. Note that `r out[1,3]` of subjects have **no** available
#' EGFR value
#' We can also compute the eGFR as 0.413 x height / serum creatinine

# Computed eGFR from sCr and heights --------------------------------------
#' ## Serum Creatinine
#+
lab_screat <- vroom(here('data/raw/labs_data_2020-01-31_1545.csv'),
                    col_select = c(subjectId, visit = folderName,
                                   CREATVAL)) %>%
  clean_names(case='snake') %>%
  mutate(creatval = ifelse(creatval >= 10, 0.0113*creatval, creatval)) %>% # account for units
  right_join(all_subjects) %>%
  filter(visit %in% c('Baseline',paste(seq(6,24,by=6),'month'))) %>%
  mutate(miss_creat = is.na(creatval)) %>%
  mutate(visit = fct_relevel(visit, 'Baseline','6 month')) %>%
  left_join(vroom(here('data/raw/dem_data_2020-01-31_1545.csv')) %>%
              clean_names() %>%
              select(subject_id, dxdage)) %>%  # Adding age
  mutate(age_cat = cut(dxdage, breaks = c(0, 12, 18, Inf),
                       labels = c('< 12', '12-17', '18+'),
                       right=F)) %>%
  mutate(norm_min = case_when( # min norms
    age_cat=='< 12' ~ 0.3,
    age_cat=='12-17' ~ 0.5,
    age_cat=='18+' ~ 0.6
  ),
    norm_max = case_when( # max norms
      age_cat == '< 12' ~ 0.7,
      age_cat == '12-17' ~ 1.0,
      age_cat == '18+' ~ 1.1
    )) %>%
  mutate(creat_status = ifelse(creatval >= norm_min & creatval <= norm_max,
                               'Normal','Abnormal')) # determine if normal

heights <- vroom(here('data/raw/phyexam_data_2020-01-31_1545.csv'),
                 col_select = c(subjectId, visit = folderName, eventIndex, HTORRES)) %>%
  clean_names() %>%
  left_join(vroom(here('data/raw/dem_data_2020-01-31_1545.csv')) %>%
              clean_names() %>%
              select(subject_id, dxdage)) %>%
  mutate(htorres = ifelse(dxdage > 5 & htorres < 80, htorres*2.54, htorres)) # Units problem fixed, everything in cm

egfr_computed_d <- all_subjects %>% left_join(lab_screat) %>% left_join(heights) %>%
  mutate(eGFR = 0.413 * htorres / creatval) %>%
  filter(str_detect(visit, 'Baseline|month')) %>%
  mutate(visit = fct_relevel(visit, c('Baseline','3 month','6 month',
                                      '9 month','12 month','18 month',
                                      '24 month','30 month','36 month'))) %>%
  mutate(gfr_class = case_when(
    eGFR < 30 ~ 'Stage 3',
    eGFR > 6 ~ 'Stage 1',
    TRUE ~ 'Stage 2'
  ))
#'
#' ## Urine protein:creatinine ratio
#'
#' I've addressed the availability of the UPC ratio in my nephrotic syndrome report
#'
#' ## Urine RBC
#'
#' Urine RBC is available as a dichotomized variable with levels < 5/hpf and >= 5/hpf, rather than the cutoff of 10/hpf that you specify
#'
urine <- vroom(here('data/raw/urine_data_2020-01-31_1545.csv'),
               col_select = c(subjectId, visit = folderName, URINRBC)) %>%
  clean_names(case='snake') %>%
  right_join(all_subjects) %>%
  filter(visit %in% c('Baseline',paste(seq(6,24,by=6), 'month'))) %>%
  mutate(visit = fct_relevel(visit, 'Baseline','6 month')) %>%
  mutate(miss_urbc = (is.na(urinrbc)|urinrbc=='Not Done'))

out <- urine %>%
  tabyl(visit, urinrbc) %>%
  adorn_totals('col') %>%
  adorn_percentages('row') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  rename('No urinanalysis' = 'NA_',
         'Visit' = 'visit',
         '> / = 5 RBC/hpf' = '> / = 5 RBC/hfp')
out %>%
  kable() %>%
  kable_styling() %>%
  column_spec(c(1,ncol(out)), bold=T)


#' ## ESRD, Dialysis or Transplant
#'
#' Variable    Description
#' --------    ------------
#' ESRENAL     End-stage renal disease (regardless of dialysis or transplantation)
#' SLEDS7      Have you ever had a kidney transplant?
#' SLEDS8      Have you ever been on dialysis?
#'
