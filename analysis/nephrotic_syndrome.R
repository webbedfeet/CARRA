#' ---
#' title: Defining nephrotic syndrome in the data
#' author: Abhijit Dasgupta
#' date: February 20, 2020
#' output:
#'   html_document:
#'     code_folding: hide
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     theme: sandstone
#'     highlight: zenburn
#' ---
#'
#+ preamble, include = F
knitr::opts_chunk$set(message = F, warning = F)
pacman::p_load(char=c('tidyverse','janitor','broom', 'knitr','kableExtra', 'vroom', 'here','pacman'))
abhiR::reload()
#'
#' <hr/>
#' # Summary
#'
#' We have a paucity of data that can help us define **nephrotic syndrome** among the study
#' subjects. The availability of urine protein:creatinine ratio is limited, and it appears that
#' serum albumin is not measured on these subject.
#'
#' How can we proceed? Does this mean reaching out to the Boston folks, or are we just stuck.
#' Please double check my variable extractions to see that I'm not missing obvious variables that
#' are available in the dataset
#'
#' <hr/>
#'
#' # Definition
#'
#' The definition of nephrotic syndrome is (Kathleen, 2/19/2020):
#'
#' 1.  The presence of nephrotic range proteinuria, which is a urine
#' protein:creatinine ratio \>200 mg/mmol or if there is a 24 hour
#' urine instead of a urine protein:creatinine ratio, it would be a 24
#' hour protein excretion greater than protein \>50 mg/kg (\>40 mg/m^2^
#'                                                            per hour) in 24 hours.
#' 2.  Hypoalbuminemia (an albumin less than 2.5 g/dL)
#'
#' # Availability of data
#'
#' ## Urinanalysis
#'
tribble(~Variable, ~Description, ~Form, ~Table, ~Type,
        "SPOTURIN", "Urine protein to creatinine ratio (UPC) in last 30 days","Visit", "URINE","Category",
        "URINRATO", "Urine Protein to creatinine ratio", "Visit", "URINE", "Numeric",
        "PROT30", "Proteinuria in last 30 days", "Visit", "URINE", 'Category',
        'DIPSTICK','Dipstick: Protein','Visit','URINE', 'Category') %>%
  kable(caption = 'Available relevant variables from urine analysis') %>%
  kable_styling(bootstrap_options = 'striped')
#'
#' There does not appear to be any raw protein or creatinine numbers from the urinanalysis.
#' The protenuria and protein measurements are categorical.
#'
#' There appears to be rather low prevalence of these measures among the subjects:

all_subjects <- vroom(here('data/raw/all_rows_data_2020-01-31_1545.csv'),
                      col_select = c(subjectId, visit = folderName)) %>%
  distinct() %>%
  mutate(folderName = fct_relevel(visit, 'Baseline','6 month','12 month','18 month','24 month')) %>%
  select(-folderName)

urine <- vroom(here('data/raw/urine_data_2020-01-31_1545.csv'))
urine <- urine %>%
  select(subjectId, visit = folderName, DIPSTICK, PROT30, URINRATO, RATIOUNT, SPOTURIN)

out <- all_subjects %>% left_join(urine)

out %>% filter(visit %in% c('Baseline','6 month','12 month','18 month','24 month')) %>%
  mutate(visit = fct_relevel(visit, 'Baseline',paste(c(6,12,18,24), 'month'))) %>%
  group_by(visit) %>%
  summarise(N = n(), perc_missing_UPC = 100*mean(is.na(URINRATO))) %>%
  kable(col.names = c('Visit','N','Percent missing UPC'), digits=2) %>%
  kable_styling(bootstrap_options = 'striped', full_width = F)

#' The UPC is also categorized as < 0.5 mg/mg and >= 0.5 mg/mg.

out %>%
  filter(visit %in% c('Baseline',paste(c(6,12,18,24),'month'))) %>%
  mutate(visit = fct_relevel(visit, 'Baseline','6 month')) %>%
  tabyl(SPOTURIN, visit) %>%
  adorn_percentages('col') %>%
  adorn_totals('row') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title('combined') %>%
  kable(caption = 'Frequency of SPOTURIN categories by visit') %>%
  kable_styling() %>%
row_spec(row=5, bold = TRUE)

#' The data on proteinuria is even sparser.
out %>%
  filter(visit %in% c('Baseline',paste(c(6,12,18,24),'month'))) %>%
  mutate(visit = fct_relevel(visit, 'Baseline','6 month')) %>%
  tabyl(PROT30, visit) %>%
  adorn_percentages('col') %>%
  adorn_totals('row') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title('combined') %>%
  kable(caption = 'Frequency of proteinuria categories by visit') %>%
  kable_styling() %>%
  row_spec(row = 5, bold = TRUE)

#' ## Blood analysis
#'
#' It does not appear that serum albumin was tested for these subjects. The available
#' assays from blood samples, as far as I can tell,  are given below.
data_dict <- readxl::read_excel(here('background/f-6-337-13136911_ZH2vHUc6_111319_CARRA_Registry_11.0_DataDictionary_DCRI_SxxX.xlsx')) %>%
  clean_names()

data_dict %>%
  filter(table_name=='LABS',data_type=='Text', !str_detect(variable_description, 'Upper')) %>%
  select(starts_with('variable')) %>%
  kable(format='html', col.names = snakecase::to_sentence_case(names(.))) %>%
  kable_styling(bootstrap_options = 'striped')

#' # Session information
#'
#' This analysis was done using `r R.version$version.string` and the following packages
#+ packages, results = 'asis', echo = FALSE
pkgs <- p_loaded() %>% sort()
d <- tibble(Package = pkgs) %>%
  mutate(Version = map(Package, p_ver) %>% map_chr(as.character))
bl <- glue::glue_data(d, '{Package} ({Version})') %>% paste(collapse = '; ')
cat(bl)
