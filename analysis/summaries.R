library(readxl)
library(tidyverse)
library(glue)
library(fs)

# Documenting version of data we're using ---------------------------------
data_date <- tibble(fname = dir_ls('data/raw', glob = '*.zip')) %>%
  separate(fname, c('p1','p2','dt','tm'), sep = '_', remove = T) %>%
  select(-p1, -p2) %>%
  mutate(tm = str_remove(tm, '.zip'),
         tm = as.numeric(tm)) %>% #,
         # tm = substr(as.POSIXct(sprintf('%4.0f', tm), format = "%H%M"), 12,16)) %>%
  arrange(desc(dt), desc(tm)) %>%
  slice(1)
glue_data(data_date, "The version of data we're using is from {dt} {substr(as.POSIXct(as.character(tm), format = '%H%M'), 12,16)}")


# reading data dictionary -------------------------------------------------

data_dict <- read_excel('background/f-6-337-13136911_ZH2vHUc6_111319_CARRA_Registry_11.0_DataDictionary_DCRI_SxxX.xlsx')
names(data_dict) = make.names(names(data_dict))

write.table(data_dict, sep = '\t', file = 'background/f-6-337-13136911_ZH2vHUc6_111319_CARRA_Registry_11.0_DataDictionary_DCRI_SxxX.tsv')



#' Principle 1 : 20% to 75% of children with SLE will develop nephritis
#' From the data dictionary, this question is answered in SLICC
#' Re-define Yes as
#' Either/or WHO2-6, ISNRPS2-6 = LN


glue_data(data_dict %>% filter(Variable.Name=='SLICC00'),
          'Variable {Variable.Name}: {Variable.Description}')

all_rows <- read_csv('data/raw/all_rows_data_2020-01-31_1545.csv')
all_rows %>% filter(varName=='SLICC00') %>%
  count(conceptValue) %>%
  mutate(perc = n/sum(n)*100)

#' Principle 2: 82% of LN in cSLE develops wihin the first year of diagnosis and 92% within 2 years
#'

#' Principle 3: Membranous (class V) LN more often presents with nephrotic
#' syndrome than proliferative LN (class III or IV)
#'
#' Biopsy form, WHO1-6, ISNRPS1-6
#' Either/or WHO2-6, ISNRPS2-6 = LN



#' Principle 4: Short term renal outcomes are worse in blacks of African American heritage

#' Principle 5: Short term renal outcomes are worse in patients who present with GFR < 60mL/min/1.73 m2
#' and/or nephrotic-range proteinuria (> 1 protein/creatinine ratio)

#' Principle 6: Rituximab has been used as a steroid-sparing agent for induction
#' in proliferative LN (LN vs no-LN, 3-4 vs 5)

#' Is there differences in age/gender for people getting Rtx.

