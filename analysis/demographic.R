# Extract demographic data
library(pacman)
p_load(char = c('tidyverse','broom','vroom','here', 'janitor'))

demographic <- vroom(here('data/raw/dem_data_2020-01-31_1545.csv')) %>%
  clean_names() %>%
  select(subject_id, rheumage, sex, dxdage,
         white, black, asian, amerind, hispanic,
         mideast, noanswer, nathwn, othrace, residenc)
saveRDS(demographic, here('data/rda/demographic.rds'), compress=T)
