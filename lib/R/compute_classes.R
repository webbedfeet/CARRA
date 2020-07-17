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
