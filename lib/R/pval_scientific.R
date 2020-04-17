pval_scientific <- function(x, dig = 2) {
  require(glue)
  f1 <- format(x, scientific = T, digits = dig) %>%
    str_split('e') %>%
    unlist() %>%
    as.numeric()
  glue('${f1[1]} \\times 10^{{{f1[2]}}}$')
}
