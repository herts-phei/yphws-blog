suppress_everything <- function(df) {
  
  df %>% mutate(across(where(is.numeric), ~ if_else(.x <= 7 & .x != 0, NA, .x) %>% `/`(5) %>% round() %>% `*`(5)))
  
}