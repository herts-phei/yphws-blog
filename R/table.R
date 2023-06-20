chi_sq_tables <- function(column) {
  
tables <- list()

tables[["education"]] <- yphws_data_private %>% select(column, prospects_education) %>% filter(column != "Prefer not to say") %>% table()
tables[["job"]] <- yphws_data_private %>% select(column, prospects_job) %>% filter(column != "Prefer not to say") %>% table()
tables[["training"]] <- yphws_data_private %>% select(column, prospects_training) %>% filter(column != "Prefer not to say") %>% table()
tables[["family"]] <- yphws_data_private %>% select(column, prospects_other) %>% filter(column != "Prefer not to say") %>% table()
tables[["unsure"]] <- yphws_data_private %>% select(column, prospects_notsure) %>% filter(column != "Prefer not to say") %>% table()
tables[["other"]] <- yphws_data_private %>% select(column, prospects_other) %>% filter(column != "Prefer not to say") %>% table()

return(tables)

}

p_value_table <- function(question) {
  
  tibble(
    "p-value" = signif(chisq_results[[question]]$p.value, 1),
    "Significance" = if (`p-value` < 0.05 &
                         `p-value` > 0.01)
      "statistically significant"
    else if (`p-value` < 0.01)
      "very statistically significant"
    else if (`p-value` > 0.05)
      "not statistically significant"
  ) %>% select(-"p-value")
  
}