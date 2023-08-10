suppress_everything <- function(df) {
  
  df %>% mutate(across(where(is.numeric), ~ if_else(.x <= 7 & .x != 0, NA, .x) %>% `/`(5) %>% round() %>% `*`(5)))
  
}

yphws_blog_plot_stackedbar <- function(df) {
  
  df <- df %>% pivot_longer(
    cols = c("Maybe", "No", "Yes"),
    names_to = "answer",
    values_to = "number"
  )
  
  df %>% 
    ggplot2::ggplot(aes_string(names(df)[[1]], "number", fill = "answer", group = "answer")) + 
    geom_col(position = "fill", alpha = 0.7) + 
    geom_text(aes(label = number), position = position_fill(vjust = 0.5)) +
    coord_flip() + 
    labs(y = "Percentage") + 
    ggpubr::theme_pubclean() +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    ggplot2::scale_fill_manual(values = c("#3969B5", "#46C3AE", "#99D5F3")) +
    ggplot2::guides(fill = guide_legend(reverse=TRUE)) +
    ggplot2::theme(text = element_text(size = 12),
                   legend.key = element_rect(fill = "transparent", colour = "transparent"),
                   legend.position = "bottom", axis.title.y = element_blank())
  
}
