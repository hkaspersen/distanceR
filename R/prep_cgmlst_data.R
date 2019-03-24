prep_cgmlst_data <- function(df, metadata) {
  df <- df %>%
    left_join(metadata, by = "id") %>%
    group_by(group) %>%
    mutate(n = 1:n(),
           new_id = paste(group, n, sep = "_")) %>%
    ungroup() %>%
    select(new_id, everything(), -c(group, id)) %>%
    mutate_all(funs(as.factor)) %>%
    column_to_rownames("new_id")
  
  return(df)
}
