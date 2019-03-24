# generate data frame for iteration analysis
create_iteration_data <- function(df, data_frame = TRUE) {
  df <- df %>%
    lower_tri_dist_calc(data_frame = data_frame) %>%
    rownames_to_column("id") %>%
    gather(isolate1, isolate2, -id) %>%
    mutate(group1 = substr(isolate1, start = 1, stop = 2),
           group2 = substr(isolate2, start = 1, stop = 2)) %>%
    select(isolate1, group1, isolate2, group2, value) %>%
    filter(is.na(value) == FALSE,
           group1 == group2) %>%
    arrange(group1, group2)
  
  return(df)
}

