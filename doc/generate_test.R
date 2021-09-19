options(digits = 15)
df_out <- ctx %>% 
  select(.x, .y, .ri, .ci) %>% 
  group_by(.ri, .ci) %>%
  do(do.lm(., intercept.omit)) %>%
  ctx$addNamespace()
df_col <- ctx$cselect() %>%
  mutate(.ci = 1:nrow(.) - 1)
df_row <- ctx$rselect() %>%
  mutate(.ri = 1:nrow(.) - 1)
df_out <- df_out %>% 
  left_join(df_col) %>%
  left_join(df_row) %>% 
  ungroup() %>% 
  select(-.ci, -.ri)
write.csv(df_out, file = "./test/output.csv", quote = FALSE, row.names = FALSE)
