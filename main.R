library(tercen)
library(dplyr)

do.lm <- function(df) {
  mod <- lm(.y ~ .x, data = df)
  out <- data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1],
    intercept = mod$coefficients[1],
    slope = mod$coefficients[2],
    fit.y = range(mod$fitted.values), # Get min and max predictions
    fit.x = range(df$.x) # Get min and max x
  )
  return(out)
}

(ctx <- tercenCtx())  %>% 
  select(.x, .y, .ri, .ci) %>% 
  group_by(.ri, .ci) %>%
  do(do.lm(.)) %>%
  ctx$addNamespace() %>%
  ctx$save()
