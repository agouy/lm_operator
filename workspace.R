library(tercen)
library(dplyr)

options("tercen.workflowId" = "3f041aa685560878aea9b2387b00358e")
options("tercen.stepId"     = "d6042405-0bd9-482e-9417-d05419d08eaa")

getOption("tercen.workflowId")
getOption("tercen.stepId")

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
