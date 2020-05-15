library(tercen)
library(dplyr)

do.lm <- function(df) {
  
  out <- data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1],
    intercept = NaN,
    slope = NaN,
    fit.y = rep(NaN, 2),
    fit.x = rep(NaN, 2)
  )
  
  mod <- try(lm(.y ~ .x, data = df))
  
  if(!inherits(mod, 'try-error')) {
    out$intercept <- mod$coefficients[1]
    out$slope <- mod$coefficients[2]
    out$fit.y <- range(mod$fitted.values)
    out$fit.x <- range(df$.x)
  }
  return(out)
  
}

(ctx <- tercenCtx())  %>% 
  select(.x, .y, .ri, .ci) %>% 
  group_by(.ri, .ci) %>%
  do(do.lm(.)) %>%
  ctx$addNamespace() %>%
  ctx$save()