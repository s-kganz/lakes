# This file is exclusively for helper functions.

library(tidyverse)

parse_huc <- function(s) {
  # convert a possibly mangled HUC string to its correct 12-char string
  if (grepl("+", s, fixed=T)) {
    # it's a malformed float - the leading 4 digits *should* have a valid HUC04
    pow <- str_sub(s, start=str_length(s)-1) %>% as.integer()
    (as.double(s) / (10^pow)) %>% as.character() %>% str_remove(fixed(".")) %>%
      str_pad(12, "right", "0")
  } else {
    # it's a string so just pad it
    return(str_pad(s, 12, side="left", pad="0"))
  }
}

dynamic_join <- function(x, y, xjoin="lake_huc04", yjoin="huc4") {
  # Join two tables together using strings as the join column
  # x: df
  # y: path
  inner_join(
    x,
    read_csv(y) %>%
      rename_at(vars(-!!yjoin), ~paste0("huc4_", .x)),
    by=setNames(yjoin, xjoin)
  )
}

skew <- function(x) {
  #https://en.wikipedia.org/wiki/Skewness#Sample_skewness
  mu <- mean(x)
  n  <- length(x)
  return(
    (1/n * sum((x - mu)^3)) / ((1/(n-1) * sum((x - mu)^2))^1.5)
  )
}

safe_log_transform <- function(x) {
  # shift a vector such that all values are positive, then take the log
  eps <- 1 # minimum output is 0
  x <- x - min(x) + eps
  log(x)
}
