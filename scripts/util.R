# This file is exclusively for functions.

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