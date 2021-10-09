library(tidyverse)
library(sf)

parse_geo_string <- function(str) {
  # parse the geology histogram
  # throw out beginning/ending brackets
  str <- substr(str, 2, str_length(str)-1)
  # split by comma
  str_vec <- str_split(str, ",")[[1]]
  ret_list <- list()
  for (item in str_vec) {
    # remove whitespace
    item <- str_trim(item)
    # find the equals sign
    eq_idx <- str_locate(item, "=")[1, 1]
    # pick out key/val
    key <- substr(item, 1, eq_idx-1)
    val <- as.numeric(substr(item, eq_idx+1, str_length(item)))
    # insert in list
    ret_list[[key]] <- val
  }
  return(ret_list)
}

sgmc_primary   <- read_csv("data_in/gee/sgmc_primary.csv", 
                           col_types=cols(NLA12_ID=col_character())) %>%
  select(-NLA12_ID, -SITEID, -.geo) %>%
  mutate(histogram_parsed = map(histogram, parse_geo_string)) %>%
  unnest(histogram_parsed) %>%
  mutate(value = as.numeric(flatten_chr(histogram_parsed)),
         key   = as.numeric(names(histogram_parsed))) %>%
  select(-histogram_parsed) %>%
  mutate(key = str_c("lith_type", key)) %>%
  spread(key, value, fill=0) %>%
  # remove a zero column
  select(-`<NA>`) %>%
  # normalize within each row
  mutate(row_sum = rowSums(select(., contains("lith_type"))),
         across(.cols=contains("lith_type"), ~ .x / row_sum)) %>%
  select(-row_sum, -histogram, -`system:index`) %>%
  # drop the NA lith column
  select(-lith_type3)

sgmc_secondary <- read_csv("data_in/gee/sgmc_secondary.csv",
                           col_types=cols(NLA12_ID=col_character())) %>%
  select(-NLA12_ID, -SITEID, -.geo) %>%
  mutate(histogram_parsed = map(histogram, parse_geo_string)) %>%
  unnest(histogram_parsed) %>%
  mutate(value = as.numeric(flatten_chr(histogram_parsed)),
         key   = as.integer(names(histogram_parsed))) %>%
  select(-histogram_parsed) %>%
  mutate(key = str_c("lith_type", key)) %>%
  spread(key, value, fill=0) %>%
  # remove a zero column
  select(-`<NA>`) %>%
  # normalize within each row
  mutate(row_sum = rowSums(select(., contains("lith_type"))),
         across(.cols=contains("lith_type"), ~ .x / row_sum)) %>%
  select(-row_sum, -histogram, -`system:index`) %>%
  # drop the NA lith column
  select(-lith_type6)

write_csv(sgmc_primary,   "data_working/sgmc_primary.csv")
write_csv(sgmc_secondary, "data_working/sgmc_secondary.csv")

  
