library(tidyverse)
library(urltools)
library(jsonlite)
library(janitor)
library(zoo)
# Bathybase comes as a directory tree containing an info.json file and a TSV of
# hypsometry. We would like to convert the info.json to a row of a dataframe
# and add the path to the TSV as another column.

root_dir <- "data_in/BathybaseDb"
df <- NULL
for (dir in list.dirs(root_dir)) {
  info_path <- list.files(dir, pattern="*.json", recursive=F,
                          full.names=T)[1]
  tsv_path  <- list.files(dir, pattern="*.tsv", recursive=F,
                          full.names=T)[1]
  if (is.na(info_path) | is.na(tsv_path)) {
    next
  }
  if (is.null(df)) {
    df <- fromJSON(info_path) %>% as.data.frame()
    df$tsv_path <- c(tsv_path)
  } else {
    next_row <- fromJSON(info_path) %>% as.data.frame()
    next_row$tsv_path <- tsv_path
    df <- bind_rows(df, next_row)
  }
}

# calculate the hypsometric integral for a given
# area-depth TSV
calculate_hi <- function(file) {
  # suppress output when reading the files
  hypsometry <- read_tsv(file, col_types=cols()) %>% clean_names()
  reldepth <- hypsometry$depth_m / max(hypsometry$depth_m)
  relarea  <- hypsometry$area_m_2 / max(hypsometry$area_m_2)
  id <- order(reldepth)
  AUC <- sum(diff(reldepth[id])*rollmean(relarea[id],2))
  return(AUC)
}
calculate_hi <- Vectorize(calculate_hi)

df <- df %>%
  mutate(domain=domain(Source),
         hi=calculate_hi(tsv_path),
         shape=ifelse(hi < 0.45, "concave", ifelse(hi < 0.55, "linear", "convex")))

write_csv(df, "data_working/bathybase.csv")
