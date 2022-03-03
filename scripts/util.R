# This file is exclusively for helper functions.

library(tidyverse)
library(colorscience)

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

fui.hue <- function(R, G, B) {
  
  # Convert R,G, and B spectral reflectance to dominant wavelength based
  # on CIE chromaticity color space
  
  # see Wang et al 2015. MODIS-Based Radiometric Color Extraction and
  # Classification of Inland Water With the Forel-Ule
  # Scale: A Case Study of Lake Taihu
  
  require(colorscience)
  # chromaticity.diagram.color.fill()
  Xi <- 2.7689*R + 1.7517*G + 1.1302*B
  Yi <- 1.0000*R + 4.5907*G + 0.0601*B
  Zi <- 0.0565*G + 5.5943*B
  
  # calculate coordinates on chromaticity diagram
  x <-  Xi / (Xi + Yi +  Zi)
  y <-  Yi / (Xi + Yi +  Zi)
  z <-  Zi / (Xi + Yi +  Zi)
  
  # calculate hue angle
  alpha <- atan2( (x - 0.33), (y - 0.33)) * 180/pi
  
  # make look up table for hue angle to wavelength conversion
  cie <- cccie31 %>%
    mutate(a = atan2( (x - 0.33), (y - 0.33)) * 180/pi) %>%
    dplyr::filter(wlnm <= 700) %>%
    dplyr::filter(wlnm >=380)
  
  # find nearest dominant wavelength to hue angle
  wl <- cie[as.vector(sapply(alpha,function(x) which.min(abs(x -
                                                               cie$a)))), 'wlnm']
  
  #out <- cbind(as.data.frame(alpha), as.data.frame(wl))
  
  return(wl)
}
