library(tidyverse)
library(data.table)
library(feather)

file <- "'/run/media/aljrico/TOSHIBA EXT/ds/all/test_set.csv'"
num.rows <- as.numeric(gsub("[^0-9.]", "",  system(paste("wc -l", file), intern = T)))

from <- 1
to <- num.rows/20
paste0("cat ", file, " | awk 'NR >= ", from, "  && NR < ", to," { print }' > data/small_test.csv") %>% system()
