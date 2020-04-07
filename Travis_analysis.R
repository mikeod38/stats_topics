library(tidyverse)
library(magrittr)
library(readr)
library(purrr)

rawdata_loader <- function(...) {
# at prompt, I chose a file in your 'Quarantine_analysis...' folder
folder <- file.choose() %>% dirname()
filenames <- list.files(folder, pattern = "*rawdata.csv", recursive = TRUE)
files <- file.path(folder,filenames)
  
alldata <- files %>%
  purrr::map_df(~ readr::read_csv(.))
}

alldata <- rawdata_loader()

date_getter <-function(column) {
  stringr::str_extract( {{ column }}, "[0-9]{1,2}-[0-9]{1,2}-[0-9]{2}")
}

# some dates were dd-mm-yy and some were yyyymmdd
alldata %<>%
  mutate(date = date_getter(animal)) %>%
  #mutate(date = fct_explicit_na(date)) %>%
  mutate(date = case_when(
    is.na(date) ~ stringr::str_extract(animal, "[0-9]{8}"),
    TRUE ~ date)) %>%
  separate(genotype, into = c("genotype", "condition", "cue"), sep = "_")

ggplot(alldata, aes(x = time, y = delF)) +
  geom_line(aes(colour = date, group = animal), alpha = 0.25) +
  stat_summary(geom = "smooth", fun.y = "mean", aes(colour = date, group = date)) +
  facet_grid(condition ~ genotype)

  




