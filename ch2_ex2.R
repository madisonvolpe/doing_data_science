library(tidyverse)
library(readxl)
library(here)
library(lubridate)
library(janitor)

mhn_dat <- readxl::read_xls(
  here("dds_datasets", "dds_ch2_rollingsales", "rollingsales_manhattan.xls"),
  skip = 4, col_names = T) %>%
  clean_names()


# structure of dataset
str(mhn_dat)

# count of NA for each dataset 
na_cnt_cols <- map_df(mhn_dat, ~sum(is.na(.x)))

mhn_dat <- mhn_dat %>%
  mutate(
    tax_class_at_present = factor(tax_class_at_present),
    building_class_at_present = factor(building_class_at_present)
    )

fun_list <- list(
  min = ~min(.x),
  max = ~max(.x),
  mean = ~mean(.x),
  median = ~median(.x),
  sd = ~sd(.x)
)

mhn_dat_stats <- mhn_dat %>%
  summarise(across(12:16, fun_list)) %>%
  pivot_longer(cols = everything())
