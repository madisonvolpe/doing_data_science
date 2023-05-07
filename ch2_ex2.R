library(tidyverse)
library(readxl)
library(here)
library(lubridate)
library(janitor)
library(gtsummary)

mhn_dat <- readxl::read_xls(
  here("dds_datasets", "dds_ch2_rollingsales", "rollingsales_manhattan.xls"),
  skip = 4, col_names = T) %>%
  clean_names()


# structure of dataset
str(mhn_dat)

# count of NA for each column in dataset 
na_cnt_cols <- map_df(mhn_dat, ~sum(is.na(.x)))

#count of 0 for each column in dataset 
  # n = 7593 observations have sale_price = 0, sale_date is not NA 
zero_cnt_cols <- map_df(mhn_dat, ~sum(ifelse(.x == 0, 1,0), na.rm = T))


# further exploring data (min, max, mean, median, sd) of continuous variables
fun_list <- list(
  min = ~min(.x),
  max = ~max(.x),
  mean = ~mean(.x),
  median = ~median(.x),
  sd = ~sd(.x)
)

mhn_dat_stats <- mhn_dat %>%
  summarise(across(c(12:16,20), fun_list)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(value = round(value, 2))

# explore data in different way using gt summary
mhn_dat %>%
  select(c(12:16, 20)) %>%
  tbl_summary(
    statistic = all_continuous() ~ '{mean},[{median}],({sd}), ({min} - {max})'
  )

# finding outliers in dataset 

  # histograms of all continuous variables

  map2(.x = select(mhn_dat, c(12:16, 20)), .y = names(select(mhn_dat, c(12:16,20))),
     ~ggplot(mhn_dat, aes(x = .x)) +
       geom_histogram() +
       ggtitle(paste("Histogram of ", .y)))
  
  # histograms of all continuous variables with log transformation
    # note some cts variables have 0 values so removing 0 values
  map2(.x = select(mhn_dat, c(12:16, 20)) %>% 
         filter(if_all(everything(), ~. > 0)) %>%
         mutate(across(everything(), log)), 
       .y = names(select(mhn_dat, c(12:16,20))),
       ~ggplot(
         mhn_dat %>% select(c(12:16, 20)) %>% filter(if_all(everything(), ~. >0))
         , aes(x = .x)) + 
                 geom_histogram() +
                 ggtitle(paste("Histogram of log ", .y)))

  # boxplots for all continuous variables 

  #boxplots with outliers
  mhn_dat %>%
    select(c(12:16, 20)) %>%
    imap(., ~ggplot(mhn_dat, aes(y = .x)) +
         geom_boxplot() +
         ggtitle(paste("Boxplot of ", .y)))
  
  #boxplots without outliers
  mhn_dat %>%
    select(c(12:16, 20)) %>%
    imap(., ~ggplot(mhn_dat, aes(y = .x))+
           geom_boxplot(outlier.shape = NA) + 
           coord_cartesian(ylim = quantile(.x, c(0.1, 0.9))) +
           ggtitle(paste("Boxplot of", .y, "(Without Outliers)")))
  
  # most likely variables like reisidential units, commerical units, total units,
  # land_square_feet and gross_square_feet have many outliers and a weird distribution
  # given the different types of housing in nyc
  
  # boxplots for sale_price by housing type 
  
  map(unique(mhn_dat$building_class_category), 
      ~select(filter(mhn_dat, building_class_category == .x), c(12:16, 20))) %>%
    set_names(unique(mhn_dat$building_class_category)) %>%
    keep(., ~nrow(.x) >= 30) %>% # only keep housing types with at least 30 observations
    imap(.,
        ~select(.x, "sale_price") %>%
          filter(sale_price > 0) %>%
          mutate(sale_price = log(sale_price)) %>%
          ggplot(., aes(y = sale_price)) + 
          geom_boxplot() +
          ggtitle(.y)
        )
  

# Final clean dataset (factor variables, assign Date)
mhn_dat_clean <- mhn_dat %>%
  mutate(
    tax_class_at_present = factor(tax_class_at_present),
    building_class_at_present = factor(building_class_at_present),
    sale_date = ymd(sale_date),
    log_sale_price = log(sale_price),
    neighborhood_group = 
      case_when(
        str_detect(neighborhood,"^UPPER EAST SIDE") ~ "UPPER EAST SIDE",
        str_detect(neighborhood, "^UPPER WEST SIDE") ~ "UPPER WEST SIDE",
        str_detect(neighborhood, "^HARLEM") ~ "HARLEM",
        str_detect(neighborhood, "^MIDTOWN") ~ "MIDTOWN",
        str_detect(neighborhood, "^WASHINGTON") ~ "WASHINGTON HEIGHTS",
        TRUE ~ neighborhood),
    neighborhood_condensed = 
      case_when(
        neighborhood %in% c(
          'MORNINGSIDE HEIGHTS', 'HARLEM-CENTRAL', 'HARLEM-EAST', 'HARLEM-UPPER',
          'HARLEM-WEST', 'INWOOD', 'MANHATTAN VALLEY', 'UPPER EAST SIDE (59-79)',
          'UPPER EAST SIDE (79-96)', 'UPPER EAST SIDE (96-110)',
          'UPPER WEST SIDE (59-79)', 'UPPER WEST SIDE (79-96)',
          'UPPER WEST SIDE (96-116)', 'WASHINGTON HEIGHTS LOWER',
          'WASHINGTON HEIGHTS UPPER'
        ) ~ 'Upper Manhattan',
        neighborhood %in% c(
          'ALPHABET CITY', 'CHINATOWN', 'CIVIC CENTER', 'EAST VILLAGE',
          'FINANCIAL', 'GREENWICH VILLAGE-CENTRAL', 'GREENWICH VILLAGE-WEST',
          'LITTLE ITALY', 'LOWER EAST SIDE', 'SOHO', 'SOUTHBRIDGE',
          'TRIBECA'
        ) ~ 'Lower Manhattan',
        neighborhood %in% c(
          'CLINTON', 'FASHION','JAVITS CENTER', 'MIDTOWN CBD', 
          'MIDTOWN EAST', 'MIDTOWN WEST', 'MURRAY HILL'
        ) ~ 'Midtown',
        neighborhood %in% c(
          'CHELSEA', 'FLATIRON', 'GRAMERCY', 'KIPS BAY'
        ) ~ 'Between Midtown and Lower Manhattan',
        TRUE ~ neighborhood
      )) %>%
  filter(sale_price > 0)

mhn_dat_clean %>%
  ggplot(aes(x = sale_date, y = sale_price, col = neighborhood)) +
    geom_point() 

mhn_dat_clean %>%
  ggplot(aes(x = sale_date, y = sale_price, col = neighborhood_condensed)) +
  geom_point()

mhn_dat_clean %>%
  ggplot(aes(x = sale_date, y = sale_price)) +
  geom_point()+
  facet_grid(~neighborhood_condensed)

mhn_dat_clean %>%
  ggplot(aes(x = sale_date, y = log_sale_price, col = building_class_category)) +
  geom_point() +
  facet_wrap(~neighborhood_condensed, nrow = 2) +
  theme(legend.position = "none")


