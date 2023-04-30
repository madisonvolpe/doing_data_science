library(tidyverse)
library(lubridate)
library(here)

# read data (for one dataset)
df_nyt <- read_csv(here("dds_datasets", "dds_ch2_nyt", "nyt1.csv"))

# create variable for age 
df_nyt <- df_nyt %>%
  mutate(
    age_group = 
      case_when(
        Age < 18 ~ "<18",
        Age >= 18 & Age <= 24 ~ "18-24",
        Age >= 25 & Age <= 34 ~ "25-34",
        Age >= 35 & Age <= 44 ~ "35-44",
        Age >= 45 & Age <= 54 ~ "45-54",
        Age >= 55 & Age <= 64 ~ "55-64",
        Age >= 65 ~ "65+"
      )
  ) %>%
  mutate(Signed_In = factor(Signed_In, levels = c(0,1), labels = c("Not Signed In", "Signed In")))

# If user is not signed in then do not get Age & Gender information for them. When doing
# age/gender analyses only do it on signed in users. 
df_nyt %>%
  select(Age, Gender, Signed_In) %>%
  group_by(Signed_In) %>%
  group_map(~unique(.x), .keep = T)

# single day (plot number of impressions vs click-through-rate for each age_group)
df_nyt %>%
  filter(Signed_In == 'Signed In') %>% 
  group_by(age_group) %>%
  summarise(
    overall_impressions = sum(Impressions),
    ctr = sum(Clicks)/sum(Impressions)) %>%
  ungroup() %>%
  mutate(ctr = round(ctr, digits = 5)) %>%
  ggplot(aes(x = overall_impressions, y = ctr, fill = age_group, label = ctr)) +
    geom_bar(stat = "identity") + 
    geom_text()

# single day plot signed in versus not signed in 
df_nyt %>%
  group_by(Signed_In) %>%
  summarise(ctr = sum(Clicks)/sum(Impressions)) %>%
  ungroup() %>%
  mutate(ctr = round(ctr, digits = 5)) %>%
  ggplot(aes(x = Signed_In, y = ctr, fill = Signed_In, label = ctr))+
    geom_bar(stat = 'identity') +
    geom_text()



