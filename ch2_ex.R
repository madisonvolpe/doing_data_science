library(tidyverse)
library(here)

# read data (only one dataframe)
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
  mutate(Signed_In = factor(Signed_In, levels = c(0,1), labels = c("Not Signed In", "Signed In")),
         Gender = factor(Gender, levels = c(0,1), labels = c("Female", "Male")))

# If user is not signed in then do not get Age & Gender information for them. When doing
# age/gender analyses only do it on signed in users. 
df_nyt %>%
  select(Age, Gender, Signed_In) %>%
  group_by(Signed_In) %>%
  summarise(across(everything(), n_distinct))

# single day CTR plot by age group
df_nyt %>%
  filter(Signed_In == 'Signed In') %>% 
  group_by(age_group) %>%
  summarise(
    overall_impressions = sum(Impressions),
    ctr = sum(Clicks)/sum(Impressions)) %>%
  ungroup() %>%
  mutate(ctr = round(ctr, digits = 5)) %>%
  ggplot(aes(x = age_group, y = ctr, fill = age_group, label = ctr)) +
    geom_bar(stat = "identity") + 
    geom_text() + 
    ggtitle("CTR by Age Group")

# single day CTR plot by gender
df_nyt %>%
  filter(Signed_In == 'Signed In') %>%
  group_by(Gender) %>%
  summarise(ctr = sum(Clicks)/sum(Impressions)) %>%
  ungroup() %>%
  mutate(ctr = round(ctr, digits = 5)) %>%
  ggplot(aes(x = Gender, y = ctr, fill = Gender, label = ctr)) +
    geom_bar(stat = 'identity') +
    geom_text() +
    ggtitle("CTR by Gender")

# single day CTR plot by Signed In
df_nyt %>%
  group_by(Signed_In) %>%
  summarise(ctr = sum(Clicks)/sum(Impressions)) %>%
  ungroup() %>%
  mutate(ctr = round(ctr, digits = 5)) %>%
  ggplot(aes(x = Signed_In, y = ctr, fill = Signed_In, label = ctr))+
    geom_bar(stat = 'identity') +
    geom_text()+
    ggtitle("CTR by Signed In")

# Create overall summary dataset

stat_summaries <- list(
  
  sum = ~sum(.x),
  max = ~max(.x),
  min = ~min(.x),
  mean = ~mean(.x)
  
  )

overview_stats_df <- list("Gender", "age_group") %>%
  map(., 
      ~df_nyt %>% 
        filter(Signed_In == 'Signed In') %>%
        group_by(across(.x)) %>%
        summarise(across(c(Impressions, Clicks), stat_summaries, .names = "{.fn}_{.col}")) %>%
        rename(grouping_by = 1) %>%
        mutate(
          ctr = sum_Clicks/sum_Impressions,
          group_var = .x) %>%
        select(11, 1:10)
        ) %>%
  bind_rows() %>%
  bind_rows(
    df_nyt %>%
    summarise(across(c(Impressions, Clicks),stat_summaries, .names = "{.fn}_{.col}")) %>%
    mutate(
      ctr = sum_Clicks/sum_Impressions,
      grouping_by = 'Overall',
      group_var = 'Overall') %>%
      select(11, 1:10)
    )


# Iterate through and create stat_summaries for each nyt file 
i_files <- list.files(here("dds_datasets", "dds_ch2_nyt"))
final_stats <- list()

for (i in 1:length(i_files)){
  
  file_nm <- i_files[i]
  nyt_data <- read_csv(here("dds_datasets", "dds_ch2_nyt", i_files[i]))
  
  nyt_data <- nyt_data %>%
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
    mutate(Signed_In = factor(Signed_In, levels = c(0,1), labels = c("Not Signed In", "Signed In")),
           Gender = factor(Gender, levels = c(0,1), labels = c("Female", "Male")))
  
  final_stats[[i]] <- 
    list("Gender", "age_group") %>%
    map(., 
        ~nyt_data %>% 
          filter(Signed_In == 'Signed In') %>%
          group_by(across(.x)) %>%
          summarise(across(c(Impressions, Clicks), stat_summaries, .names = "{.fn}_{.col}")) %>%
          rename(grouping_by = 1) %>%
          mutate(
            ctr = sum_Clicks/sum_Impressions,
            group_var = .x) %>%
          select(11, 1:10)
    ) %>%
    bind_rows() %>%
    bind_rows(
      nyt_data %>%
        summarise(across(c(Impressions, Clicks),stat_summaries, .names = "{.fn}_{.col}")) %>%
        mutate(
          ctr = sum_Clicks/sum_Impressions,
          grouping_by = 'Overall',
          group_var = 'Overall') %>%
        select(11, 1:10)) %>%
    mutate(nyt_filename = file_nm)
  
}

# bind datasets together save final dataset of all files to csv
final_stats_df <- bind_rows(final_stats)
final_stats_df <- final_stats_df %>%
  arrange(str_rank(nyt_filename, numeric = T)) %>% 
  mutate(nyt_file_order = as.numeric(str_extract(nyt_filename, "\\d+")))
write_csv(final_stats_df, here("dds_datasets", "dds_ch2_nyt", "all_nyt_ds_stats.csv"))

# Overall CTR over time 

final_stats_df %>%
  filter(group_var == 'Overall') %>%
  mutate(ctr = round(ctr, digits = 3)) %>%
  ggplot(aes(x = nyt_file_order, y = ctr)) +
    geom_line(stat = 'identity')

# Average CTR by group
final_stats_df %>%
  group_by(grouping_by) %>%
  summarise(avg_ctr = sum(sum_Clicks)/sum(sum_Impressions))

# Overall CTR by age group over time 
final_stats_df %>%
  filter(!grouping_by %in% c('Female', 'Male', 'Overall')) %>%
  ggplot(aes(x = nyt_file_order, y = ctr, color = grouping_by)) +
  geom_line()






