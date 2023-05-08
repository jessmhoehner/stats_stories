# Authors:     JH
# Maintainers: JH
# Copyright:   2023, HRW, GPL v4 or later
# ============================================
# stats_stories/clean/src/clean.R

# Load libraries ---------------------------------------------------------------
pacman::p_load("here", "tidyverse", "assertr", 
               "janitor","stringr", "readr", "lubridate")

# input data location
input_data <- here("clean/input/Stats+Stories 300th Episode Data Visualization Contest - Stats.csv")

# output location
output_data <- here("clean/output/cleaned_data.rds")

# read in data
raw_df <- read_csv(input_data, show_col_types = FALSE) 

# munge
# keeping in mind: interest in being able to put data into Kibana 
#                       (raw has errors, need to read listens as a number) 
#                  and group episodes in next task

clean_df <- raw_df %>%
  clean_names() %>%
  # removing one episode that has two categories
  filter(str_detect(category, ",") == FALSE) %>%
  mutate(
    release_date = mdy(release_date_m_d_y), 
    release_year = year(release_date), 
    listens_num = as.numeric(listens)) %>%
  rename(
    episode_num = total_number) %>%
  separate(guest_s, into = c("guest_1", "guest_2", "guest_3"), ",", extra = "merge") %>% 
  separate(panel, into = c("host_1", "host_2", "host_3"), ",", extra = "merge") %>%
  # TODO: test that this logic is working
  mutate(
    guest_num = case_when(
      (is.na(guest_3) == TRUE & is.na(guest_2) == TRUE & is.na(guest_1)) ~ 0,
      (is.na(guest_3) == TRUE & is.na(guest_2) == TRUE) ~ 1,
      (is.na(guest_3) == TRUE & is.na(guest_2) == FALSE) ~ 2,
      is.na(guest_3) == FALSE ~ 3),
    host_num = case_when(
      (is.na(host_3) == TRUE & is.na(host_2) == TRUE) ~ 1,
      (is.na(host_3) == TRUE & is.na(host_2) == FALSE) ~ 2,
      is.na(host_3) == FALSE ~ 3), 
    #episode_text = as.character(str_c(title, description, sep = " "))
    ) %>%
  select(release_date, release_year, episode_num, category, format, 
         listens_num, guest_1, guest_2, guest_3, guest_num, 
         starts_with("host_"), length)

clean_df %>%
saveRDS(output_data)
