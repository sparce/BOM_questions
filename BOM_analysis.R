library(tidyverse)

BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

#Q1: For each station, how many days have a minimum temperature, a maximum temperature 
#    and a rainfall measurement recorded?
#
#    From looking at data, all in BOM_data, and it looks like unmeasured values are '-'
#    Temp min/max are stored in the same column, will need to separate it, then filter for rows
#    that are not '-' in the three measurements, then summarise the number of rows left per station

#Will need this data frame again later
BOM_with_temps <- BOM_data %>% 
  separate(Temp_min_max, into = c("t_min", "t_max"), sep = "/") %>% 
  filter(t_min != "-", t_max != "-", Rainfall != "-")

q1_ans <- BOM_with_temps %>% 
  group_by(Station_number) %>% 
  summarise(num_records = n())

#Print it to the screen if running in RStudio
q1_ans

#Save it to a file 
write_csv(q1_ans, "results/q1_record_completeness_by_station.csv")

#Q2: Which month saw the lowest difference between minimum and maximum temperatures in a day? 
#    And which state saw the highest?
#
#    Month data is found in BOM_data, state is in BOM_stations so will need a join for the last part
#    To find the *difference* between min amd max temp, will need to mutate the data. Can then
#    arrange it to look at highest/lowest.

BOM_with_temps %>% 
  mutate(t_diff = as.numeric(t_max) - as.numeric(t_min)) %>%  # t_min/t_max are text ("<chr>") need to convert them to numbers 
  arrange(t_diff)

# Quite a few days with equal min and max temps. 
# Let's look at the differences averaged across a month instead
q2a_ans <- BOM_with_temps %>% 
  mutate(t_diff = as.numeric(t_max) - as.numeric(t_min)) %>%
  group_by(Month) %>% 
  summarise(avg_t_diff = mean(t_diff)) %>% 
  arrange(avg_t_diff)

#Print it to the screen if running in RStudio
#Can see that it's Month 6 (June) with the lowest average temp difference 
q2a_ans


