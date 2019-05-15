library(tidyverse)

BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

#Q1: For each station, how many days have a minimum temperature, a maximum temperature 
#    and a rainfall measurement recorded?
#
#    From looking at data, all in BOM_data, and it looks like unmeasured values are '-'
#    Temp min/max are stored in the same column, will need to separate it, then filter for rows
#    that are not '-' in the three measurements, then summarise the number of rows left per station

q1_ans <- BOM_data %>% 
  separate(Temp_min_max, into = c("t_min", "t_max"), sep = "/") %>% 
  filter(t_min != "-", t_max != "-", Rainfall != "-") %>% 
  group_by(Station_number) %>% 
  summarise(num_records = n())

#Print it to the screen if running in RStudio
q1_ans

#Save it to a file 
write_csv(q1_ans, "results/q1_record_completeness_by_station.csv")