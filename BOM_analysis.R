library(tidyverse)

BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

#Q1: For each station, how many days have a minimum temperature, a maximum temperature -----
#    and a rainfall measurement recorded?
#
#    From looking at data, all in BOM_data, and it looks like unmeasured values are '-'
#    Temp min/max are stored in the same column, will need to separate it, then filter for rows
#    that are not '-' in the three measurements, then summarise the number of rows left per station

#Will need this data frame again later
BOM_with_temps <- BOM_data %>% 
  separate(Temp_min_max, into = c("t_min", "t_max"), sep = "/") 

q1_ans <- BOM_with_temps %>% 
  filter(t_min != "-", t_max != "-", Rainfall != "-") %>% 
  group_by(Station_number) %>% 
  summarise(num_records = n())

#Print it to the screen if running in RStudio
q1_ans

#Save it to a file 
write_csv(q1_ans, "results/q1_record_completeness_by_station.csv")

# Q2: Which month saw the lowest average daily temperature difference? -----
# 
#    To find the *difference* between min amd max temp, will need to mutate the data. Can then
#    arrange it to look at highest/lowest.

q2_ans <- BOM_with_temps %>% 
  # t_min/t_max are text ("<chr>") need to convert them to numbers
  mutate(t_diff = as.numeric(t_max) - as.numeric(t_min)) %>%
  #t_diff will be NA where the t_min or t_max values were missing, need to remove those lines
  filter(!is.na(t_diff)) %>% 
  group_by(Month) %>% 
  summarise(avg_t_diff = mean(t_diff)) %>% 
  arrange(avg_t_diff)

#Print it to the screen if running in RStudio
#Can see that it's Month 6 (June) with the lowest average temp difference 
q2_ans

#Write out Q2 answer to a file
write_csv(q2_ans, "results/q2_avg_tempdiff_by_month.csv")

# Q3: Which state saw the lowest average daily temperature difference? -----

# State information does not exist in the BOM_data.csv file. It is in the BOM_stations.csv file. We
# will need to join these two data frames together to use them. But BOM_stations.csv has
# the station identifiers as column headers, while BOM_data.csv has them as the values in a column.
# We will need to tidy BOM_stations.csv with a gather and spread before we can join it.

# We want to take the column names and store them in a new column called "Station_number" to match 
# how they are named in the BOM_data data frame. We will create a new column called "values" to store 
# all the information that used to be stored in the columns under each station number. By default we 
# would gather up the data from *all* columns doing this, but we want to do it for everything but the 
# info column, so we can exclude that with '-info'

stations_very_long <- BOM_stations %>% 
  gather(key = "Station_number", value = "values", -info) 

# Eg of structure:
# A tibble: 140 x 3
# info  Station_number values                
# <chr> <chr>          <chr>                 
#  elev  9194           14                    
#  end   9194           2018                  
#  lat   9194           -32.2208           

# We now want to restructure the data frame again. This time creating new columns with names from the 
# current info column and the contents of the new columns coming from the current 'values' column
# This is the reverse of a gather step and so is a spread

stations_tidy <- stations_very_long %>% 
  spread(key = info, value = values)

# Eg. of structure:
# A tibble: 20 x 8
# Station_number elev  end   lat      lon      name                              start state
# <chr>          <chr> <chr> <chr>    <chr>    <chr>                             <chr> <chr>
#  14825          88.5  2018  -16.403  131.0145 VICTORIA RIVER DOWNS              1885  NT   
#  14909          416   2018  -13.3275 133.0861 CENTRAL ARNHEM PLATEAU            2003  NT   
#  22050          41.1  2018  -33.9703 137.6628 KADINA AWS                        2005  SA   

# Try to join the two together now. Both have a "Station_number" column to join on
# But this line gives an error:

#BOM_combined <- left_join(BOM_data, stations_tidy)
#
#Error: Can't join on 'Station_number' x 'Station_number' because of incompatible types (character / numeric)

# So we need to convert them to the same data type. A slightly dangerous method is to just overwrite
# the column in the stations_tidy data frame. It's unlikely to be an issue in this case as we are
# never going to need the station numbers as a character

stations_tidy <- mutate(stations_tidy, Station_number = as.numeric(Station_number))

#Join the two together properly now. Both have a "Station_number" column to join on
#This brings the station metadata (including state) into our data frame with the meterological measurements
BOM_combined <- left_join(BOM_with_temps, stations_tidy)

# Now can run the same analysis as for Q2. Only differences are that we will group by state instead
# of month, and we want to see the *highest* average temperature difference, so we will arrange the
# values in descending order
q3_ans <- BOM_combined %>% 
  mutate(t_diff = as.numeric(t_max) - as.numeric(t_min)) %>%
  filter(!is.na(t_diff)) %>% 
  group_by(state) %>% 
  summarise(avg_t_diff = mean(t_diff)) %>% 
  arrange(desc(avg_t_diff))

#Print it to the screen if running in RStudio
#Can see that the ACT has the highest average differences between min and max temperatures (14.5)
q3_ans


write_csv(q3_ans, "results/q3_avg_tempdiff_by_state.csv")

#Q4: Does the westmost (lowest longitude) or eastmost (highest longitude) weather station ------ 
#    in our dataset have a higher average solar exposure?

# Longitude of a station is stored in the BOM_stations.csv file, solar exposure is stored in the
# BOM_data.csv file. We have already combined these two and stored them in the BOM_combined variable.

# So the steps we need to perform are:
#     1) Start with the data in BOM_combined
#     2) Convert solar exposure and longitude values to numbers from text 
#        (check the types by printing BOM_combiened out to the screen)
#     3) Remove rows with no solar exposure measurements
#     4) Keep only rows from the station with the highest or lowest longitude
#     5) Group our data by station
#     6) Summarise out data to get the average solar exposure

q4_ans <- BOM_combined %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure), lon = as.numeric(lon)) %>%
  # Could have combined these into one filter function, but I'll separate them for a clearer example
  filter(!is.na(Solar_exposure)) %>% 
  filter(lon == min(lon) | lon == max(lon)) %>% # Could instead use range(): lon %in% range(lon)
  group_by(Station_number, lon) %>% # Group by lon as well so that it appears in our final table
  summarise(avg_solar_exp = mean(Solar_exposure))


# Print it to the screen if running in RStudio
# The eastmost station (longitude ~153) has a higher average solar exposure. 
# But not by much (19.5 vs 19.2)
q4_ans

#Write out the Q4 answer to a file
write_csv(q4_ans, "results/q4_average_solar_exposure_for_furthest_stations.csv")
