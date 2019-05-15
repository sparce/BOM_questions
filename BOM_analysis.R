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

# Q2: Which month saw the lowest difference between minimum and maximum temperatures in a day? -----
#    And which state saw the highest?
# 
#    Month data is found in BOM_data, state is in BOM_stations so will need a join for the last part
#    To find the *difference* between min amd max temp, will need to mutate the data. Can then
#    arrange it to look at highest/lowest.

BOM_with_temps %>% 
  mutate(t_diff = as.numeric(t_max) - as.numeric(t_min)) %>%  # t_min/t_max are text ("<chr>") need to convert them to numbers 
  filter(!is.na(t_diff)) %>% #t_diff will be NA where the t_min or t_max values were missing, need to remove those lines
  arrange(t_diff)

# Quite a few days with equal min and max temps. 
# Let's look at the differences averaged across a month instead
q2a_ans <- BOM_with_temps %>% 
  mutate(t_diff = as.numeric(t_max) - as.numeric(t_min)) %>%
  filter(!is.na(t_diff)) %>% 
  group_by(Month) %>% 
  summarise(avg_t_diff = mean(t_diff)) %>% 
  arrange(avg_t_diff)

#Print it to the screen if running in RStudio
#Can see that it's Month 6 (June) with the lowest average temp difference 
q2a_ans

# To bring in the state information, we will need to join it with BOM_stations. But BOM_stations has
# the station identifiers as column headers, so need to gather them first

# We want to take the column names and store them in a new column called "Station_number" to match 
# how they are named in the BOM_data data frame. We will create a new column called "values" to store 
# all the information that used to be stored in the columns under each station number. By default we 
# would gather up the data from *all* columns doing this, but we want to do it for everything but the 
# info column, so I'll exclude that with '-info'

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
# This is essentially the reverse of the gather step and so is a spread

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

# So need to convert them to the same data type. I'm going to be slightly dangerous and just overwrite
# the column in the stations_tidy data frame. It's unlikely to be an issue in this case as we are
# never going to need the station numbers as a character

stations_tidy <- mutate(stations_tidy, Station_number = as.numeric(Station_number))

#Join the two together properly now. Both have a "Station_number" column to join on
#This brings the sstation metadata (including state) into our data frame with the meterological measurements
BOM_combined <- left_join(BOM_with_temps, stations_tidy)

# Now can run the same analysis as for Q2a. Only differences are that we will group by state instead
# of month, and we want to see the *highest* average temperature difference, so we will arrange the
# values in descending order
q2b_ans <- BOM_combined %>% 
  mutate(t_diff = as.numeric(t_max) - as.numeric(t_min)) %>%
  filter(!is.na(t_diff)) %>% 
  group_by(state) %>% 
  summarise(avg_t_diff = mean(t_diff)) %>% 
  arrange(desc(avg_t_diff))

#Print it to the screen if running in RStudio
#Can see that the ACT has the highest average differences between min and max temperatures (14.5)
q2b_ans

#Write out both Q2 answers to a file
write_csv(q2a_ans, "results/q2a_avg_tempdiff_by_month.csv")
write_csv(q2b_ans, "results/q2b_avg_tempdiff_by_state.csv")

#Q3: Which state had the lowest average monthly minimum temperature after excluding sites more  ----
#    than 500m above sea level.
#
#    Bit of interpretation required here. I read it as requiring two averages, first the monthly 
#    average minimum per state, then averaging those monthly average minimums for each state.
#
#    You may interpret the question differently, as only needing the first averaging step. In that
#    case, you won't need step 4 below:
#
#    1) filter to include sites above 500m and that have minimum temperature data
#    2) group by the state and month
#    3) take the average of the minimum temperature, will be average per month, per state
#    4) take the average of those averages, will be an average of the monthly average minimum per state
#    5) arrange by our final summary value to see the state with the lowest

# t_min is still a character in BOM_combined at the moment, so we would need to convert it with 
# eg. mutate(BOM_combined, t_min = as.numeric(t_min))

# But I'll introduce a new function type_convert() that looks at all columns in a data frame and tries
# to convert them to their most likely data type.
# Since we remove rows with "-" for t_min, all that should be left is numbers
# and so type_convert() will notice that and convert them.

q3_ans <- BOM_combined %>% 
  filter(t_min != "-", elev < 500) %>% #"elev"ation data came from the BOM_stations data frame
  type_convert() %>% # t_min is now a numeric type (as are several other columns)
  group_by(state, Month) %>% 
  summarise(avg_monthly_min = mean(t_min)) %>% 
  summarise(avg_min = mean(avg_monthly_min)) %>% 
  arrange(avg_min)

# Print it to the screen if running in RStudio
# VIC comes out with the lowest average minimum temp in this case (8.42)
# If you did only the first summary step above, you will get a different table that has VIC in 
# Month 7 (July) on top with an average minimum for the month of 2.98
q3_ans

#Write out the anQ3 ansswer to a file
write_csv(q3_ans, "results/q3_averaged_monthly_minimums_per_state.csv")
