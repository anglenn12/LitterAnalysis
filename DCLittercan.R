littercan_fills
littercans_randomized

library(tidyverse)
library(stats)
library(dplyr)
library(ggplot2)

df_fills <- littercan_fills
df_fills

# we see data about garbage cans, sensors, the fill level, 
#locations, and dates. There is no information on what condition -- 
#with or without a nudging sign -- a garbage can has been assigned to in DC
#We're looking an additional data file. 

df_cans <- littercans_randomized
df_cans

#In this second datafile we see can-level information in DC, the address, the street
#the identifier of the block, and the side of the street. In North America, odd numbers
#are on the side of the street, while even numbers are on the other. In the database
#we see the assignment variable here, 'Z', which is a '0' if the garbage can had no sign
#on it and a '1' if the can did have a sign on it

#In this datafile, we have one file which all of the observations that were taken
#thats our 'df_fills', and we have another which has details about the assignment condition
#and information about the population of the samples (in this case garbage cans) in 
#our experiment, it is considered our 'df_cans', we need to join these two dataframes together
#We have a unique garbage can identifer in the datasets, 'MeId', and this will be key to our dataset


#Taking a look at our variables in our datafile
df_fills |> 
  select(RecordedDateTime, MeId, CalculatedPercentFull)

#It looks like we gave a can identifier, a timestamp of when vaues were measured, and a
#a percentage of fullness. Looking closely you'll see that the fullness is 
#measured more than once per day. 

# we're doing a bit of exploratory data analysis in a single garbage can using a specific date

df_fills |>
  
#Taking the first MeId 
  filter(MeId=='A10000502617E8') |>
  #And using the first day
  filter(str_detect(RecordedDateTime, "2017-11-27")) |>
  # Now a quick little line plot
  ggplot(aes(x=RecordedDateTime, y=CalculatedPercentFull)) +
  geom_line()

#We have a variable fill level being measured out over the day, Let's now the calculate the 'average
#of the maximum fullness per can, per day' which means we need to find the maximum fullness of a can on a
#given day, and then calculate the average of that across all of the days for each can. 

library(lubridate)

daily_max <- df_fills |>
  #We're converting this to an actual date object using using lubridate. This will get rid of the time
  #component. you can treat this as a string instead.
  #We need to get rid of the time component. We can treat this is as a string.
  mutate(date_reading = date(RecordedDateTime)) |>
  #Now we group by our can ids and dates
  group_by(MeId, date_reading) |>
  #Creating a new variable, which is the maximum of the CalculatedPercentFull, and divide by 100 
  #to get a number between 0 and 1
  summarize(max_fill = max(CalculatedPercentFull)/100)
daily_max


# Updating the line plot with a new can and add breaks

df_fills |>
  filter(MeId=='A10000438B4753') |>
  ggplot(aes(x=RecordedDateTime, y=CalculatedPercentFull)) +
  geom_line() +
  scale_y_continuous(n.break=10)

daily_max <- daily_max |>
  mutate(max_fill=(if_else(max_fill>1.0,1.0,max_fill)))





  

