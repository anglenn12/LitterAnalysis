# Looking at data garbage cans, sensors, the fill level, locations, and dates
# There is no information on which condition, with or without a nudging sign --
# a garbage can has been assigned to on various streets in DC. 

# Taking a look at some of the variables available 
df_fills |>
  select(RecordedDateTime, MeId, CalculatedPercentFull)

df_fills |>
  filter(MeId=='A10000502617E8') |>
  
  filter(str_detect(RecordedDateTime,"2017-11-27")) |>
  



  
  ggplot(aes(x=RecordedDateTime, y=CalculatedPercentFull)) + 
  geom_line()
  

daily_max <- df_fills |>
  
  
  mutate(date_reading = date(RecordedDateTime)) |>
  
  group_by(MeId, date_reading) |>
  
  
  summarize(max_fill = max(CalculatedPercentFull)/100)
daily_max
  
# example of a dataframe on someone creating a course


df_can_day <- daily_max |>
  left_join(df_cans, by = "MeId") |>
  
  filter(!is.na(Z)) |>
  
  ungroup() |>
  
  mutate (Z = ifelse(Z == 1, 'Treatment' , 'Control'))



df_fills <- littercan_fills
