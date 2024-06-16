

dhis2_monthly <- read_csv("./data/dhis2-monthly-2019-2023.csv") |> 
  mutate(period = ym(period),
         year = year(period), 
         month = month(period, abbr = T, label = T),
         district = str_remove(district, " District")) |> 
  select(-c(period, region)) |> 
  group_by(year, month) |> 
  summarise(confirmed_cases = n())



surveillance_monthly <- surveillance_data |> 
  group_by(year, month) |> 
  summarise(confirmed_cases = n())

surveillance_monthly |> 
  sem_join()
