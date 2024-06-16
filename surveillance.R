library(tidyverse)
library(ggspatial)
library(glue)
library(sf)


ug_geodata <- sf::read_sf("./shapefiles/local/") |> 
  st_as_sf() |> 
  mutate(District = str_to_title(District), 
         District = str_replace(District, "Madi Okollo", "Madi-Okollo"), 
         District = str_replace(District, "Kassnda", "Kassanda"), 
         District = str_replace(District, "Ssembabule", "Sembabule"), 
         District = str_replace(District, "Namutunmba", "Namutumba"))


surveillance_data <- read_csv("./data/Measles data 2019 to-date _Robinson.csv",
                                       na = "Missing" ) |> 
                    filter(`Measles Lab Results` == 1 | `Rubella Lab Results` == 1) |> 
  mutate(District = str_to_title(District), 
         District = str_replace(District, "Madi Okollo", "Madi-Okollo"), 
         District = str_replace(District, "Kasanda", "Kassanda"), 
         District = str_replace(District, "Fortportal City", "Fort Portal City"),
         District = str_replace(District, "Namaingo", "Namayingo"), 
         across(c(`Date of birth`, `Date of last vaccination`, `Date lab received specimen`,
                         `Date lab sent result to district`, `Date of onset`, `Date district received lab results`), dmy)) |> 
  mutate(year = year(`Date of onset`), 
         month = month(`Date of onset`, label = T, abbr = T)) |> 
  filter(!is.na(year)) |> 
  filter(year != "2007")


## From 2019 to 2024

surveillance_districts <- surveillance_data |> 
  count(District, sort = T, name = "confirmed_cases")

surveillance_districts_clean <- ug_geodata |> 
  left_join(surveillance_districts, by = c("District" = "District")) |> 
  mutate(District = as.factor(District)) |> 
  filter(!is.na(District))



ggplot(ug_geodata ) +
  geom_sf(data = ug_geodata, fill = "#e6e3e3", aes(group = District),
          color = "white",
          linetype = 1)+
  geom_sf(data = surveillance_districts_clean, aes(fill = confirmed_cases, group = District),
          color = "white",
          linetype = 1) +
  scale_fill_gradient(low = "#e6e3e3", high = "#D83F31")+
  geom_sf_text(data =   surveillance_districts_clean |> top_n(n = 25, confirmed_cases),
               aes(label = glue('{District}\n{ confirmed_cases }'), NULL),
               size = 3.3, colour = "black")+
  datastorytelling_map_theme()



### By Year
surveillance_by_year <- surveillance_data |> 
  filter(year %in% c("2019", "2020", "2021", "2022", "2023", "2024") & !is.na(year)) |>
  group_by(District, year) |> 
  summarise(confirmed_cases = n())


### map data 


surveillance_by_year_clean <- ug_geodata |> 
  left_join(surveillance_by_year, by = c("District" = "District")) |> 
  mutate(District = as.factor(District)) |> 
  filter(!is.na(year))

###

district_text <- bind_rows(surveillance_by_year_clean |> group_by(year)|> top_n(n = 9, confirmed_cases), 
                           surveillance_by_year_clean |> filter(District == "Kakumiro"))

ggplot(ug_geodata ) +
  geom_sf(data = ug_geodata, fill = "#e6e3e3", aes(group = District),
          color = "white",
          linetype = 1)+
  geom_sf(data = surveillance_by_year_clean, aes(fill = confirmed_cases, group = District),
          color = "white",
          linetype = 1) +
  scale_fill_gradient(low = "#e6e3e3", high = "#D83F31")+
  geom_sf_text(data =  district_text ,
               aes(label = glue('{District} \n { confirmed_cases }'), NULL),
               size = 3, colour = "black")+
  facet_wrap(vars(year)) +
  datastorytelling_map_theme()



### Age 

age_group_districts <- surveillance_data |> 
  mutate(age_group = case_when(`Age in Mths computed` < 9 ~ "< 9", 
                               `Age in Mths computed` <= 18 ~ "9 - 18",
                               `Age in Mths computed` <= 59 ~ "19 - 59",
                               `Age in Mths computed` <= 180 ~ "60 - 180", 
                               `Age in Mths computed` > 180 ~ "> 180",
                               ) |>  factor(levels = c("< 9", "9 - 18", "19 - 59", "60 - 180", "> 180")),
                                        .before = 9) 


age_group <- age_group_districts |> 
  group_by(age_group, year) |> 
  summarise(confirmed_cases = n()) |> 
  mutate(important = case_when(age_group == "9 - 18" ~ "Yes",
                               .default = "No"
  ))

  ggplot(age_group, aes(x = age_group, y = confirmed_cases)) +
    geom_col(aes(fill = important)) +
    facet_wrap(vars(year)) +
    geom_text(data = age_group |> group_by(year) |> top_n(3, confirmed_cases), mapping = aes( label = confirmed_cases),
              family = "Roboto Mono", position = position_dodge(width = 0.9), vjust = 1.5, colour = "#4F4F4F") +
    scale_fill_manual(values = c("Yes" =  "#D83F31", "No" = "#d6d0d0"))+
    datastorytelling_theme_facet()

### Cases by Sex
  
surveillance_data |> 
 # group_by( `Date of onset`) |> 
  group_by(year, month, Sex) |> 
  summarise(confirmed_cases = n()) |> 
#  mutate(month = factor(months))
  ggplot() +
  geom_line(aes(x = month, y = confirmed_cases, colour = Sex, group = Sex)) +
  facet_wrap(vars(year))+
  datastorytelling_theme_facet()

## By Sex 2019 to 2024 
surveillance_data |> 
  count(Sex, name = "confirmed_cases") |> 
  ggplot(aes(x = Sex, y = confirmed_cases))+
  geom_col()+
  geom_label(aes(label = confirmed_cases))

## 

         