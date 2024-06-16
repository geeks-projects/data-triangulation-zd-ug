library(UNEPItargets)

## input dhis2 data

dhis2_monthly <- read_csv("./data/dhis2-monthly-2020-2024.csv") |> 
  mutate(period = ym(period),
         year = year(period), 
         month = month(period, abbr = T, label = T),
         district = str_remove(district, " District"))


dhis2_annual_2020_2024 <- read_csv("./data/dhis2-annual-2020-2024.csv") |> 
  mutate(district = str_remove(district, " District"))

dhis2_annual_2018_2019 <- read_csv("./data/dhis2-annual-2018-2019.csv") |> 
  mutate(district = str_remove(district, " District")) |> 
  filter(year != "2018") |> 
  mutate(zero_dose = round(pop_under_one) - DPT1_given)

dhis2_annual <- bind_rows(dhis2_annual_2020_2024, dhis2_annual_2018_2019)

## input shapefiles

ug_geodata <- sf::read_sf("./shapefiles/local/") |> 
  st_as_sf()

dhis2_annual_data_clean <- ug_geodata |> 
  mutate(District = str_to_title(District), 
         District = str_replace(District, "Madi Okollo", "Madi-Okollo"), 
         District = str_replace(District, "Kassnda", "Kassanda"), 
         District = str_replace(District, "Ssembabule", "Sembabule"), 
         District = str_replace(District, "Namutunmba", "Namutumba")) |>
  left_join(dhis2_annual, by = c("District" = "district")) |> 
  mutate(District = as.factor(District)) |> 
  #filter(year != 2024 ) |> 
  mutate(DPT1_coverage_cat = group_coverage(DPT1_coverage)) 
# summarise() |> 


## Uganda map for coverage

ggplot(dhis2_annual_data_clean ) +
  geom_sf(data = dhis2_annual_data_clean , aes(fill = DPT1_coverage_cat, group = District),
          color = "white",
          linetype = 1) +
#  geom_sf_text(data = dhis2_annual_data_clean ,aes(label =DPT1_coverage ),size = 2,na.rm = T) +
  # Labs
  # labs(title = "title",
  #      fill = "%") +
   scale_fill_manual(values = cols
  #                   #                    label = labs_plot,
  #                   #                       guide = NULL
  #                   #                       guide = guide_legend(direction = "horizontal",
  #                   #                                         nrow = 1,
  #                   #                                         label.position = "bottom")
   )+
  facet_wrap(vars(year)) +
  datastorytelling_map_theme() +
  theme(legend.position = "top")


## Burden of the problem for 100%

# 
# ggplot(dhis2_annual_data_clean ) +
#   geom_sf(data = dhis2_annual_data_clean , aes(fill = DPT1_coverage_cat, group = District),
#           color = "white",
#           linetype = 1) +
#  #geom_sf_text(data = dhis2_annual_data_clean ,aes(label = case_when(DPT1_coverage_cat == "Above 100"~ glue('{District} \n {DPT1_coverage}%'), NULL)),
#  #             size = 2, colour = "black", face = "bold") +
#   # Labs
#   # labs(title = "title",
#   #      fill = "%") +
#   scale_fill_manual(values = c("0 - 50" = "#e6e3e3" , "50 - 80" = "#e6e3e3",
#                                "80 - 100"= "#e6e3e3", "Above 100" = "#D83F31")  )+
#   facet_wrap(vars(period)) +
#   datastorytelling_map_theme() +
#   theme(legend.position = "top")

## Zero dose 

dhis2_annual |> 
  filter(DPT1_coverage >= 100 & zero_dose >= 0  )


ggplot(dhis2_annual_data_clean ) +
  geom_sf(data = dhis2_annual_data_clean, fill = "#e6e3e3", aes(group = District),
          color = "white",
          linetype = 1) +
  geom_sf(data = dhis2_annual_data_clean |> group_by(year)|> top_n(n = 5, zero_dose),
          aes(fill = zero_dose, group = District),
          color = "white",
          linetype = 1) +
  # Labs
  # labs(title = "title",
  #      fill = "%") +
  scale_fill_gradient2(
                       low = "#e6f3e3",
                       mid = "#fc9272",
                       high = "#D83F31")+
  geom_sf_text(data =   dhis2_annual_data_clean |> group_by(year)|> top_n(n = 5, zero_dose),
               aes(label = glue('{District} \n{zero_dose} \n {DPT1_coverage}%'), NULL),
               size = 3, colour = "black")+
  facet_wrap(vars(year)) +
  datastorytelling_map_theme() +
  theme(legend.position = "top") 


 
