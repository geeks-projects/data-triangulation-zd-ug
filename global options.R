library(tidyverse)
library(ggspatial)
library(glue)
library(sf)
library(ragg)
library(scales)
library(showtext)



## Get San serif = Roboto
font_add_google("Roboto", family = "Roboto")


## Get Serif  = Lora
#They generally, I would say worked better as like titles, subtitles, annotations.
#But I wouldn't really recommend them for axis text unless they're very low contrast,
#They do work very well for extended reading.
font_add_google("Lora", family = "Lora")

## Monospaced
# monospaced fonts don't work super well for like long extended reading.
#  things like annotations, labels, text, 
# like for Axis, labels or for number annotations that I put on bars. 

font_add_google("Roboto Mono", "Roboto Mono")

showtext_auto()

grey_light <- "#888A8C"
grey_dark <- "#939598"
blue_dark <- "#144673"



## Gobal variables

cols <- c("0 - 50" =  "#D83F31", "50 - 80" = "#E9B824",
          "80 - 100"= "#219C90", "Above 100" ="#e6e3e3")

group_coverage <- function(x){
  
  case_when(x <= 50 ~ "0 - 50", 
            x <= 80 ~ "50 - 80", 
            x <= 100 ~ "80 - 100", 
            x > 100 ~ "Above 100"
  ) |> 
    factor(levels = c("0 - 50", "50 - 80", "80 - 100", "Above 100"))
}



datastorytelling_theme <- function(){
  theme_classic()+
  theme(axis.text = element_text(family = "Roboto Mono", size = 12, colour = "#888A8C"),
        axis.ticks = element_line(colour = grey_light),
        axis.title = element_text(family = "Roboto",  size = 15, colour = grey_light),
        axis.line = element_line(colour = grey_light),
        legend.text = element_text(colour = grey_light),
        legend.title = element_text(colour = grey_light, face = "bold"),
        legend.position = "none",
        plot.subtitle =  element_text(family = "Roboto",  size = 20, colour = grey_light),
        plot.title  = element_text(family = "Roboto",  size = 30, colour = "#585859"))
}

datastorytelling_theme_facet <- function(){
  theme_bw()+
    theme(axis.text = element_text(family = "Roboto Mono", size = 12, colour = "#888A8C"),
          axis.ticks = element_line(colour = grey_light, size = 0.5),
          axis.title = element_text(family = "Roboto",  size = 15, colour = grey_light),
         # axis.line = element_blank(),
          strip.background = element_rect(color=grey_light, fill= "#fafafa", size= 0.5, linetype="solid"),
          panel.border = element_rect(color= grey_light, fill= NA, size= 0.5, linetype="solid"),,
          panel.grid = element_blank(),
          legend.text = element_text(colour = grey_light),
          legend.title = element_text(colour = grey_light, face = "bold"),
          legend.position = "none",
          strip.text = element_text(family = "Roboto",  size = 12, colour = grey_light),
          plot.subtitle =  element_text(family = "Roboto",  size = 20, colour = grey_light),
          plot.title  = element_text(family = "Roboto",  size = 30, colour = "#585859"))
}

datastorytelling_map_theme <- function(){
  theme_void()+
  theme(legend.text = element_text(colour = grey_light),
        legend.title = element_text(colour = grey_light, face = "bold"),
        strip.text = element_text(family = "Roboto",  size = 12, colour = grey_light),
        plot.subtitle =  element_text(family = "Roboto",  size = 20, colour = grey_light),
        plot.title  = element_text(family = "Roboto",  size = 30, colour = "#585859"),
        legend.position = "top"
        )
}
