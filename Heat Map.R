# Description 
# To create heat maps using population estimates and spatial data of New Zealand.

# Set up ---------------------------------------------------------------
## Libraries ----
library(readxl)
library(tidyverse)
library(sf)
library(stringi)
# library(patchwork)

## Data sources ----
# StatsNZ (Infoshare) 
  # - population data 
  # - district (territorial authority) outline 
  # - region outline
# Land Information New Zealand (LINZ) 
  # - coastline 

## Data ----
## Population data sourced at the following link, and appropriate tables loaded
## via the "population data.xlsx"
## https://www.stats.govt.nz/information-releases/subnational-population-estimates-at-30-june-2022-provisional/
## district data is table 2 from sub-national population estimates linked above
## region data is table 1 from sub-national population estimates linked above 

### District (load and format) ----
district_population <- read_excel("./Data/population data.xlsx", sheet  = "district") %>% 
  # create either_area to describe location, using TA where possible, otherwise AKL Local Board
  mutate(either_area = ifelse(is.na(`Territorial Authority`), `Akl Local Board`, `Territorial Authority`)) %>% 
  # convert to ASCII and title case (accounts for macrons)
  mutate(either_area = stri_trans_general(either_area, "Latin-ASCII")) %>% 
  mutate(either_area = str_to_title(either_area)) %>% 
  # order columns
  select(either_area, `2018`, `2020`, `2021`, `2022`, everything())

### Region (load and format) ----
region_population <- read_excel("./Data/population data.xlsx", sheet  = "region") %>% 
  # convert to ASCII and title case (accounts for macrons)
  mutate(either_area = stri_trans_general(Region, "Latin-ASCII")) %>% 
  mutate(either_area = str_to_title(either_area))

### DairyNZ colours (hex codes) ----
dnz_blue <- "#00D7FF"
dnz_yellow <- "#EBB928"
dnz_dark_green <- "#00461E"
dnz_light_green <- "#3C9628"

# Shapefiles --------------------------------------------------------------
# you might need to create a LINZ account to download these, or grab them from 
# this repo

## Outline of New Zealand ----
## In LINZ datafinder the coastline .shp is called "NZ Coastlines and Islands Polygons Topo 1:50k"
## https://data.linz.govt.nz/layer/51153-nz-coastlines-and-islands-polygons-topo-150k/
nz_coastline <- st_read(
  "./Data/Coastline Shapefile/nz-coastlines-and-islands-polygons-topo-150k.shp")

## Districts / Local Boards Outline ----
## In StatsNZ datafinder this TA shapefile is called "Territorial Authority Local Board 2023 (generalised)"
## https://datafinder.stats.govt.nz/layer/111184-territorial-authority-local-board-2023-generalised/
territorial_authorities <- st_read(
  "./Data/Local Board Shapefile/territorial-authority-local-board-2023-generalised.shp"
) %>% 
  # ensure the Co-ordinate Reference System is the same accross layers
  st_transform(crs = st_crs(nz_coastline)) %>% 
  st_make_valid() %>%
  # convert to ASCII and title case (accounts for macrons)
  mutate(either_area = stri_trans_general(TALB2023_2, "Latin-ASCII")) %>% 
  mutate(either_area = str_to_title(either_area)) 

# trim the territorial authorities such that they don't extend past the coastline
trimmed_ta <- st_intersection(nz_coastline, territorial_authorities)

# ggplot() +
#   geom_sf(data = trimmed_ta) +
#   coord_sf(xlim = c(165, 180))

## Regions Outline ----
## https://datafinder.stats.govt.nz/layer/111182-regional-council-2023-generalised/
region_shapefile <- st_read("./Data/Region Shapefile 2023/regional-council-2023-generalised.shp") %>% 
  # ensure the Co-ordinate Reference System is the same accross layers
  st_transform(crs = st_crs(nz_coastline)) %>% 
  st_make_valid()  %>%
  # convert to ASCII and title case (accounts for macrons)
  mutate(either_area = stri_trans_general(REGC2023_2, "Latin-ASCII")) %>% 
  mutate(either_area = str_to_title(either_area)) 

# trim the regions such that they don't extend past the coastline
trimmed_region <- st_intersection(nz_coastline, region_shapefile)

# ggplot() +
#   geom_sf(data = trimmed_region) +
#   coord_sf(xlim = c(165, 180))

# Join shapies and population data ----------------------------------------
## Join the population data to the TA shapefile
territorial_authorities_joined <- left_join(
  trimmed_ta, 
  district_population, 
  by = c("either_area" = "either_area")
                                           )
## Join the population data to the Region shapefile
region_joined <- left_join(
  trimmed_region, 
  region_population, 
  by = c("either_area" = "either_area")
                          )

# District Plots ----------------------------------------------------------
ggplot() +
  # add a simple features layer using coastline data 
  geom_sf(data = nz_coastline, fill = "grey", color = "black") +
  # add a simple features layer using the territorial authorities data
  geom_sf(data = territorial_authorities_joined, aes(fill = `2022`), color = "black") +
  # fill the map with a colour gradient
  # scale_fill_steps(low = dnz_blue, high = dnz_dark_green) +
  scale_fill_viridis_c(option = "C", na.value = "grey90", name = "Population") +
  # format plot
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(
    title = "Population Estimates by Territorial Authority 2022",
    caption = "Data Sources: StatsNZ, Land Information New Zealand"
  ) +
  # set limits to New Zealand
  coord_sf(xlim = c(165, 180))

# save the plot
ggsave(plot = last_plot(),
       filename = "./District Population Heatmap 2022.png",
       width = 200,
       height = 150,
       units = c("mm"))

# Region Plots ------------------------------------------------------------
ggplot() +
  # add a simple features layer using coastline data
  geom_sf(data = nz_coastline, fill = "grey", color = "black") +
  # add a simple features layer using the region data
  geom_sf(data = region_joined, aes(fill = `2022`), color = "black") +
  # fill the map with a colour gradient
  scale_fill_steps(low = dnz_blue, high = dnz_yellow, name = "Population") +
  # scale_fill_viridis_c(option = "C", na.value = "grey90", name = "Population") +
  # format plot
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(
    title = "Population Estimates by Region 2022",
    caption = "Data Sources: StatsNZ, Land Information New Zealand"
  ) +
  # set limits to New Zealand
  coord_sf(xlim = c(165, 180))

# save the plot
ggsave(plot = last_plot(),
       filename = "./Regional Population Heatmap 2022.png",
       width = 200,
       height = 150,
       units = c("mm"))

# Functionalisation -------------------------------------------------------
# Create a function of the plots above to allow for easy plotting 
# across different years and aggregations 

heat_map_fx <- function(
    # set argument values
    aggregation = c("region", "district"), 
    pop_year = c("2018", "2020", "2021", "2022"))
  {
  
  # debugging ----
  # aggregation <- "district"
  # pop_year <- `2022`
  
  # check arguments match acceptable values
  aggregation <- match.arg(aggregation)
  pop_year <- match.arg(pop_year)

  # select the data to plot based on the aggregation
if (aggregation == "district") {
  plot_data <- territorial_authorities_joined
} else {
  plot_data <- region_joined
}
  
  # turn the population year into a symbol
pop_year_sym <- sym(pop_year)
  
plot <- ggplot() +
  # add a simple features layer using coastline data (consistent across maps)
  geom_sf(data = nz_coastline, fill = "grey", color = "black") +
  # add a simple features layer depending on the plot_data set above based on
  # aggregation function argument
  geom_sf(data = plot_data, aes(fill = !!pop_year_sym), color = "black") +
  # fill the map with a colour gradient
  scale_fill_viridis_c(option = "C", 
                       na.value = "grey90", 
                       name = "Population", 
                       labels = scales::comma) +
  theme_minimal() +
  # set x limits to New Zeaalnd
  coord_sf(xlim = c(165, 180)) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold")
  ) +
  # annotate the plot based on the pop_year and aggregation arguments
  labs(
    title = str_c(pop_year, " Population Estimates by ", str_to_title(aggregation)),
    legend.title = "Population",
    caption = "Data Sources: StatsNZ, Land Information New Zealand")

return(plot)  
  
}


# Patchwork using function ------------------------------------------------
## Here we create a patchwork of heat maps compiled into one image using 
## patchwork
# deploy the function 
a <- heat_map_fx(aggregation = "region", pop_year = "2022") + 
  theme(plot.title = element_blank(),  # Remove titles
        axis.text = element_blank(),   # Remove axis text
        axis.ticks = element_blank(),  # Remove axis ticks
        axis.title = element_blank(),  # Remove axis titles
        plot.caption = element_blank()) # Remove captions
b <- heat_map_fx(aggregation = "district", pop_year = "2022") + 
  theme(plot.title = element_blank(),  # Remove titles
        axis.text = element_blank(),   # Remove axis text
        axis.ticks = element_blank(),  # Remove axis ticks
        axis.title = element_blank(),  # Remove axis titles
        plot.caption = element_blank()) # Remove captions
c <- heat_map_fx(aggregation = "region", pop_year = "2018") + 
  theme(plot.title = element_blank(),  # Remove titles
        axis.text = element_blank(),   # Remove axis text
        axis.ticks = element_blank(),  # Remove axis ticks
        axis.title = element_blank(),  # Remove axis titles
        plot.caption = element_blank()) # Remove captions
d <- heat_map_fx(aggregation = "district", pop_year = "2018") + 
  theme(plot.title = element_blank(),  # Remove titles
        axis.text = element_blank(),   # Remove axis text
        axis.ticks = element_blank(),  # Remove axis ticks
        axis.title = element_blank(),  # Remove axis titles
        plot.caption = element_blank()) # Remove captions

# combine the plot objects using patchwork 
# patch <- (a + b) / (c + d) + 
#   plot_layout(guides = "collect")  &
#   # remove all the plot elements that are not needed
#   theme(
#     legend.position = "none",      # Remove the legend
#     plot.title = element_blank(),  # Remove titles
#     axis.text = element_blank(),   # Remove axis text
#     axis.ticks = element_blank(),  # Remove axis ticks
#     axis.title = element_blank(),  # Remove axis titles
#     plot.caption = element_blank() # Remove captions
#   )  
#   # annotate combined plot

# patch + 
#   plot_annotation('Population Growth by Region and District 2018-2022', 
#                   caption = 'Data Sources: StatsNZ, Land Information New Zealand',
#                   theme = theme(
#                     plot.title = element_text(hjust = 0.5),
#                     plot.caption = element_text(hjust = 0.5))
#                   ) +
#   plot_layout(guides = "collect")

# Didn't use patchwork because shared legend required the same x limits, 
# ggarrange was more flexible so was used instead

arranged <- ggarrange(a, b, c, d, 
          ncol = 2, 
          nrow = 2, 
          common.legend = TRUE, 
          legend = "right")   

annotate_figure(p = arranged, 
                  top = text_grob("Population Growth by Region (Left) and District (Right) 2018-2022", 
                                  face = "bold", size = 16),
                  bottom = text_grob("Data Sources: StatsNZ, Land Information New Zealand", 
                                     face = "italic", size = 10))

ggsave(plot = last_plot(),
       filename = "./Combined Heatmap.png",
       width = 200,
       height = 150,
       units = c("mm"))

