# Description 
# To create heat maps using population estimates and Spatial Data of New Zealand.

# Set up ---------------------------------------------------------------
## Libraries ----
library(readxl)
library(tidyverse)
library(sf)
library(stringi)
library(patchwork)

## Data ----
## Population data sourced at the following link, and appropriate tables loaded
## via the "population data.xlsx"
## https://www.stats.govt.nz/information-releases/subnational-population-estimates-at-30-june-2022-provisional/

district_population <- read_excel("./Data/population data.xlsx", sheet  = "district") %>% 
  mutate(either_area = ifelse(is.na(`Territorial Authority`), `Akl Local Board`, `Territorial Authority`)) %>% 
  mutate(either_area = stri_trans_general(either_area, "Latin-ASCII")) %>% 
  mutate(either_area = str_to_title(either_area)) %>% 
  select(either_area, `2018`, `2020`, `2021`, `2022`, everything())

region_population <- read_excel("./Data/population data.xlsx", sheet  = "region") %>% 
  mutate(either_area = stri_trans_general(Region, "Latin-ASCII")) %>% 
  mutate(either_area = str_to_title(either_area))

dnz_blue <- "#00D7FF"
dnz_yellow <- "#EBB928"
dnz_dark_green <- "#00461E"
dnz_light_green <- "#3C9628"

# Shapefiles --------------------------------------------------------------

## Outline of New Zealand ----
## In LINZ datafinder the coastline .shp is called "NZ Coastlines and Islands Polygons Topo 1:50k"
nz_coastline <- st_read(
  "./Data/Coastline Shapefile/nz-coastlines-and-islands-polygons-topo-150k.shp")

## Districts / Local Boards of New Zealand ----
## In StatsNZ datafinder this TA shapefile is called "Territorial Authority Local Board 2023 (generalised)"
territorial_authorities <- st_read(
  "./Data/Local Board Shapefile/territorial-authority-local-board-2023-generalised.shp"
) %>% 
  st_transform(crs = st_crs(nz_coastline)) %>% 
  st_make_valid() %>%
  mutate(either_area = stri_trans_general(TALB2023_2, "Latin-ASCII")) %>% 
  mutate(either_area = str_to_title(either_area)) 

trimmed_ta <- st_intersection(nz_coastline, territorial_authorities)

# ggplot() +
#   geom_sf(data = trimmed_ta) +
#   coord_sf(xlim = c(165, 180))

## Regions of New Zealand ----
## https://datafinder.stats.govt.nz/layer/111182-regional-council-2023-generalised/
region_shapefile <- st_read("./Data/Region Shapefile 2023/regional-council-2023-generalised.shp") %>% 
  st_transform(crs = st_crs(nz_coastline)) %>% 
  st_make_valid()  %>%
  mutate(either_area = stri_trans_general(REGC2023_2, "Latin-ASCII")) %>% 
  mutate(either_area = str_to_title(either_area)) 

trimmed_region <- st_intersection(nz_coastline, region_shapefile)

# ggplot() +
#   geom_sf(data = trimmed_region) +
#   coord_sf(xlim = c(165, 180))


# Join shapies and population data ----------------------------------------
territorial_authorities_joined <- left_join(trimmed_ta, district_population, by = c("either_area" = "either_area"))
region_joined <- left_join(trimmed_region, region_population, by = c("either_area" = "either_area"))

# District Plots ----------------------------------------------------------

ggplot() +
  geom_sf(data = nz_coastline, fill = "grey", color = "black") +
  geom_sf(data = territorial_authorities_joined, aes(fill = `2022`), color = "black") +
  # scale_fill_steps(low = dnz_blue, high = dnz_dark_green) +
  scale_fill_viridis_c(option = "C", na.value = "grey90", name = "Population") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(
    title = "Population Estimates by Territorial Authority 2022",
    caption = "Data Sources: StatsNZ, Land Information New Zealand"
  ) +
coord_sf(xlim = c(165, 180))

ggsave(plot = last_plot(),
       filename = "./District Population Heatmap 2022.png",
       width = 200,
       height = 150,
       units = c("mm"),
       bg = "white")

# Region Plots ------------------------------------------------------------
ggplot() +
  geom_sf(data = nz_coastline, fill = "grey", color = "black") +
  geom_sf(data = region_joined, aes(fill = `2022`), color = "black") +
  # scale_fill_steps(low = dnz_blue, high = dnz_dark_green) +
  scale_fill_viridis_c(option = "C", na.value = "grey90", name = "Population") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(
    title = "Population Estimates by Region 2022",
    caption = "Data Sources: StatsNZ, Land Information New Zealand"
  ) +
  coord_sf(xlim = c(165, 180))

ggsave(plot = last_plot(),
       filename = "./Regional Population Heatmap 2022.png",
       width = 200,
       height = 150,
       units = c("mm"),
       bg = "white")

# Functionalisation -------------------------------------------------------

heat_map_fx <- function(
    aggregation = c("region", "district"), 
    pop_year = c("2018", "2020", "2021", "2022"))
  {
  
  # debugging ----
  # aggregation <- "district"
  # pop_year <- `2022`
  
  aggregation <- match.arg(aggregation)
  pop_year <- match.arg(pop_year)

if (aggregation == "district") {
  plot_data <- territorial_authorities_joined
} else {
  plot_data <- region_joined
}
  
pop_year_sym <- sym(pop_year)
  
plot <- ggplot() +
  geom_sf(data = nz_coastline, fill = "grey", color = "black") +
  geom_sf(data = plot_data, aes(fill = !!pop_year_sym), color = "black") +
  scale_fill_viridis_c(option = "C", na.value = "grey90", name = "Population") +
  theme_minimal() +
  coord_sf(xlim = c(165, 180)) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(
    title = str_c(pop_year, " Population Estimates by ", str_to_title(aggregation)),
    legend.title = "Population",
    caption = "Data Sources: StatsNZ, Land Information New Zealand")

return(plot)  
  
}

a <- heat_map_fx(aggregation = "region", pop_year = "2022")
b <- heat_map_fx(aggregation = "district", pop_year = "2022")
c <- heat_map_fx(aggregation = "region", pop_year = "2018")
d <- heat_map_fx(aggregation = "district", pop_year = "2018")

patch <- (a + b) / (c + d) + 
  plot_layout(guides = "collect") & 
  theme(
    legend.position = "none",       # Remove the legend
    plot.title = element_blank(),  # Remove titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title = element_blank(),  # Remove axis titles
    plot.caption = element_blank() # Remove captions
  )  
  
patch + 
  plot_annotation(title = "Population Growth by Region and District 2018-2022",
                  caption = "Data Sources: StatsNZ, Land Information New Zealand")


