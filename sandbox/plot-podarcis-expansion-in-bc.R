
## Packages 

library(tidyverse)
library(rnaturalearth)
library(geodata)
library(elevatr)
library(sf)
library(viridis)
library(tidyterra)

## Read and process data

# read data 
podarcis_data <- read.csv("raw_data/observations-653516.csv")

# remove observations without time_observed_at and add year column
podarcis_data <- podarcis_data |>
    filter(time_observed_at != "") |>
    mutate(year = year(ymd_hms(time_observed_at))) |>
    filter(longitude < -122)

# get grid of latitudes and longitudes to extract elevation data from
lats <- seq(min(podarcis_data$latitude) - 0.1, 
             max(podarcis_data$latitude) + 0.1, 
             by = 0.1)
longs <- seq(min(podarcis_data$longitude) - 0.1, 
              max(podarcis_data$longitude) + 0.1, 
              by = 0.1)
grid <- expand.grid(x = longs, y = lats)

# get elevation raster data for area with Podarcis observations
elev_raster <- get_elev_raster(
    locations = grid,
    z = 8,
    prj = sf::st_crs("+proj=longlat +datum=WGS84")
)

# transform into terra SpatRaster
elev_raster <- terra::rast(elev_raster)

# bound raster to only include area with observations
elev_raster <- terra::crop(elev_raster, terra::ext(c(min(podarcis_data$longitude) - 0.1, 
                                         max(podarcis_data$longitude) + 0.1,
                                         min(podarcis_data$latitude) - 0.1,
                                         max(podarcis_data$latitude) + 0.1)))

# transform to data frame
elev_df <- as.data.frame(elev_raster, xy = TRUE, na.rm = TRUE)
colnames(elev_df) <- c("long", "lat", "elev")

# filter elevation data above 0 m
elev_df <- elev_df |> filter(elev > 1.5)

# transform elev_df into sf object
elev_sf <- st_as_sf(elev_df, coords = c("long", "lat"), crs = 4326)

## Plot data ----

# sort data by year latest to earliest
podarcis_data <- podarcis_data[order(-podarcis_data$year), ]

# add variables for year interval
podarcis_data <- podarcis_data |>
    mutate(year_interval = case_when(
        year <= 2005 ~ "2005",
        year > 2005 & year <= 2010 ~ "2006-2010",
        year > 2010 & year <= 2015 ~ "2011-2015",
        year > 2015 & year <= 2020 ~ "2016-2020",
        year > 2020 ~ "2021-2025"
    ))

ggplot() +
    geom_raster(data = elev_df,
                aes(x = long, y = lat, fill = elev),
                show.legend = FALSE) +
    scale_fill_hypso_c(palette = "colombia_hypso") +
    geom_point(data = podarcis_data, 
                aes(x = longitude, y = latitude, col = year_interval),
                size = 2) +
    scale_color_manual(
        values = viridis::viridis(5, option = "D"),
    ) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.ticks = element_line(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.background = element_rect(fill = "#506e78", color = NA),
        plot.title = element_text(size = 14, hjust = 0.5)
    ) +
    labs(
        title = expression(italic("Podarcis muralis")~"expansion in BC"),
        x = "Longitude",
        y = "Latitude",
        fill = "Elevation (m)",
        color = "Year"
    ) 


