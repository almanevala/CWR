
# Downloaded packages

library(tidyverse)
library(raster)
library(sf)
library(prettymapr)
library(ggspatial)
library(writexl)
library(data.table)
library(here)

# Set working directory

dir.create(here("data"), showWarnings = FALSE, recursive = TRUE)
zenodo <- "24302177"
file_names <- c("cwr_finland.csv", "cwr_norway.csv", "cwr_sweden.csv")

for (name in file_names) {
  path <- here("data", name)
  zenodo_url <- paste0("https://zenodo.org/records/", zenodo, "/files/", name)
  curl::curl_download(url = zenodo_url, destfile = path)
}

# 1. Data cleanup

norway  <- fread(here("data", "cwr_norway.csv"))
sweden  <- fread(here("data", "cwr_sweden.csv"))
finland <- fread(here("data", "cwr_finland.csv"))


# Observation dates in each data frame were renamed "date" and coordinate columns were renamed "latitude" and "longitude"

finland <- finland %>% mutate(date = eventDate) %>% mutate(latitude = decimalLatitude) %>% mutate(longitude = decimalLongitude)

# A new "name" column is created to include only the species-level name 

finland <- finland %>% mutate(name = word(as.character(scientificName), 1, 2))
norway <- norway %>% mutate(name = word(as.character(validScientificName), 1, 2))
sweden <- sweden %>% mutate(name = word(as.character(Vetenskapligt.namn), 1, 2))

# Created a list of the data frames
data_frames <- mget(c("finland", "norway", "sweden"))

# Deleted all rows with invalid (empty or NA) values in date, latitude, or longitude columns
columns <- c("latitude", "longitude", "date")

data_frames <- map(data_frames, ~ {
  .x %>%
    mutate(across(all_of(columns), ~ {
      x <- as.character(.)
      na_if(trimws(x), "")
    })) %>%
    mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    ) %>%
    drop_na(all_of(columns))
})

# Extracted only the year values from the "date" column into a "Year" column
# Created column "year" with only observation year and classified the observation "O" or "N"
# N if year >= 2000 and O if year < 2000.

data_frames <- imap(data_frames, ~ {
  a <- if (.y == "norway") 7 else 1
  b <- if (.y == "norway") 10 else 4
  .x %>%
    mutate(
      Year = substr(date, a, b),
      occurrence = if_else(as.numeric(Year) < 2000, "O", "N")
    )
})

# 2. Data filtering and analysis

# Created a list for the shapefiles of each study area

shapes <- c("norway.shp", "finland.shp", "sweden.shp")
shapes <- shapes %>% 
  set_names(tools::file_path_sans_ext(basename(.))) %>%
  map(~ st_read(file.path("shapefiles", .x)))

# Filtered the each dataset with its corresponding shapefile
# Created an ESPG:3035 projection on the data and created a 1km x 1 km grid on the map with a unique grid-id for each data point 

origin_x <- 4321000
origin_y <- 3210000
cell_size <- 1000
shp_3035 <- map(shapes, ~ st_transform(.x, 3035))

df_3035 <- imap(data_frames, ~ {
  shp = shp_3035[[.y]]
  
  points <- .x %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(3035) %>%
    st_filter(shp)
  
  coord <- st_coordinates(points)
  points %>%
    mutate(
      grid_x = floor((coord[, 1] - origin_x) / cell_size) * cell_size + origin_x,
      grid_y = floor((coord[, 2] - origin_y) / cell_size) * cell_size + origin_y,
      grid_id = paste0("X", grid_x, "_Y", grid_y)
    )
})

# Assigned a "status" value for each data point (data points are grouped by grid-id and species) 
# status == "disappeared" if all values in "occurrence" are "O"
# status == "persistent" if values in "occurrence" include both "N" and "O"
# status == "new" if all values in "occurrence" are "N"

df_3035 <- imap(df_3035, ~{
  .x %>% 
    filter(!is.na(grid_id)) %>%
    group_by(name, grid_id) %>%
    mutate(status = case_when( 
      !("N" %in% occurrence) ~ "disappeared", 
      "N" %in% occurrence & "O" %in% occurrence ~ "persistent",
      "N" %in% occurrence & !("O" %in% occurrence) ~ "new",
      TRUE ~ "other")) %>%  
    ungroup()
})

# Created plots of each study area, separating "disappeared", "new" and "persistent" observations 

status_colors = list(
  "disappeared" = "red", 
  "persistent" = "dodgerblue",
  "new" = "green"
)

plots <- imap(df_3035, ~{
  country <- .y
  pts <- .x
  shp <- shp_3035[[country]]
  shp_buffered <- st_buffer(shp, dist = 1000)  
  bbox <- st_as_sfc(st_bbox(shp_buffered))
  
  grid = st_make_grid(
    bbox, 
    cellsize = c(1000, 1000), 
    offset = c(4321000, 3210000), 
    square = TRUE
  )
  
  clipped_grid <- st_intersection(st_sf(geometry = grid, crs = 3035), bbox)
  
  map2(status_colors, names(status_colors), ~ {
    status_type <- .y
    pts_color <- .x
    pts_status <- pts %>% filter(status == status_type)
    
    ggplot() +
      annotation_map_tile(type = "osm", zoom = 12) +
      geom_sf(data = shp, fill = "gray70", color = "gray20", linewidth = 0.8, alpha = 0.5) +
      geom_sf(data = clipped_grid, fill = NA, color = "black", alpha = 0.4, linewidth = 0.15) +
      geom_sf(data = pts_status, color = pts_color, size = 0.5, alpha = 1) +
      labs(title = paste0(tools::toTitleCase(country), " - ", tools::toTitleCase(status_type))) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
      )
  }) %>%
    set_names(paste0(country, "_", names(status_colors)))
})
plots <- flatten(plots)

