# Oyster_Project
View(X2026_analysis_Shoreline_Survey_Data_Cleaned_Data)
library(readr)
X2026_analysis_Shoreline_Survey_Data_Cleaned_Data <- read_csv("Desktop/2026-analysis_Shoreline Survey Data - Cleaned Data.csv")
Clean_data <- X2026_analysis_Shoreline_Survey_Data_Cleaned_Data
summary(Clean_data)

clean_osyters <- na.omit(Clean_data$`Oysters per quad (adjusted for quad size)`)
mean(clean_osyters)

#What percentage of plots have oysters?
pct_oysters <- mean(
  +   clean_oysters$`Oysters per quad (adjusted for quad size)` > 0
  + ) * 100
pct_oysters

#Mean size of oysters
Clean_data$`SL (mm)` <- as.numeric(Clean_data$`SL (mm)`)
Mean_size <- mean(Clean_data$`SL (mm)`, na.rm = TRUE)
Mean_size

#Descritpive statstics for dead and alive oysters
alive_data <- Clean_data[
   Clean_data$`Alive (1/0)` == 1,
]
Size_alive_summary <- summary(alive_data$`SL (mm)`, na.rm = TRUE)
Size_alive_mean <- mean(alive_data$`SL (mm)`, na.rm = TRUE)
Size_alive_median <- median(alive_data$`SL (mm)`, na.rm = TRUE)
Size_alive_sd <- sd(alive_data$`SL (mm)`, na.rm = TRUE)
Size_alive_var <- var(alive_data$`SL (mm)`, na.rm = TRUE)
Size_alive_min <- min(alive_data$`SL (mm)`, na.rm = TRUE)
Size_alive_max <- max(alive_data$`SL (mm)`, na.rm = TRUE)
Size_alive_median

dead_data <- Clean_data[
   Clean_data$`Alive (1/0)` == 0,
]
Size_dead_summary <- summary(dead_data$`SL (mm)`, na.rm = TRUE)
Size_dead_mean <- mean(dead_data$`SL (mm)`, na.rm = TRUE)
Size_dead_median <- median(dead_data$`SL (mm)`, na.rm = TRUE)
Size_dead_sd <- sd(dead_data$`SL (mm)`, na.rm = TRUE)
Size_dead_var <- var(dead_data$`SL (mm)`, na.rm = TRUE)
Size_dead_min <- min(dead_data$`SL (mm)`, na.rm = TRUE)
Size_dead_max <- max(dead_data$`SL (mm)`, na.rm = TRUE)

#Samples per site
library(dplyr)
  site_counts <- clean_oysters %>% 
  count(Site)
print(site_counts)

library(dplyr)
library(lubridate)
library(stringr)
site_year_counts <- clean_oysters %>%
  mutate(
    Site = str_trim(Site),
    Year = year(mdy(Date))
    ) %>%
  filter(Year != 2027) %>% 
# Count the remaining combinations
count(Site, Year)
print(site_year_counts)

library(dplyr)
library(lubridate)
library(stringr)
install.packages("leaflet")
library(leaflet)
install.packages("geosphere")
library(geosphere)

# 1. Clean your map data (including your coordinate fix)
  cleaned_data <- clean_oysters_coordinates %>%
  mutate(
    Year = year(mdy(Date)),
    Lat_num = as.numeric(Latitude),
    Lon_num = as.numeric(Longitude)
    ) %>%
  mutate(
    Correct_Lat = ifelse(!is.na(Lat_num) & Lat_num < 0, Lon_num, Lat_num),
    Correct_Lon = ifelse(!is.na(Lat_num) & Lat_num < 0, Lat_num, Lon_num)
    ) %>%
  filter(!is.na(Correct_Lat), !is.na(Correct_Lon), Year != 2027)

# 2. Calculate distance between consecutive quadrats
shoreline_distances <- cleaned_data %>%
    # Arrange by site, year, zone, and quadrat number to ensure chronological order
    arrange(Site, Year, `Zone (mid, low)`, `Quad (#)`) %>%
    group_by(Site, Year) %>%
    mutate(
      # Calculate distance (in meters) from the PREVIOUS quadrat row
        dist_from_prev = distHaversine(
          cbind(lag(Correct_Lon), lag(Correct_Lat)), 
          cbind(Correct_Lon, Correct_Lat)
          )
      ) %>%
    # 3. Sum the distances up for each site and year
    summarise(
      Total_Distance_Meters = sum(dist_from_prev, na.rm = TRUE),
      Total_Distance_Miles = Total_Distance_Meters * 0.000621371, # Convert to miles
      Total_Quadrats = n(),
      .groups = "drop"
      )
print(shoreline_distances)

View(X2026_analysis_Shoreline_Survey_Data_Cleaned_Data)
clean_oysters_coordinates <- clean_oysters[
  !is.na(clean_oysters$'Latitude'),
  ]

library(dplyr)
clean_oysters_coordinates <- clean_oysters_coordinates %>% 
  filter(`Unique Quad (#)` != "DM-26-M-1")
clean_oysters_coordinates <- clean_oysters_coordinates %>% 
  filter(`Unique Quad (#)` != "DR-23-L-23")

# 1. Clean the dataset (fix flipped coordinates and extract Year)
  map_data <- clean_oysters_coordinates %>%
  mutate(
    Year = year(mdy(Date)),
    Lat_num = as.numeric(Latitude), na.rm = "TRUE",
    Lon_num = as.numeric(Longitude), na.rm = "TRUE"
    ) %>%
# AUTOMATIC FIX: If Latitude is negative, flip Lat and Lon back to normal
  mutate(
    Correct_Lat = ifelse(!is.na(Lat_num) & Lat_num < 0, Lon_num, Lat_num),
    Correct_Lon = ifelse(!is.na(Lat_num) & Lat_num < 0, Lat_num, Lon_num)
    ) %>%
#Remove rows missing coordinates or from the year 2027
  filter(!is.na(Correct_Lat), !is.na(Correct_Lon), Year != 2027)
  # 2. Set up color coding for each survey year
  year_colors <- colorFactor(palette = "viridis", domain = map_data$Year)
  # 3. Generate the interactive map
    leaflet(map_data) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>% # Adds real satellite imagery
    addCircleMarkers(
      lng = ~Correct_Lon, 
      lat = ~Correct_Lat, 
      color = ~year_colors(Year),
      radius = 4, 
      stroke = FALSE, 
      fillOpacity = 0.8,
      popup = ~paste("Site:", Site, "<br>Year:", Year) # Click point to see info
      ) %>%
    addLegend(
      pal = year_colors, 
      values = ~Year, 
      title = "Year Surveyed", 
      position = "bottomright"
      )

# Make sure to install and load the ggspatial package
install.packages("ggspatial")
install.packages(c("prettymapr", "rosm"))
library(prettymapr)
library(rosm)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

map_data <- clean_oysters %>%
  filter(!`Unique Quad (#)` %in% c("DM-26-M-1", "DR-23-L-23")) %>%
  mutate(
    Year = year(mdy(Date)),
    Lat_num = as.numeric(Latitude),
    Lon_num = as.numeric(Longitude)
    ) %>%
  mutate(
    Correct_Lat = ifelse(!is.na(Lat_num) & Lat_num < 0, Lon_num, Lat_num),
    Correct_Lon = ifelse(!is.na(Lat_num) & Lat_num < 0, Lat_num, Lon_num)
    ) %>%
  filter(!is.na(Correct_Lat), !is.na(Correct_Lon), Year != 2027)
# 2. Plot the shoreline shape faceted by Year over a map
ggplot(map_data) +
  # STEP A: Add the background map tiles underneath the data
  # Options for type include: "cartolight" (clean & minimal), "osm" (standard OpenStreetMap), or "hotstyle"
  annotation_map_tile(type = "cartolight", zoom = 14, progress = "none") + 
  # STEP B: Draw your dots over the map
  # Note: Moved aes() inside geom_point so it doesn't conflict with the map layer
  geom_point(aes(x = Correct_Lon, y = Correct_Lat, color = Site), alpha = 0.8, size = 1.2) +
  # STEP C: Force R to recognize the coordinates as actual GPS data (WGS84)
  coord_sf(crs = 4326) + 
  facet_wrap(~Year, ncol = 2) + 
  theme_minimal() +
  labs(
    title = "Shoreline Survey Spatial Coverage (Year-by-Year)",
    subtitle = "Points trace out the surveyed footprints of each site over time",
    x = "Longitude",
    y = "Latitude",
    color = "Site Location"
    ) +
  theme(
    panel.border = element_rect(color = "grey80", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
    )

library(dplyr)
library(lubridate)
# Ensure Year is available for grouping
  oyster_prep <- clean_oysters %>%
  mutate(Year = year(mdy(Date)))
# --- A. Combined Across All Sites by Year ---
  avg_density_by_year <- oyster_prep %>%
  group_by(Year) %>%
  summarize(
    Avg_Oysters_Per_Quad = mean(`Oysters per quad (adjusted for quad size)`, na.na = TRUE),
    Total_Quads_Sampled = n()
    )
avg_density_by_year
# --- B. Broken Down by Site and Year ---

avg_density_by_site_year <- oyster_prep %>%
  filter(Year != 2027) %>% 
  group_by(Site, Year) %>%
  summarize(
    Avg_Oysters_Per_Quad = mean(`Oysters per quad (adjusted for quad size)`, na.rm = TRUE),
    Total_Quads_Sampled = n()
    ) %>%
  arrange(Site, Year)
avg_density_by_site_year

avg_density_by_species <- oyster_prep %>%
  group_by(Site, Year) %>%
  summarize(
    Avg_American_Oyster = mean(`American Osyter (1/0)`, na.rm = TRUE),
    Avg_European_Oyster = mean(`European Oyster (1/0)`, na.rm = TRUE),
    .groups = "drop"
    )
avg_density_by_species

#Alive vs. Dead
avg_alive_vs_dead <- oyster_prep %>%
  # Filter to rows where alive/dead status was actually recorded
  filter(!is.na(`Alive (1/0)`)) %>% 
  group_by(Site, Year) %>%
  summarize(
    Total_Evaluated = n(),
    Proportion_Alive = mean(`Alive (1/0)`, na.rm = TRUE),
    Proportion_Dead = 1 - mean(`Alive (1/0)`, na.rm = TRUE),
    .groups = "drop"
    )
avg_alive_vs_dead

library(ggplot2)
library(dplyr)
library(lubridate)
# Filter and clean rows specifically containing size data
size_data <- clean_oysters %>%
  mutate(
    Year = year(mdy(Date)),
    SL_num = as.numeric(`SL (mm)`),
    Status = ifelse(`Alive (1/0)` == 1, "Alive", "Dead")
    ) %>%
  # Keep only valid measurements and remove the 2027 entry
  filter(!is.na(SL_num), !is.na(Status), Year != 2027)
size_data

ggplot(size_data, aes(x = SL_num)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white", alpha = 0.8) +
  facet_wrap(~Year, ncol = 2) + 
  theme_minimal() +
  labs(
    title = "Overall Oyster Size Frequency Distribution by Year",
    x = "Shell Length (SL, mm)",
    y = "Count of Oysters"
    )
#by year and site
ggplot(size_data, aes(x = SL_num)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white", alpha = 0.8) +
  facet_grid(Site ~ Year) + 
  theme_minimal() +
  labs(
    title = "Overall Oyster Size Frequency Distribution by Year",
    x = "Shell Length (SL, mm)",
    y = "Count of Oysters"
    )
#Dead Oysters — Do they all die at the same size?
ggplot(filter(size_data, Status == "Dead"), aes(x = SL_num)) +
  geom_histogram(binwidth = 10, fill = "tomato", color = "white", alpha = 0.8) +
  facet_wrap(~Year, ncol = 2) + 
  theme_minimal() +
  labs(
    title = "Size Frequency Distribution of Dead Oysters",
    subtitle = "Evaluating if mortality is restricted to a specific size class",
    x = "Shell Length (SL, mm)",
    y = "Count of Dead Oysters"
    )
#Are oysters are large enough for reproduction
ggplot(filter(size_data, Status == "Alive"), aes(x = SL_num)) +
  geom_histogram(binwidth = 10, fill = "darkolivegreen4", color = "white", alpha = 0.8) +
  facet_wrap(~Year, ncol = 2) + 
  # Add visual reference line for minimum reproductive size
  geom_vline(xintercept = 35, linetype = "dashed", color = "red", size = 0.8) +
  theme_minimal() +
  labs(
    title = "Size Frequency Distribution of Live Oysters",
    subtitle = "Dashed red line indicates estimated minimum size at sexual maturity (35 mm)",
    x = "Shell Length (SL, mm)",
    y = "Count of Live Oysters"
    )
#Alive vs. Dead plot
ggplot(size_data, aes(x = SL_num, fill = Status)) +
  geom_histogram(binwidth = 10, color = "white", alpha = 0.6, position = "identity") +
  facet_wrap(~Year, ncol = 2) +
  scale_fill_manual(values = c("Alive" = "darkolivegreen4", "Dead" = "tomato")) +
  geom_vline(xintercept = 35, linetype = "longdash", color = "grey30") +
  theme_minimal() +
  labs(
    title = "Oyster Size Distribution: Alive vs. Dead Cohorts",
    x = "Shell Length (SL, mm)",
    y = "Count of Oysters",
    fill = "Condition Status"
    )
