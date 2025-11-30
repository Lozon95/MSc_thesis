""" This R-script reprojects WGS84 coordinates in CSV column to SWEREF99 spatial
object. It then extract observations in the CSV file inside a polygon. The
script also calculates relative humidity based on ERA5 dewpoint and temperature.
As well as converting data and constructing files to be compatible in Delft3D FM
suit."""

library(akima)
library(ggplot2)
library(sf)
library(raster)
library(dplyr)

################################################################################
################################## Read data ###################################
################################################################################
# Read CSV file for land hourly data
land_hourly_data <- read.csv("D:\\EXAMENSARBETE!!\\Thesis_data\\Pre_processing_Delft3D\\Era5_preproccess_RProj\\Era5_preprocess\\land_hourly_2023.csv")
###### ERA5 LAND HOURLY VARIABLES #####
#d2m = Dewpoint temperature
#t2m = Air temperature
#lmlt = Mix lake water temperature
#ssrd = Surface solar radiation downwards
#u10 = x-component wind
#v10 = y-component wind
#sp = Surface pressure

# Read CSV file for single hourly data (cloud coverage)
single_hourly_data <- read.csv("D:\\EXAMENSARBETE!!\\Thesis_data\\Pre_processing_Delft3D\\Era5_preproccess_RProj\\Era5_preprocess\\cloud_coverage_2023.csv")
###### ERA5 SINGLE HOURLY VARIABLES #####
#tcc = Total cloud coverage

################################################################################
################################## Reproject ###################################
################################################################################

# Convert crs to SWEREF99TM for land hourly data
points_land_hourly <- st_as_sf(land_hourly_data, coords = c("longitude", "latitude"), crs = 4326)  # WGS84
points_sweref99_land_hourly <- st_transform(points_land_hourly, crs = 3006)

# Add sweref coordinates to land hourly data
land_hourly_data$X <- st_coordinates(points_sweref99_land_hourly)[, "X"]
land_hourly_data$Y <- st_coordinates(points_sweref99_land_hourly)[, "Y"]

# Convert crs to SWEREF99TM for single hourly data
points_single_hourly <- st_as_sf(single_hourly_data, coords = c("longitude", "latitude"), crs = 4326)  # WGS84
points_sweref99_single_hourly <- st_transform(points_single_hourly, crs = 3006)

# Add sweref coordinates to single hourly data
single_hourly_data$X <- st_coordinates(points_sweref99_single_hourly)[, "X"]
single_hourly_data$Y <- st_coordinates(points_sweref99_single_hourly)[, "Y"]

################################################################################
############## Filter observations inside lake boundary ########################
################################################################################

# Read polygon
lake_polygon <- st_read("D:\\EXAMENSARBETE!!\\Thesis_data\\Hydrology\\Land_boundary\\Vattern_no_islands_final_polygon.shp")

# Filter points inside the polygon
filtered_points_land_hourly <- points_sweref99_land_hourly %>%
  filter(st_within(geometry, lake_polygon, sparse = FALSE))

# Filter points inside the polygon
filtered_points_single_hourly <- points_sweref99_single_hourly %>%
  filter(st_within(geometry, lake_polygon, sparse = FALSE))


################################################################################
######################### Check point extent against  ##########################
################################################################################
# Read bathymetry raster
raster_path <- "D:\\EXAMENSARBETE!!\\Thesis_data\\LST_depth\\Raster_representation\\vattern_raster_depth.tif"
raster_data <- raster(raster_path)

# Convert raster to raster dataframe
raster_df <- as.data.frame(rasterToPoints(raster_data), xy = TRUE)

# Plot raster and temperature coordinates
ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = vattern_raster_depth)) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_sf(data = points_sweref99_land_hourly, color = "red", size = 2) + 
  labs(title = "ERA5 variable points",
       x = "Longitud",
       y = "Latitud")

# Plot raster and temperature coordinates
ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = vattern_raster_depth)) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_sf(data = filtered_points_land_hourly, color = "red", size = 2) + 
  labs(title = "ERA5 variable points",
       x = "Longitud",
       y = "Latitud")

################################################################################
######################### Convert spatial object to DF  ########################
################################################################################
# We need coordinates in sweref for grid making, save and add to df after
# droping geometry
sweref_coordinates <- st_coordinates(filtered_points_land_hourly)
df_land_hourly <- filtered_points_land_hourly %>%
  st_drop_geometry() %>%
  mutate(X = sweref_coordinates[, 1], Y = sweref_coordinates[, 2])

sweref_coordinates <- st_coordinates(filtered_points_single_hourly)
df_single_hourly <- filtered_points_single_hourly %>%
  st_drop_geometry() %>%
  mutate(X = sweref_coordinates[, 1], Y = sweref_coordinates[, 2])


# Test for grid making of wind and air pressure
sweref_coordinates <- st_coordinates(points_sweref99_land_hourly)
df_land_hourly_without_filter <- points_sweref99_land_hourly %>%
  st_drop_geometry() %>%
  mutate(X = sweref_coordinates[, 1], Y = sweref_coordinates[, 2])

################################################################################
######################### Convert temperature from K to C ######################
################################################################################
df_land_hourly$d2m <- df_land_hourly$d2m - 273.15
df_land_hourly$t2m <- df_land_hourly$t2m - 273.15

################################################################################
###################### Calculate Saturation vapor pressure #####################
################################################################################
# Different equations if temperature is negative or positive
df_land_hourly$Sat_vap_pres <- ifelse(
  df_land_hourly$t2m < 0,
  6.11 * 10^((9.5 * df_land_hourly$t2m) / (265.5 + df_land_hourly$t2m)),
  6.11 * 10^((7.45 * df_land_hourly$t2m) / (237.3 + df_land_hourly$t2m))
)

################################################################################
######################## Calculate Actual vapor pressure #######################
################################################################################
df_land_hourly$act_vap_pres <- 6.108 * exp((17.27 * df_land_hourly$d2m)/(237.3 + df_land_hourly$d2m)) 

################################################################################
######################## Calculate Relative Humidity ###########################
################################################################################
df_land_hourly$`Humidity [%]` <- df_land_hourly$act_vap_pres / df_land_hourly$Sat_vap_pres * 100

################################################################################
##################### Convert solar radiation to W/m2 ##########################
################################################################################
df_land_hourly$`Solar radiation [W/m2]` <- df_land_hourly$ssrd/3600

################################################################################
##################### Convert cloud cover to percent ###########################
################################################################################
df_single_hourly$`Cloud coverage [%]` <- df_single_hourly$tcc * 100



################################################################################
######################## Wind and Pressure GRID creation ASCII #################
################################################################################


data <- df_land_hourly_without_filter  

# Define grid borders
x_min <- min(data$X)
x_max <- max(data$X)
y_min <- min(data$Y)
y_max <- max(data$Y)
dx <- 9000  # Gridsize in meters 
dy <- 9000

# Create grid coordinates
x_coords <- seq(x_min, x_max, by = dx)
y_coords <- seq(y_min, y_max, by = dy)

create_arcinfo_time_series_grid <- function(data, parameter, file_name, start_time, quantity1) {
  # Extract unique timestamps
  time_steps <- sort(unique(data$valid_time))
  
  # Create file and write header
  file.create(file_name)
  cat("### START OF HEADER\n### This file is created by Loa Andersson\n### Additional comments\n", file = file_name, append = TRUE)
  cat(sprintf("FileVersion     =    1.03\nfiletype        =    meteo_on_equidistant_grid\n"), file = file_name, append = TRUE)
  cat("NODATA_value    =    -9999.0\n", file = file_name, append = TRUE)
  cat(sprintf("n_cols          =    %d\nn_rows          =    %d\n", length(x_coords), length(y_coords)), file = file_name, append = TRUE)
  cat("grid_unit       =    m\n", file = file_name, append = TRUE)
  cat(sprintf("x_llcenter      =    %f\ny_llcenter      =    %f\n", x_min, y_min), file = file_name, append = TRUE)
  cat(sprintf("dx              =    %f\ndy              =    %f\n", dx, dy), file = file_name, append = TRUE)
  cat("n_quantity      =    1\n", file = file_name, append = TRUE)
  cat(sprintf("quantity1       =    %s\nunit1           =    m s-1\n", quantity1), file = file_name, append = TRUE)
  cat("### END OF HEADER\n", file = file_name, append = TRUE)
  
  # Wirte timestamps and grid values 
  for (time in time_steps) {
    # FIlter data on timestamp
    time_data <- data %>% filter(valid_time == time)
    
    # Interpolate data onto grid
    interpolated <- interp(
      x = time_data$X,
      y = time_data$Y,
      z = time_data[[parameter]],
      xo = x_coords,
      yo = y_coords,
      extrap = TRUE
    )
    
    # Write timestamps
    time_label <- as.POSIXct(time) - as.POSIXct(start_time, tz = "UTC")
    time_label <- as.numeric(difftime(as.POSIXct(time), as.POSIXct(start_time), units = "hours"))
    cat(sprintf("TIME = %d hours since %s\n", time_label, start_time), file = file_name, append = TRUE)
    
    # Write interpolated data
    write.table(interpolated$z, file = file_name, append = TRUE, row.names = FALSE, col.names = FALSE, na = "-9999.0")
  }
  
  message(sprintf("Tidsseriegrid för %s sparades i: %s", parameter, file_name))
}


# Create time series for U10
start_time <- "2023-01-01 00:00:00 +00:00"
create_arcinfo_time_series_grid(data, parameter = "u10", file_name = "time_series_grid_u10.txt", start_time, quantity1 = "x_wind")

# Repeat for V10 and air pressure
create_arcinfo_time_series_grid(data, parameter = "v10", file_name = "time_series_grid_v10.txt", start_time, quantity1 = "y_wind")
create_arcinfo_time_series_grid(data, parameter = "sp", file_name = "time_series_grid_sp.txt", start_time, quantity1 = "air_pressure")



################################################################################
##################### Aggregate values spatialy to uniform #####################
################################################################################
land_hourly_aggregated <- df_land_hourly %>%
  group_by(valid_time) %>%
  summarise(
    `Air temperature [°C]` = mean(t2m, na.rm = TRUE),
    `Wind x-component` = mean(u10, na.rm = TRUE),
    `Wind y-component` = mean(v10, na.rm = TRUE),
    `Air pressure` = mean(sp, na.rm = TRUE),
    `Humidity [%]` = mean(`Humidity [%]`, na.rm = TRUE),
    `Solar radiation [W/m2]` = mean(`Solar radiation [W/m2]`, na.rm = TRUE)
  )

single_hourly_aggregated <- df_single_hourly %>%
  group_by(valid_time) %>%
  summarise(
    `Cloud coverage [%]` = mean(`Cloud coverage [%]`, na.rm = TRUE)
  )


################################################################################
##################### Join single and land hourly data  ########################
##################### And subset meterological variable  #######################
################################################################################
# Merge data based on valid_time
Final_df <- merge(land_hourly_aggregated, single_hourly_aggregated, by = "valid_time", all = TRUE)

# Subset meterological variables
Meterological_df <- Final_df[, c("valid_time", "Humidity [%]", "Air temperature [°C]",
                                 "Cloud coverage [%]", "Solar radiation [W/m2]")]
# Rename time column
Meterological_df <- Meterological_df %>%
  rename(`Time [-]` = valid_time)

# Export meterological df to csv
write.csv(Meterological_df, "D:\\EXAMENSARBETE!!\\Thesis_data\\ERA5_DATA\\Meterological_processed\\Sep\\sep_meterological.csv", row.names = FALSE)


################################################################################
####################### Create wind time series ################################
################################################################################
# Reference time, the first observation in the time series
Final_df$valid_time <- as.POSIXct(Final_df$valid_time, tz = "UTC")
reference_time <- as.POSIXct("2023-09-01 00:00:00", tz = "UTC")

#Subset
wind_df <- Final_df[, c("valid_time", "Wind x-component", "Wind y-component")]

# calculate time in minutes from reference time
wind_df$time_in_minutes <- as.numeric(difftime(wind_df$valid_time, reference_time, units = "mins"))

# Choose columns for .wnd-file
wind_df <- wind_df[, c("time_in_minutes", "Wind x-component", "Wind y-component")]

# Write
write.csv(wind_df, "D:\\EXAMENSARBETE!!\\Thesis_data\\ERA5_DATA\\Meterological_processed\\Sep\\sep_wind.csv", row.names = FALSE, col.names = FALSE)

################################################################################
#################### Create air pressure time series ###########################
################################################################################
# Subset
pressure_df <- Final_df[, c("valid_time", "Air pressure")]

# calculate time in minutes from reference time
pressure_df$time_in_minutes <- as.numeric(difftime(pressure_df$valid_time, reference_time, units = "mins"))

# Choose columns for pressure file (I THINK THE EXTENSION IS .ap, IF ERROR: CHECK)
pressure_df <- pressure_df[, c("time_in_minutes", "Air pressure")]

# Write
write.csv(pressure_df, "D:\\EXAMENSARBETE!!\\Thesis_data\\ERA5_DATA\\Meterological_processed\\Sep\\sep_airpress.csv", row.names = FALSE, col.names = FALSE)


