################################################################################
################################# Install packages #############################
# Install packages 
install.packages("data.table")
install.packages("lubridate")
install.packages("sf")
install.packages("raster")
install.packages("dplyr")

# Load packages
library(ggplot2)
library(sf)
library(data.table)
library(lubridate)
library(raster)
library(dplyr)

###############################################################################
######################### Clean receiver info data ############################
#                               Receivers_2023
###############################################################################

# Extract reciever ID and skip reciever modell name. Rename. 
Receivers_2023 <- Receivers_2023 %>%
  mutate(receiver_name = sub(".*-(.*)", "\\1", receiver_name))%>%
  rename(receiver_id = receiver_name)



###############################################################################
######################### Clean receiver temp data ############################
#                             temp_2023_receiver
###############################################################################
""" The temperature for each reciever is registred every hour. However, depth
and temperature can sometimes be missing during registration. The reciever can
also detach from the bottom and float to surface or drift in the water column.
In those cases the tilt value and depth value will be skewed. A tilt value 
above 50 strongly indicates a detached reciever. In the reciever documentation data
no recievers is placed shallower than 9m. These errors need to be removed. """

# Start with renaming the reciever ID so it has the same name as Receiver_2023 
temp_2023_receiver <- temp_2023_receiver %>%
  rename(receiver_id = `Serial Number`)

# temp_2023_receiver is missing receiver coordinates. Add these by matching the
# receiver_id between the datasets and add coordinates from Receivers_2023
temp_2023_receiver$latitude <- Receivers_2023$station_latitude[match(temp_2023_receiver$receiver_id, Receivers_2023$receiver_id)]
temp_2023_receiver$longitude <- Receivers_2023$station_longitude[match(temp_2023_receiver$receiver_id, Receivers_2023$receiver_id)]

# Some depth values are NA in the reciever temperature data. However, we have the
# serial number for the reciever and another dataset which states the depth for 
# each reciever. Fetch the depth value for the reciever based on the receiver ID
# and add to reciever temp data where depth value is NA.
for(i in 1:nrow(temp_2023_receiver)) {
  # Check if depth is NA 
  if(is.na(temp_2023_receiver$`Depth (m)`[i])) {
    # Fetch the reciever ID
    receiver_id <- temp_2023_receiver$receiver_id[i]
    # Find corresponding row in the reciever data where serial_number match receiver_id
    matching_row <- Receivers_2023[Receivers_2023$serial_number == receiver_id, ]
    # Update if matching row is found 
    if(nrow(matching_row) == 1) {
      temp_2023_receiver$`Depth (m)`[i] <- matching_row$depth
    }
  }
}

# Some detections will still be missing depth, and also temperature. Remove.
# Depth
temp_2023_receiver <- temp_2023_receiver %>%
  filter(!is.na(`Depth (m)`))
# Temp
temp_2023_receiver <- temp_2023_receiver %>%
  filter(!is.na(`Ambient Temperature (deg C)`))
# No reciever is placed shallower than 9m, remove all observations above 9m depth
temp_2023_receiver <- temp_2023_receiver %>%
  filter(`Depth (m)` > 8)
# If the tilt value is above 50, the receiver have got loose, remove.
temp_2023_receiver <- temp_2023_receiver %>%
  filter(`Tilt (deg)` <= 50)

# Also set uniform date format to avoid conflict with other datasets
temp_2023_receiver$`Device Time (UTC)` <- as.POSIXct(temp_2023_receiver$`Device Time (UTC)`)

# Lastly, subset the reciever temperature dataset to uniform column names which
# will be followed for transmitter dataset and remove unnecessary columns. 
temp_2023_receiver <- temp_2023_receiver %>%
  select(`Device Time (UTC)`, `latitude`, `longitude`, `receiver_id`, `Ambient Temperature (deg C)`, `Depth (m)`) %>%
  rename(detection_timestamp = `Device Time (UTC)`,
         temp = `Ambient Temperature (deg C)`,
         depth = `Depth (m)`)



###############################################################################
######################## Clean transmitter temp data ##########################
#                                  temp_2023
###############################################################################
# The reciever_id have to be the same for all dataframes in order for joines. 
# Remove the reciever modell in the reciever name in the transmitter temperature
# data since the reciever temperature data does not include this in the reciever
# ID. 
temp_2023 <- temp_2023 %>%
  mutate(receiver_name = sub(".*-(.*)", "\\1", receiver_name))

# Subset and remove columns we do not need. Rename receiver ID column to
# uniform value. 
temp_2023 <- temp_2023 %>%
  select(`detection_timestamp`, `latitude`, `longitude`,`receiver_name`,
         `Sensor_type`, `Transformed_sensor_value`, `animal_id` ) %>%
  rename(receiver_id = receiver_name)


################################################################################
############### Create DF with depth representative temperatures ###############
################################################################################
""" Depth and temperature is registred at different timestamps in the transmitter 
data. They need to be paired based on the difference in timestamp. Create two
subsets, one for pressure (Depth) and one for temperature. This is to split the
Transformed sensor value to separate dataframes for easier processing. A one to many
join is then computed for these two based on animal_id, receiver_id, year,
month and day with temperature being the first join dataframe. Since there is more
than one observation each day there will be joins with many depht observations for
one temperature observation (all depth observations for the same temp observation
hour). The result is a very large dataframe with much redundancy. However, in this
redundancy we can filter observations by comparing the timestamp for depth and temp
observations. In the first selection we set up a criterion that a temperature and 
a depth observation cant have a timedistance larger than 10min for the temperature
to be representative for the depth (since the fish can move from the location
of temperature measurement."""

# Split DF into two, one for temperature and one for depth
df_T <- subset(temp_2023, Sensor_type == "T")
df_P <- subset(temp_2023, Sensor_type == "P")

# Remove unecessary columns and rename the sensor value
df_T <- df_T[, c("detection_timestamp",
                 "latitude",
                 "longitude",
                 "Transformed_sensor_value",
                 "animal_id",
                 "receiver_id")]
colnames(df_T)[colnames(df_T) == "Transformed_sensor_value"] <- "temp"

df_P <- df_P[, c("detection_timestamp",
                 "latitude",
                 "longitude",
                 "Transformed_sensor_value",
                 "animal_id",
                 "receiver_id")]
colnames(df_P)[colnames(df_P) == "Transformed_sensor_value"] <- "depth"

# Convert time format to the same as for temp_2023_receiver
df_P$detection_timestamp <- as.POSIXct(df_P$detection_timestamp)
df_T$detection_timestamp <- as.POSIXct(df_T$detection_timestamp)

# Split date so each time metric gets own column
df_T <- df_T %>%
  mutate(year = as.integer(format(detection_timestamp, "%Y")),
         month = as.integer(format(detection_timestamp, "%m")),
         day = as.integer(format(detection_timestamp, "%d")))

df_P <- df_P %>%
  mutate(year = as.integer(format(detection_timestamp, "%Y")),
         month = as.integer(format(detection_timestamp, "%m")),
         day = as.integer(format(detection_timestamp, "%d")))

# Create a DF with both temp and corresponding timestamp and depth and corresponding timestamp
TP_df <- df_T %>%
  inner_join(df_P, by = c("animal_id", "receiver_id", "year", "month", "day"))

# Function to determine the time difference between observed temp and observed depth
check_minute_difference <- function(timestamp_x, timestamp_y) {
  # Calculate difference in minutes
  diff_minutes <- abs(as.numeric(difftime(timestamp_x, timestamp_y, units = "mins")))
  # If difference is smaller or equal to 10, return True
  if (diff_minutes <= 10) {
    return(TRUE)
  }
  return(FALSE)
}

# The TP_df have over 26 million datapoints. Convert to datatable (data.table) in
# order to use indexing and thus speeding up process. 
setDT(TP_df)

# Calculate time difference with function and save result in new column
TP_df[, result := abs(as.numeric(difftime(detection_timestamp.x, detection_timestamp.y, units = "mins"))) <= 10]

# Subset the datatable to only include time differences of 10min
TP_df <- subset(TP_df, result == TRUE)


################################################################################
############### Create DF with time representative temperatures ###############
###############################################################################
""" In this second filtering we need to make sure that the fish have been located
on a certain depth for long enough so the temperature registration is representative
for the surrounding water. If the fish moves the temperature will be representative
for the last location of the fish due to the delay in temperature measurement in
the fish abdomnen. A scientific study used the thresholds 0-2m for minimum 20min
in order for the temperature to be accuracy. However, during winter time in Lake
Vättern we can assume a dissolved temperature troughout the depth column and a
longer depth intervall and shorter time intervall can be used. We achieve the
filtering by creating a ID for each continuous time series of detection and
then checks the biggest time difference between all combinations of temp
timestamp and depth timestamp inside each contionous time serie is above the
threshold."""
# Group each depth/temp timestamp series with a unique ID 
# Since we joined one to many for the inner_join timestamp.x will be the same
# for many timestamp.y. With that information we can group each sequence of
# detections with unique IDs. 
TP_df <- TP_df %>%
  group_by(animal_id, receiver_id, detection_timestamp.x) %>%
  mutate(temp_depth_id = cur_group_id())

# Check if the biggest time difference between any detection time of timestamp.x
# and timestamp.y is bigger than 20min. Keep only if bigger than 20min during
# summer months and bigger than 5min in winter months
final_df <- TP_df %>%
  group_by(animal_id, receiver_id, temp_depth_id) %>%
  filter(any(
    (month %in% c("5", "6", "7", "8", "9") & abs(difftime(detection_timestamp.x, detection_timestamp.y, units = "mins")) >= 20) |
      (month %in% c("1", "2", "3", "4", "10", "11", "12") & abs(difftime(detection_timestamp.x, detection_timestamp.y, units = "mins")) >= 5)
  )) %>%
  ungroup()

# Do the same filtering on the depth value. Must be under 2m for summer months
# and under 10m for winter months. 
final_df <- final_df %>%
  group_by(animal_id, receiver_id, temp_depth_id) %>%
  filter(any(
    (month %in% c(5, 6, 7, 8, 9) & (max(depth) - min(depth)) <= 2) |
      (month %in% c(1, 2, 3, 4, 10, 11, 12) & (max(depth) - min(depth)) <= 10)
  )) %>%
  ungroup()

# If there is only one observation, we cant not say for sure that the fish have
# been on the same depth for the time 20 or 5min. Therefore we must remove all
# observations that is not part of a series of observations. There must be a 
# minimum of two observations for us to be certain about that the fish have
# been at the same depth for enough time. 
final_df <- final_df %>%
  group_by(temp_depth_id) %>%
  filter(n() > 1) %>%
  ungroup()

# Since we did the inital join on one to many on the temperature dataframe (df_T)
# we can filter out the latest detection for every time series ID. 
# This is becouse all these have the same temperature 
final_df <- final_df %>%
  group_by(temp_depth_id) %>%
  filter(detection_timestamp.y == max(detection_timestamp.y)) %>%
  ungroup()

# Now we can say for certain that all observations are representative and correct.
# Remove unnecesary columns and prepare to join transmitter temp data with reciever
# temp data. 
final_df <- as_tibble(final_df)
final_df <- final_df %>%
  select(`detection_timestamp.x`, `latitude.x`, `longitude.x`,`depth`,
         `temp`, `animal_id`, `receiver_id`) %>%
  rename(detection_timestamp = detection_timestamp.x,
         latitude = latitude.x,
         longitude = longitude.x)

################################################################################
############### Merge reciever temperature with transmitters ###################
###############################################################################
""" The receivers take hourly temperature measurements. These are valuable for
the dataset since they are many. During the pre-processing step we prepared
the datasets so the column names and contents is uniform. Therefore we should now
be able to do a simple rbind() on the receiver data and the transmitter data"""
# Add column for animal_id in receiver data to avoid number of column conflict
temp_2023_receiver$animal_id <- NA
# Combine transmitter and receiver observations
final_df <- rbind(final_df, temp_2023_receiver)

################################################################################
###################### Filter data based on date ###############################
################################################################################
# Date and timestamp to subset on. 
subset_timestamp <- as.POSIXct("2023-02-25 01", format = "%Y-%m-%d %H")

#Subset
date_subset <- final_df %>% filter(format(detection_timestamp, "%Y-%m-%d %H") == format(subset_timestamp, "%Y-%m-%d %H"))

################################################################################
######################### Address depth issue ##################################
###############################################################################
""" A problem we need to solve is when a transmitter temperature is registred 
deeper than the receiver. Since the only coordinates we have is the receiver
the transmitter temp and depth will be located on this position. However, the fish 
can be deeper somewhere around the reciever in a 2km buffer. This means that i delft3D
points will be located below the seabed which will result in modelling conflicts.
We should be able to solve this by loading the depht data in raster format and 
compare the temperature depth with the cell value (depth). If the temperature
depth is deeper than the bathymetry depth, we move the point to the nearest cell 
where bathymetry depth is larger than the temperature depth by switching the 
coordinates to this cell."""

# First we need to convert the coordinates from the receiver from WGS84 to
# SWEREF99TM as this is the CRS used for all other data during this project.
# Create spatial objects
sf_df <- st_as_sf(date_subset, coords = c("longitude", "latitude"), crs = 4326) # WGS84

# Convert
sf_df_sweref99tm <- st_transform(sf_df, crs = 3006)

# Extract the SWEREF99TM coordinates and ad to the temperature dataframe again.
# st_coordinates() ensures a top down process between the transformed DF and the
# extracted coordinates so correct coordinates gets assigned on correct row. 
date_subset$x <- st_coordinates(sf_df_sweref99tm)[,1]
date_subset$y <- st_coordinates(sf_df_sweref99tm)[,2]

# Read bathymetry raster
raster_path <- "D:\\EXAMENSARBETE!!\\Thesis_data\\LST_depth\\Raster_representation\\vattern_raster_depth.tif"
raster_data <- raster(raster_path)


# Convert raster to raster dataframe
raster_df <- as.data.frame(rasterToPoints(raster_data), xy = TRUE)

# Plot raster and temperature coordinates
ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = vattern_raster_depth)) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_sf(data = sf_df_sweref99tm, color = "red", size = 2) + 
  labs(title = "Reciever detections 2023",
       x = "Longitud",
       y = "Latitud")



















################################################################################
######################### FUNKAR NÄSTA, SPARA IFALL DEN BEHÖVS! ############
###############################################################################

# Skapa ett SpatialPointsDataFrame från final_df
coordinates(date_subset) <- ~x+y

# Extract cell values from the raster dataset where a temperature point is located
date_subset$extracted_depth <- extract(raster_data, date_subset)

# Calculate the difference between the temp depth and the bathymetry depth
date_subset$difference <- date_subset$depth - date_subset$extracted_depth

# Move points to nearest cell with higher depthvalue if diff is larger than 0
# Loop trough and check every point in the temp data
for (i in 1:nrow(date_subset)) {
  print(paste("Processing point:", i))  
  if (date_subset$difference[i] > 0) {
    # In that case, save the variables of the point 
    current_point <- date_subset[i, ]
    print(current_point)  # Debug statement
    
    # Create a que of the raster cells and make sure the cell that corresponds
    # to the temperature points i located first in the que. 
    queue <- list(as(raster_data, "SpatialPoints")[i,])
    # Create a vector with the same length as the raster and mark all cells as
    # not visited
    visited <- rep(FALSE, ncell(raster_data))
    # Mark the cell that coresponds with the point as visited
    visited[cellFromXY(raster_data, coordinates(current_point))] <- TRUE
    # Create a flag that will be changed when a raster cell with higher depth
    # have been found. 
    found <- FALSE
    
    # As long as the que are not empty and the found variable is FALSE,
    # keep the while loop running. 
    while (length(queue) > 0 && !found) {
      # For each iteration, remove the first point in the que and save it in variable
      point <- queue[[1]]
      queue <- queue[-1]
      # Convert coordinates for the point to the cell index in raster data
      cell <- cellFromXY(raster_data, coordinates(point))
      # Check if the depth for the cell is larger than the point
      if (!is.na(raster_data[cell]) && raster_data[cell] > current_point$depth) {
        # If it is larger, save the coordinates and change variable to found
        nearest_coords <- coordinates(point)
        found <- TRUE
      } else {
        # Retrieve the eight neighbouring cells
        adj_cells <- adjacent(raster_data, cells = cell, directions = 8, pairs = FALSE)
        # Filter out cells that already have been visited
        adj_cells <- adj_cells[!visited[adj_cells]]
        # Mark all the neighbouring cells as visited
        visited[adj_cells] <- TRUE
        # Loop trough the neighbouring cells
        for (adj_cell in adj_cells) {
          # Convert cell to spatial point
          adj_point <- as(raster_data, "SpatialPoints")[adj_cell,]
          # Add the points to the que
          queue <- append(queue, list(adj_point))
        }
      }
    }
    # If a cell with larger depth is found, change the coordinates for the point
    if (found) {
      date_subset[i, c("x", "y")] <- nearest_coords
    } else {
      print(paste("No higher depth found for point:", i))
    }
  }
}
# Spara den modifierade final_df data frame
# write.csv(final_df, "din_fil_väg/final_df_modified.csv")





