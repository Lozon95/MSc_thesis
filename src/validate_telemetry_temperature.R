################################################################################
################################# Install packages #############################
# Install packages 
install.packages("data.table")
install.packages("lubridate")
install.packages("sf")
install.packages("raster")
install.packages("dplyr")
install.packages("plotly")

# Load packages
library(ggplot2)
library(scales)
library(sf)
library(data.table)
library(lubridate)
library(raster)
library(plotly)
library(tidyr)
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



################################################################################
############### Create unique ID for continous depth observations ##############
################################################################################
""" By sorting the depth dataset based on animal and detection time, we can check
with a for-loop if the values for a new row in the iteration deviates from the
precious row. That approach gives us the ability to assign uniqe IDs to observations
where a fish at a reciever have continous time detections and does not change depth.
This is important becouse we need to check if a fish is static at a depth in order
to trust the temperature measruements."""

# Sort df based on animal_id and detection timestamp
df_P <- df_P[order(df_P$animal_id, df_P$detection_timestamp), ]

# Add a column for new IDs
df_P$new_id <- NA
current_id <- 1

# Iterate trough the rows
for (i in 1:nrow(df_P)) {
  if (i == 1) {
    # Assign first row the ID
    df_P$new_id[i] <- current_id
  } else {
    # Compare current row with previous row 
    if (df_P$animal_id[i] != df_P$animal_id[i-1] || 
        df_P$receiver_id[i] != df_P$receiver_id[i-1] || 
        df_P$depth[i] != df_P$depth[i-1] ||
        # The time difference between two observations are not allowed to be above 10min
        # If the fish have been at a different depth during this 10min gap, the temperature
        # have not stagnated at the new depth before the fish swims back. 
        as.numeric(difftime(df_P$detection_timestamp[i], df_P$detection_timestamp[i-1], units = "mins")) >= 10) {
      # If any of values is not equal to previous, set new ID
      current_id <- current_id + 1
    }
    # Otherwise keep giving the observations the same ID. 
    df_P$new_id[i] <- current_id
  }
}


# Check so the observation duration is above one hour for the same observation ID
id_duration <- df_P %>%
  group_by(new_id) %>%
  summarize(
    start_time = min(detection_timestamp),
    end_time = max(detection_timestamp),
    duration = difftime(end_time, start_time, units = "hours")
  ) %>%
  filter(duration > 1)

# Filter observation IDs for contionous observations that does not exceed one hour
df_P_filtered <- df_P %>%
  filter(new_id %in% id_duration$new_id)


################################################################################
############### Pair temperature detections with depth detections ##############
################################################################################
""" We need to find a temperature registration somewhat at the same time as the 
depth was registred. Use +-5min intervall to find a temperature to pair the depth
with on animalID and receiverID."""
# Convert df to data.tables for faster indexing
setDT(df_P_filtered)
setDT(df_T)

# Add columns that buffers the detection time with +- 5min
# This is becouse depth and temp is not registred at the same time, we need to
# pair a representative temperature to the depth by checking that the temp is
# not registred to long from depth registration (+-5min)
df_T[, detection_timestamp_lower := detection_timestamp - minutes(5)]
df_T[, detection_timestamp_upper := detection_timestamp + minutes(5)]

# Pair the temperature with the depth if found, otherwise the value will be NA
# if all temperature detections is to far from the depth detection time vise
df_P_filtered[df_T, 
              on = .(animal_id, receiver_id, 
                     detection_timestamp >= detection_timestamp_lower, 
                     detection_timestamp <= detection_timestamp_upper), 
              temp_from_T := i.temp, by = .EACHI]

# Remove all rows where a temperature could not be found
df_P_filtered <- df_P_filtered %>%
  filter(!is.na(temp_from_T))%>%
  # Round the depth and temp to one decimal
  mutate(temp_from_T = round(temp_from_T, 1)) %>%
  mutate(depth = round(depth, 1))



################################################################################
############### Check a fish sample temperature against depth time #############
################################################################################
"""We need to check when the temperature stagnates when a fish moves to a new depth
since there is a delay in the temperature measurement. The code below can 
establish 1hour as depth duration as used above."""

# Filter out an individual
individual <- df_P_filtered %>%
  filter(
    month %in% c(7) &
    day %in% c(22,23) &
    animal_id == "Char_1395039" &
    receiver_id == "551112") 

# Create a long dataframe for better plotting
individual <- individual %>%
  pivot_longer(cols = c(temp_from_T, depth), names_to = "variable", values_to = "value")

# Plot with facet wrap to visualize depth and temp simultaniosly 
ggplot(individual, aes(x = detection_timestamp, y = value)) +
  geom_line() +
  scale_x_datetime(date_labels = "%D %H:%M") +
  labs(
    title = "Temperature and Depth Change for Char_1395039",
    x = "Timestamp",
    y = "Value"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~variable, scales = "free_y", ncol = 1)

################################################################################
############## Validate temperature measurements with proof temp #############
###############################################################################
""" Since the temperatures registred in the fish transmitter is somewhat not reliable
becouse of the delay in detection registration, we need to check it against validated
temperatures. SLU have validated temperatures via the hydroacoustics they measure
at three locations each year. The coordinates is given in degree minutes and secons
in WGS84 and all other data in this script uses decimal degrees in WGS84. A function
to convert the DMS in the format: (example: 58.179154, originally 58``17.9154) 
to decimal degrees is used."""

# Convert DSM in DD format to real DD
dms_to_dd <- function(df, column_name) {
  # Index the column with the coordinates
  df[[column_name]] <- sapply(as.character(df[[column_name]]), function(x) {
    # Split the DSM coordinate in DD format ("58" "179154")
    parts <- strsplit(x, "\\.")[[1]]
    # Convert degrees to numeric
    degrees <- as.numeric(parts[1])
    # Ad a decimal two numbers in in the seconds and minute (179154 to 17.9154)
    decimal_part <- substr(parts[2], 1, 2)
    remaining_part <- substr(parts[2], 3, nchar(parts[2]))
    new_value <- paste0(decimal_part, ".", remaining_part)
    # Convert minutes and seconds to degrees and paste the two values
    converted_value <- degrees + (as.numeric(new_value) / 60)
    return(converted_value)
  })
  return(df)
}

# Convert hydroacustic DSM to DD
Hydroakustik_temp_2023_PROCESSED <- dms_to_dd(Hydroakustik_temp_2023_PROCESSED, "latitude")
Hydroakustik_temp_2023_PROCESSED <- dms_to_dd(Hydroakustik_temp_2023_PROCESSED, "longitude")

# First we need to convert the coordinates from the receiver from WGS84 to
# SWEREF99TM as this is the CRS used for all other data during this project.
# Create spatial objects
sf_df <- st_as_sf(Receivers_2023, coords = c("station_longitude", "station_latitude"), crs = 4326) # WGS84
# Convert
sf_df_sweref99tm <- st_transform(sf_df, crs = 3006)

# Convert also the hydroacoustic coordinates which is now DD
sf_df_2 <- st_as_sf(Hydroakustik_temp_2023_PROCESSED, coords = c("longitude", "latitude"), crs = 4326) # WGS84
# Convert
proof_temp <- st_transform(sf_df_2, crs = 3006)

# Read bathymetry raster
raster_path <- "D:\\EXAMENSARBETE!!\\Thesis_data\\LST_depth\\Raster_representation\\vattern_raster_depth.tif"
raster_data <- raster(raster_path)

# Convert raster to raster dataframe
raster_df <- as.data.frame(rasterToPoints(raster_data), xy = TRUE)

# Plot depth data, reciever position and hydroacoustic position
p <- ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = vattern_raster_depth)) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_sf(data = sf_df_sweref99tm, color = "red", size = 2) + 
  geom_sf(data = sf_df_2, color = "green", size = 2) +
  geom_sf_text(data = sf_df_2, aes(label = hydroacoustic_id),
               color = "black", size = 3, nudge_y = 0.01) +
  geom_sf_text(data = sf_df_sweref99tm, aes(label = receiver_id),
               color = "black", size = 3, nudge_y = -0.01) +
  labs(title = "Receiver Detections 2023",
       x = "Longitud",
       y = "Latitud")

# Create an interactive plot in order to zoom
p_interactive <- ggplotly(p)

# show interactive plot
p_interactive

# NOTERA!!!! HYDROAOUSTIC_ID = 2 KORRESPONDERAR VÄL MED RECEIVER_ID 551102
# 1 OCH RECEIVER_ID 487335, 487344, 487350, 487357
################################################################################
################# Convert transmitter coordinates to SWEREF99TM ################
################################################################################

# Extract the SWEREF99TM coordinates and ad to the temperature dataframe again.
# st_coordinates() ensures a top down process between the transformed DF and the
# extracted coordinates so correct coordinates gets assigned on correct row. 
Receivers_2023$x <- st_coordinates(sf_df_sweref99tm)[,1]
Receivers_2023$y <- st_coordinates(sf_df_sweref99tm)[,2]
