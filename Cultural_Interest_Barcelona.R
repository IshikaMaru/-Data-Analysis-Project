# Load required libraries
library(dplyr)
library(leaflet)

# Load the dataset (replace 'file_path' with your dataset path)
file_path <- "opendatabcn_pics-csv.csv"
data <- read.csv(file_path, fileEncoding = "UTF-16", stringsAsFactors = FALSE)

# View the structure of the dataset
str(data)

# Step 1: Select relevant columns
# Choosing columns that might be relevant for analysis
cleaned_data <- data %>%
  select(name, geo_epgs_4326_lat, geo_epgs_4326_lon, values_category, values_value) 

# Step 2: Rename columns for readability
cleaned_data <- cleaned_data %>%
  rename(
    location_name = name,
    latitude = geo_epgs_4326_lat,
    longitude = geo_epgs_4326_lon,
    category = values_category,
    details = values_value
  )

# Step 3: Remove rows with missing latitude or longitude
cleaned_data <- cleaned_data %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Step 4: Check for duplicates and remove if necessary
cleaned_data <- cleaned_data %>%
  distinct()

# Step 5: Inspect for missing values in other columns
missing_summary <- colSums(is.na(cleaned_data))
print("Missing Values Summary:")
print(missing_summary)

# Step 6: Handle missing values in non-critical columns
# (e.g., Replace missing details with "Not Available")
cleaned_data <- cleaned_data %>%
  mutate(
    details = ifelse(is.na(details), "Not Available", details)
  )

# Create a basic leaflet map
leaflet(data = cleaned_data) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    ~longitude, ~latitude,
    radius = 5,  # Marker size
    popup = ~paste("Location Name:", location_name, "<br>",
                   "Category:", category, "<br>",
                   "Details:", details),  # Add popups for each point
    color = "blue"
  ) %>%
  setView(lng = mean(cleaned_data$longitude), lat = mean(cleaned_data$latitude), zoom = 12)

# Step 7: Verify cleaned data
summary(cleaned_data)
head(cleaned_data)

# Save the cleaned dataset for future use
write.csv(cleaned_data, "cleaned_data.csv", row.names = FALSE)

