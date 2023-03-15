library(tidyverse)

services <- read_csv('temp/combined.csv')
peel_postal_codes <- read_lines('../data/peel_postal_codes.txt')

extract_postal_code <- Vectorize(function(address) {
  str_extract(address, '[:upper:]\\d[:upper:] ?\\d[:upper:]\\d')
})

services <- services %>%
  mutate(Postal = ifelse(is.na(Postal), extract_postal_code(Address), Postal))

# obtained postal code coordinates here:
# https://www.serviceobjects.com/blog/free-zip-code-and-postal-code-database-with-geocoordinates/
extract_numeric_coordinates <- function(coordinates, get_latitude) {
  if (is.na(coordinates)) {
    return(NA)
  }
  
  if (get_latitude) {
    i <- 1
  } else {
    i <- 2
  }
  
  coordinates <- as.numeric(
    str_split(str_remove_all(coordinates, '\\)|\\('), ', ')[[1]]
  )
  
  return(coordinates[i])
}

postal_coordinates <- read_csv('../data/CanadianPostalCodes202208.csv') %>%
  rename(Postal = POSTAL_CODE, Latitude = LATITUDE, Longitude = LONGITUDE) %>%
  select(Postal, Latitude, Longitude)

services <- services %>%
  left_join(postal_coordinates, by = 'Postal') %>%
  rowwise() %>%
  mutate(Latitude = ifelse(is.na(Latitude), extract_numeric_coordinates(`Coordinates (lat, long)`, TRUE), Latitude),
         Longitude = ifelse(is.na(Longitude), extract_numeric_coordinates(`Coordinates (lat, long)`, FALSE), Longitude))

# temporary dataframe, for testing out arcgis
temp_df <- services %>%
  select(Name, Address, Postal, Latitude, Longitude) %>%
  mutate(in_peel = substr(Postal, 1, 3) %in% peel_postal_codes || 
           str_detect(Address, 'Brampton|Mississauga|Caledon') ||
           str_detect(Name, 'Brampton|Mississauga|Caledon')) %>%
  filter(in_peel)

write_csv(temp_df, '../results/coordinates.csv')