library(sf)
library(dplyr)
library(stringr)
library(rgdal)

fname <- "/Users/jordanbratt/mapping-elections/data-cleaning/mapping-test/districtShapes"
counter <- 2
read_shapefile <- function(filename) {
  sp <- readOGR(dsn = fname, layer = filename)
  st_as_sf(sp)
}

first_districts <- readOGR(dsn = fname, layer = "districts001")
congressional_unique <- st_as_sf(first_districts)


while(counter != 115){
  filler <- if_else(counter <= 9, "00",
                    if_else(counter <= 99, "0", ""))

  count <- as.character(counter)
  dis <- "districts"
  congress <- str_c(dis, filler, counter)
  temp <- read_shapefile(congress)

  congressional_unique <- rbind (congressional_unique, temp)

  counter <- counter + 1
}

congressional_unique <- congressional_unique %>%
  distinct(ID,.keep_all = TRUE)

st_write(congressional_unique, "congressional_districts.shp")

# while(counter != 110){
#   filler <- if_else(counter <= 9, "00",
#                     if_else(counter <= 99, "0", ""))
#
#   count <- as.character(counter)
#   dis <- "districts"
#   congress <- str_c(dis, filler, counter)
#   temp <- st_read(dsn = fname, layer = congress)
#
#   congressional_unique <- rbind (congressional_unique, temp)
#
#   counter <- counter + 1
# }
#
#
# districts_unique <- congressional_shapefiles %>%
#   distinct(ID,.keep_all = TRUE)
#
#
# congressional_unique <- congressional_unique %>%
#   distinct(ID,.keep_all = TRUE)
