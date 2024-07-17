# some packages may timeout when installing
getOption('timeout')
options(timeout=600)

# packages
install.packages("tidyverse")
install.packages("h3jsr")
# https://cran.r-project.org/web/packages/h3jsr/h3jsr.pdf
install.packages("sf")

library(tidyverse)
library(ggplot2)
library(h3jsr)
library(sf)


#import geodata (needs to be GeoJSON)
# NYC data from here: https://data.cityofnewyork.us/City-Government/2010-Neighborhood-Tabulation-Areas-NTAs-/cpf4-rkhq
map <- read_sf("/Users/ericafagin/Documents/H3/NYC/nycntas.geojson") #PUT YOUR OWN PATH HERE

# CRS should be WGS84 in order to work with H3
st_crs(map)

# map it to have a visual handy
ggplot(map) +
  geom_sf()

# this function calculates the H3 hexagons (for all resolutions) that contain the centroid of each geometry in your dataset
# the output is a dataframe that contains the original geometries and their associated H3 hexagons
geometry_calc <- function(df # a dataframe containing a <geometry> column
                          , join_column) # the unique id column in your dataframe
  { 
      points <- st_centroid(df)
      all_hexes <- point_to_cell(points
                                 , res = seq(0,15)
                                 , simple = FALSE
                              )
      prejoin <- select(s_hex
                        , ntacode
                        , h3_resolution_0:h3_resolution_15
                      )
      ny_geom_hexes <- inner_join(map
                           , prejoin
                           , by = join_column
                      )
      view(ny_geom_hexes)
  }

geometry_calc(map,"ntacode")


# function to map NYC neighborhoods at specified hexagon resolution
# for reference, column 15 is resolution 0, 16 is resolution 1, etc...
map_hexes <- function(df, col_num){
 hex_map <- cell_to_polygon(df[,col_num], simple = FALSE)
 ggplot(hex_map[1,]) +
   geom_sf(fill = NA, colour = 'black') +
   geom_sf(data = hex_map, aes(fill = h3_address), alpha = 0.5) +
   scale_fill_viridis_d() +
   ggtitle('H3 hexagon mappings for NYC', subtitle = 'Hexagon level is whatever you specified in the function earlier')
   theme_minimal() +
   coord_sf()
}

map_hexes(ny_hexes,19)