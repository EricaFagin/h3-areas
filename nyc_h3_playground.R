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

# centroids for all neighborhoods
ny_pts <- st_centroid(map)

#create a new column, one for each H3 hexagon 0 through 15 with the id of the hexagon containing that centroid
ny_hexes <- point_to_cell(ny_pts
                          , res = seq(0, 15)
                          , simple = FALSE
                          )

# we want our dataframe to have both the polygon geometry and the hexagons
ny_pts_hexes <- inner_join(map
                           ,ny_hexes
                           , by = "ntaname"
                           )

view(ny_pts_hexes)

# function to map NYC neighborhoods at specified hexagon resolution
# for reference, column 15 is resolution 0, 16 is resolution 1, etc...
map_hexes <- function(df, col_num){
 hex_map <- cell_to_polygon(df[,col_num], simple = FALSE)
 ggplot(hex_map[1,]) +
   geom_sf(fill = NA, colour = 'black') +
   geom_sf(data = hex_map, aes(fill = h3_address), alpha = 0.5) +
   scale_fill_viridis_d() +
   ggtitle('H3 hexagon mappings for NYC', subtitle = 'Resolution 5') +
   theme_minimal() +
   coord_sf()
}

map_hexes(ny_pts_hexes,19)
