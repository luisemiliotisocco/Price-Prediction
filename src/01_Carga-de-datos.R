library(tidyverse)
library(sf)
library(ggplot2)

proj <- "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

barrios <- st_read("data/raw/GCABA/barrios/barrios_badata_wgs84.shp") %>% 
    st_transform(proj) %>% 
    select(BARRIO)

#_______________________________________________________________________________
### Filtramos el df de properati, que pesa 1GB

#prop <- vroom::vroom("data/raw/Properati/ar_properties.csv") %>% 
#    dplyr::filter(l2=="Capital Federal",
#                  operation_type=="Venta",
#                  property_type=="Departamento") %>% 
#    drop_na(lon, lat, created_on, l2, l3, rooms, bedrooms, bathrooms, surface_total, 
#            surface_covered, price, currency, property_type)

#write.csv (prop, "data/raw/Properati/ar_properties_filtrado.csv")
#_______________________________________________________________________________

prop <- vroom::vroom("data/raw/Properati/ar_properties_filtrado.csv") %>% 
    select(...1, created_on, lat, lon, rooms, bedrooms, bathrooms, surface_total, 
           surface_covered, price, currency) %>% 
    dplyr::filter(currency=="USD") %>% 
    rename(id=...1) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    st_transform(proj) %>% 
    st_intersection(barrios)

# Precio promedio por barrio
prop_precio_barrio <- prop %>% 
    as.data.frame() %>% 
    group_by(BARRIO) %>% 
    summarise(promedio_barrio=mean(price))
