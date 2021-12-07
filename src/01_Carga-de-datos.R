library(tidyverse)
library(sf)
library(ggplot2)
library(lubridate)
options(scipen=999)

proj <- "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

barrios <- st_read("data/raw/GCABA/barrios/barrios_badata_wgs84.shp") %>% 
    st_transform(proj) %>% 
    select(BARRIO)

manzanas <- st_read("data/raw/GCABA/manzanas/manzanas.geojson") %>% 
    st_transform(proj) %>% 
    select(SM, MZ_SUP) %>% 
    st_join(barrios)

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
    st_intersection(manzanas) %>% 
    mutate(price_m2=price/surface_total)

# Precio promedio por barrio
prop_precio_barrio <- prop %>% 
    as.data.frame() %>% 
    group_by(BARRIO) %>% 
    summarise(promedio_barrio=mean(price))

# Manzanas con más ventas
venta_por_manzana <- prop %>% 
    as.data.frame() %>% 
    group_by(SM) %>% 
    summarise(cantidad_venta=n())

manzanas <- manzanas %>% left_join(venta_por_manzana, by="SM")

ggplot()+
    geom_sf(data=manzanas, fill="grey90", color="grey80")+
    geom_sf(data=manzanas %>% dplyr::filter(!is.na(cantidad_venta)), aes(fill=cantidad_venta), color=NA)+
    scale_fill_viridis_c(option = "magma", direction=-1) +
    theme_void()

##
prop <- prop %>% left_join(venta_por_manzana, by="SM")


# FECHAS


prop <- prop %>% 
    mutate(fecha=as_date(created_on))

manzana_con_mas_ventas <- prop %>% dplyr:: filter(SM=="098-005M")

ggplot(manzana_con_mas_ventas) + 
    geom_bar(aes(x = fecha), fill="orange", alpha=.5, color="black")

