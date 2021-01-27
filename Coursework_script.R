library(sf)
library(sp)
library(readxl)
library(tidyverse)
library(tmap)
library(tmaptools)
library(rgdal)
library(mapview)
library(ggplot2)
library(ggthemes)
library(mapboxapi)
library(spatstat)
library(httr)
library(pins)
library(XML)
library(methods)
library(purrr)
library(ggmap)
library (reshape2)
library(plotly)

setwd("/Users/AnnaLvova/Documents/CASA/Term_1/GIS/assignment")
tmap_mode("view")

#I. DATA COLLECTION AND CLEANING

#download open data about buildings in Moscow including buildings address, built year and flat count
reforma_url  <-  "https://www.reformagkh.ru/opendata/export/101"
reforma_tbl <-  pin(reforma_url)

reforma_buildings <- read_csv2(reforma_tbl, na = "NA",
               col_types = cols_only(
                 id = col_integer(),
                 address = col_character(),
                 built_year = col_integer(),
                 living_quarters_count = col_double())
               )

reforma_buildings <- reforma_buildings%>% 
  dplyr::rename(flats_count=living_quarters_count)

#please uncomment to check the geocoding part below
#reforma_buildings_ <- sample_n(reforma_buildings, 5)

#geocode addresses into coordinates
base_url <- "https://geocode-maps.yandex.ru/1.x/?"
apikey_ya = "2bdf984e-37cd-4fa6-bd89-e64a8d1c1cf4"
func <- function(addr){
r <- GET(base_url, 
           query = list(apikey = apikey, format = "json", geocode = enc2utf8(addr)))
  result <-  content(r)
  coord <- result[["response"]][["GeoObjectCollection"]][["featureMember"]][[1]][["GeoObject"]][["Point"]][["pos"]]
  return(coord)
}

res <- reforma_buildings %>%
  rowwise() %>% 
  mutate(newcol = func(address))

buildings_msc <- res %>% 
  filter(newcol != "   ") %>% 
  filter(id != 9068616) %>% 
  rowwise() %>% 
  mutate(
    lat = strsplit(newcol, "   ")[[1]][1],
    lon = strsplit(newcol, "   ")[[1]][2]
  ) %>% 
  st_as_sf(., coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., 3857)

st_bbox(buildings_msc)

# st_write(buildings_msc, "data/buildings_msc.geojson")
buildings_msc <- st_read("data/buildings_msc.geojson")

#Data about organisations and pois from Moscow data store

# 0. Eating out
download.file("https://op.mos.ru/EHDWSREST/catalog/export/get?id=1023452",
              destfile = "data/eating_out.zip")
  
unzip("data/eating_out.zip", exdir="data")
eating_out_source <- read_excel("data/data-4275-2021-01-05.xlsx",
                         na = "")

table(eating_out_source$TypeObject)

Datatypelist <- eating_out %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

# eating_out <- eating_out_source %>% 
#   dplyr::select(ID, Name,Address, TypeObject,SeatsCount,Longitude_WGS84,Latitude_WGS84) %>%
#   mutate(seats_count = as.numeric(SeatsCount), lon = as.numeric(Longitude_WGS84), lat = as.numeric(Latitude_WGS84),) %>% 
#   dplyr::select(ID, Name,Address, TypeObject,seats_count,lon,lat) %>%
#   dplyr::filter(seats_count > 0) %>% 
#   st_as_sf(., coords = c("lon", "lat"), 
#          crs = 4326) %>% 
#   st_transform(., 3857)
# 
# qtm(eating_out)

# 1. Groceries

download.file("https://op.mos.ru/EHDWSREST/catalog/export/get?id=1034392",
              destfile = "data/groceries.zip")
unzip("data/groceries.zip", exdir="data")

groceries_source <- read_excel("data/data-28509-2021-01-24.xlsx",
                                na = "")

table(groceries_source$TypeService)
table(groceries$IsNetObject)

Datatypelist <- groceries_source %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

groceries <- groceries_source %>% 
  dplyr::filter(TypeService == "реализация продовольственных товаров") %>% 
  dplyr::filter(TypeObject!="Магазин «Алкогольные напитки»") %>% 
  dplyr::filter(IsNetObject == "да") %>% 
  dplyr::select(ID, Name,Address,IsNetObject,TypeObject,geoData) %>%
  rowwise() %>% 
  mutate(
    coord = strsplit(geoData, "coordinates=[", fixed=TRUE)[[1]][2],
    lon = strsplit(coord, ", ", fixed=TRUE)[[1]][1],
    lat = strsplit(coord, ", ", fixed=TRUE)[[1]][2],
    lat = strsplit(lat, "]}", fixed=TRUE)[[1]][1]
    ) %>% 
  dplyr::select(ID, Name,Address,IsNetObject,TypeObject,lat,lon) %>%
  st_as_sf(., coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., 3857)

# 2. Parks

parks <- st_read('data/parks.geojson') %>% 
  st_transform(., 3857) %>% 
  select(name,geometry)

parks$sqm_area <- st_area(parks)
parks <- parks %>% 
  mutate(sqm_area_num = as.numeric(sqm_area)/1000000) %>% 
  filter(sqm_area_num > 0.1)

# 3. Schools

schools <- read_tsv("data/schools.tsv") %>% 
  st_as_sf(., coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_transform(., 3857)

# 4. Doctors
doctors <- st_read("data/doctors.geojson") %>% 
  dplyr::select(global_id, ShortName,ObjectAddress, geometry) %>% 
  st_transform(., 3857)

# 5. Sports (osm data via overpass-turbo)
sport <- st_read("data/sport.geojson") %>% 
  dplyr::select(name, leisure, sport, geometry) %>% 
  st_transform(., 3857)

#2. ISOCHRONES

houses_points <- houses_points %>% 
  dplyr::select(address,lat,lon,geom) %>%
  rowid_to_column(., "sid")

mb_token <- "pk.eyJ1IjoiYXNsdm92YSIsImEiOiJja2ptOGdvcjIwMWQ3MnZvOHEyM2x3bXQ3In0.BRbwaqHQSnuBwpFATRBDzQ"

#takes approx 2hours
iso_15 <- mb_isochrone(buildings_msc, "walking", time = 15, id_column = "id", access_token = mb_token)

iso_15 %>% 
  st_write(., "data/buildings_msc_iso_15.geojson")

iso_15 <- st_read("data/buildings_msc_iso_15.geojson") %>% 
  st_transform(., 3857)
iso_15$sqm_area <- st_area(iso_15)

mean(iso_15$sqm_area_num)
mean(buf$sqm_area_num)

iso_15 <- iso_15 %>% 
  mutate(sqm_area_num = as.numeric(sqm_area)/1000000)

s <- iso_15 %>%  filter(id %in% c(7827017,9367231))
b <- buildings_msc %>% filter(id %in% c(7827017,9367231))

buf <- b %>% 
  st_buffer(dist = 2200)
buf$sqm_area <- st_area(buf)
buf <- buf %>% 
  mutate(sqm_area_num = as.numeric(sqm_area)/1000000)

gghist <- ggplot(iso_15, 
                 aes(x=sqm_area_num)) + 
  geom_histogram(color="white", 
                 fill="coral",
                 alpha = 0.7)+
  labs(title="Distribution of service areas in Moscow", 
       x="Sqm area, km2", 
       y="Frequency")
gghist + geom_vline(aes(xintercept=mean(sqm_area_num, 
                                        na.rm=TRUE)),
                    color="red",
                    linetype="dashed",
                    size=1)+
  annotate("text", x=7.2, y=4000, label=paste0("Avg: ", round(mean(iso_15$sqm_area_num), 1)))+
  
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept=mean(buf$sqm_area_num, 
                                 na.rm=TRUE)),
             color="blue", 
             size=1)+
  
  annotate("text", x=14, y=4000, label=paste0("Buffer: ", round(mean(buf$sqm_area_num), 1)))+
  theme(plot.title = element_text(hjust = 0.5))


#III. INTERSECT

# 1
groceries_sa <- groceries %>%
  st_join(., iso_15) %>%
  na.omit() # omit POI that do not intersect with any service area

groceries_count <- groceries_sa %>%
  group_by(id) %>%
  summarise(n=n())%>% 
  st_drop_geometry()

joined_groceries <- groceries_count %>% 
  merge(.,buildings_msc, by.x="id", by.y="id", all.y = TRUE) %>% 
  mutate(n = coalesce(n, 0))

# 2
parks_sa <- parks %>%
  st_join(., iso_15) %>%
  na.omit() # omit POI that do not intersect with any service area

parks_count <- parks_sa %>%
  group_by(id) %>%
  summarise(n=n()) %>% 
  st_drop_geometry()

joined_parks <- parks_count %>% 
  merge(.,buildings_msc, by.x="id", by.y="id", all.y = TRUE) %>% 
  mutate(n = coalesce(n, 0))

# 3
schools_sa <- schools %>%
  st_join(., iso_15) %>%
  na.omit() # omit POI that do not intersect with any service area

schools_count <- schools_sa %>%
  group_by(id) %>%
  summarise(Unique_Elements = n_distinct(rating)) %>%  
  dplyr::rename(n_schools = Unique_Elements) %>% 
  st_drop_geometry()

joined_schools <- schools_count %>% 
  merge(.,buildings_msc, by.x="id", by.y="id", all.y = TRUE) %>% 
  mutate(n_schools = as.numeric(n_schools)) %>%  
  mutate(n_schools = coalesce(n_schools, 0))

# 4
doctors_sa <- doctors %>%
  st_join(., iso_15) %>%
  na.omit() # omit POI that do not intersect with any service area

doctors_count <- doctors_sa %>%
  group_by(id) %>%
  summarise(n=n()) %>% 
  st_drop_geometry()

joined_doctors <- doctors_count %>% 
  merge(.,buildings_msc, by.x="id", by.y="id", all.y = TRUE) %>% 
  mutate(n = coalesce(n, 0)) %>% 
  st_as_sf()

# 5
  
sport_sa <- sport %>%
  st_join(., iso_15) %>%
  na.omit() # omit POI that do not intersect with any service area

sport_count <- sport_sa %>%
  group_by(id) %>%
  summarise(n=n())%>% 
  st_drop_geometry()

joined_sport <- sport_count %>% 
  merge(.,buildings_msc, by.x="id", by.y="id", all.y = TRUE) %>% 
  mutate(n = coalesce(n, 0))

#Merge all

joined_groceries <- joined_groceries %>% 
  dplyr::select(id,n) %>%
  dplyr::rename(n_groceries=n)

joined_parks <- joined_parks %>% 
  dplyr::select(id,n) %>%
  dplyr::rename(n_parks=n)

joined_schools <- joined_schools %>% 
  dplyr::select(id,n_schools)

joined_doctors <- joined_doctors %>% 
  dplyr::select(id,n) %>%
  dplyr::rename(n_doctors=n)

joined <- joined_sport %>% 
  dplyr::select(id,n, built_year, flats_count) %>%
  dplyr::rename(n_sports=n) %>% 
  merge(.,joined_groceries, by.x="id", by.y="id") %>% 
  merge(.,joined_parks, by.x="id", by.y="id") %>% 
  merge(.,joined_schools, by.x="id", by.y="id") %>% 
  merge(.,joined_doctors, by.x="id", by.y="id") %>% 
  mutate(n1 = case_when(n_groceries > 0 ~ 1, 
                        TRUE ~0)) %>% 
  mutate(n2 = case_when(n_parks > 0 ~ 1, 
                        TRUE ~0)) %>% 
  mutate(n3 = case_when(n_schools > 0 ~ 1, 
                        TRUE ~0)) %>% 
  mutate(n4 = case_when(n_doctors > 0 ~ 1, 
                        TRUE ~0)) %>% 
  mutate(n5 = case_when(n_sports > 0 ~ 1, 
                        TRUE ~0)) %>% 
  mutate(n_total = n1 + n2 + n3 + n4 + n5) %>% 
  st_as_sf() %>% 
  st_transform(., 3857)

st_write(joined, "data/results.geojson")
# joined <- st_read("data/results.geojson")

gathered <- as.data.frame(joined) %>%
  select(n_groceries, n_parks, n_schools, n_doctors, n_sports)
  
gathered %>% 
  gather(cols, value) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(color="white", 
                 fill="coral",
                 bins=50,
                 alpha = 0.7) + 
  facet_wrap(~ cols, ncol = 3)

# #looking at histograms in depth
# gghist_2 <- ggplot(gathered, 
#                    aes(x=n_schools)) + 
#   geom_histogram(color="white", 
#                  fill="coral",
#                  alpha = 0.7)+
#   labs(title="Groceries accessibility in Moscow", 
#        x="Parks", 
#        y="Frequency")
# gghist_2
# 
# gghist_2 <- ggplot(gathered, 
#                    aes(x=n_sports)) + 
#   geom_histogram(color="white", 
#                  fill="coral",
#                  bins=50,
#                  alpha = 0.7)
# gghist_2
# 
# gghist_3 <- ggplot(gathered, 
#                    aes(x=n_parks)) + 
#   geom_histogram(color="white", 
#                  fill="coral",
#                  bins=50,
#                  alpha = 0.7)
# gghist_3

gghist <- ggplot(joined,
                   aes(x=n_total)) +
  geom_histogram(color="white",
                 fill="coral",
                 bins=20,
                 alpha = 0.7)+
  ggtitle("How many types of facilities are accessible?") + 
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5)) +
  labs(x = "number of categories", y = "total buildings")

NROW(subset(joined,n_total==5))
NROW(subset(joined,n_total==5))/NROW(joined)

#accessibility by flats
joined <- joined %>% 
  mutate(flats_count = coalesce(flats_count, 0))
sum(joined$flats_count)

gghist_flats <- ggplot(joined,
                 aes(x=n_total, weights=flats_count/1000000)) +
  geom_histogram(color="white",
                 fill="coral",
                 bins=20,
                 alpha = 0.7)+
  ggtitle("How many types of facilities are accessible?") + 
  theme(plot.title = element_text(lineheight=.8, hjust = 0.5)) +
  labs(x = "number of categories", y = "total households, mln")
gghist_flats

sum(subset(joined, n_total==5)$flats_count)
sum(joined$flats_count) - sum(subset(joined, n_total==5)$flats_count)

#Aggregate by grid
grid <-  st_read("data/grid.geojson") %>% 
  mutate(grid_id = row_number()) %>% 
  st_transform(., 3857)

#I. Map one of the layers

joined_parks_map <- parks_count %>% 
  merge(.,buildings_msc, by.x="id", by.y="id", all.y = TRUE) %>% 
  mutate(n = coalesce(n, 0)) %>% 
  st_as_sf() %>% 
  st_transform(., 3857)

parks_grid <- joined_parks_map %>%
  st_join(grid,.) %>%
  group_by(grid_id) %>%
  summarize(mean_park = mean(n, na.rm = TRUE))

#show parks
tmap_mode("plot")
qtm(msc_osm, alpha=0.5)+
  tm_shape(parks) +
  tm_polygons(col="#7fc97f")+
  tm_scale_bar(width = 0.1, position = c("right", "bottom")) +
  tm_compass(position = c("right", "bottom"))+
  # tm_borders(col = "black", lwd=0.1)+
  tm_layout(
    legend.position = c(0.1, 0.2),
    legend.outside = TRUE,
    frame=FALSE,
    legend.hist.size = 0.5)

#show parks accessibility
tmap_mode("plot")
qtm(msc_osm, alpha=0.5)+
  tm_shape(parks_grid)+
  tm_fill(col="mean_park",
          breaks = c(0, 1, 2, 3, 4, Inf),
          palette="BuGn",
          alpha=0.8,
          legend.hist = TRUE,
          title = "Number of parks\n within 15 mins\n ") + 
  tm_scale_bar(width = 0.1, position = c("right", "bottom")) +
  tm_compass(position = c("right", "bottom"))+
  tm_borders(col = "black", lwd=0.1)+
  tm_layout(
    legend.position = c(0.1, 0.2),
    legend.outside = TRUE,
    frame=FALSE,
    legend.hist.size = 0.5)

# 2. Aggregate all

output <- joined %>% 
  st_join(grid,.) %>% 
  group_by(grid_id) %>% 
  summarize(mean_n = mean(n_total, na.rm = TRUE))

# tmap_mode("view")
tmap_mode("plot")

bb_msc <- bb("Moscow", width=1.13, height=.92)
msc_osm <- read_osm(bb_msc, type = "stamen-toner")
# msc_osm <- read_osm(bb_msc, type = "osm")

qtm(msc_osm)+
  tm_shape(output)+
  tm_fill(col="mean_n",
              palette="viridis",
              breaks = c(0,0.5, 1, 1.5, 2, 2.5, 3,3.5, 4, 4.5, Inf),
              alpha=0.7,
              legend.hist = TRUE,
              title = "Types of services within 15 mins") + 
  tm_scale_bar(width = 0.1, position = c("right", "bottom")) +
  tm_compass(position = c("right", "bottom"))+
  tm_borders(col = "black", lwd=0.1)+
tm_layout(
          legend.position = c(0.1, 0.2),
          legend.outside = TRUE,
          frame=FALSE,
          legend.hist.size = 0.5)

NROW(subset(output,mean_n>=4))
NROW(subset(joined,n_total==5))/NROW(joined)
