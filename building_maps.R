# Libraries
library(tidyverse)
library(colorspace)
library(ggplot2)
library(sf)
library(dplyr)
library(data.table)
library(DBI)
library(RMySQL)
library(lwgeom)

# Connection with MySQL database
connection <- dbConnect(RMariaDB :: MariaDB(),
                        dbname = 'Swine',
                        host = "localhost",
                        user = "root",
                        password = "projetoporcos")
# Directory
setwd("C:/Users/teres/Desktop/EPIVET/DGAV - SISS/Swine_Pseudorabies_PT/maps")
setwd("~/Desktop/Treino Estágio 2020-2021/Swine_Pseudorabies_PT/maps")

# Read table from MYSQL
freguesias <- dbReadTable(connection, "st_tabela_freguesias")

# Table with dicofre, concelho, svl and dsavr
concelhos <- freguesias %>% select(dicofre, concelho, svl, dsavr)

## Remove the last 2 digits from dicofre
concelhos$dicofre <- substr(concelhos$dicofre, 1, nchar(concelhos$dicofre) - 2)
## Select one row by concelho
concelhos <- unique(concelhos)

# CONTINENT MAP (https://www.dgterritorio.gov.pt/cartografia/cartografia-tematica/caop)
## Changing temporary directory
temp_dir <- tempdir()
temp <- tempfile(tmpdir = temp_dir, fileext = ".zip")
## Set destination for map file
download.file("http://mapas.dgterritorio.pt/ATOM-download/CAOP-Cont/Cont_AAD_CAOP2019.zip", destfile = temp)

## Unzip temp and set a temporary directory to uncompress the archive to (temp_dir)
unzip(temp, exdir = temp_dir)

## Table
cont_geo <- read_sf(temp_dir)

## Convert SRS to WGS84
cont_geo <- st_as_sf(cont_geo, 4326)
cont_geo <- st_transform(cont_geo, "+proj=longlat +datum=WGS84")

### View map
ggplot(cont_geo) + geom_sf()

## Add column with area
cont_geo$area <- st_area(cont_geo)

## Remove last 2 digits from DICOFRE
cont_geo$Dicofre <- substr(cont_geo$Dicofre, 1, nchar(cont_geo$Dicofre) - 2)

## Merge with concelho's table
cont_geo_concelhos <- merge(cont_geo, concelhos, by.x = "Dicofre", by.y = "dicofre", all = TRUE)

## Remove NA 
cont_geo_concelhos <- na.omit(cont_geo_concelhos)

## Aggregate by LVS
cont_geo_lvs <- cont_geo_concelhos %>%
  group_by(svl) %>%
  summarise(area = sum(area))
cont_geo_lvs <- na.omit(cont_geo_lvs)

### View map
ggplot(cont_geo_lvs) + geom_sf()

## Aggregate by FVRD
cont_geo_fvrd <- cont_geo_concelhos %>%
  group_by(dsavr) %>%
  summarise(area = sum(area))
cont_geo_fvrd <- na.omit(cont_geo_fvrd)

### View geo 
ggplot(cont_geo_fvrd) + geom_sf()


## Export as SHP files
st_write(cont_geo_fvrd, dsn = "continent_dsavr_map", driver = "ESRI Shapefile")
st_write(cont_geo_lvs, dsn = "continent_svl_map", driver = "ESRI Shapefile")


###### FAZER TERMINATE R ########

# MADEIRA MAP
temp_dir <- tempdir()
temp <- tempfile(tmpdir = temp_dir, fileext = ".zip")
download.file("http://mapas.dgterritorio.pt/ATOM-download/CAOP-RAM/ArqMadeira_AAD_CAOP2019.zip", destfile = temp)
unzip(temp, exdir = temp_dir)

## Table
mad_geo <- read_sf(temp_dir)

## Convert SRS to WGS84
mad_geo <- st_as_sf(mad_geo, 4326)
mad_geo <- st_transform(mad_geo, "+proj=longlat +datum=WGS84")

### View map
ggplot(mad_geo) + geom_sf()

## Add column with area
mad_geo$area <- st_area(mad_geo)

## Remove last 2 digits from DICOFRE
mad_geo$DICOFRE <- substr(mad_geo$DICOFRE, 1, nchar(mad_geo$DICOFRE) - 2)

## Merge with concelho's table
mad_geo_concelhos <- merge(mad_geo, concelhos, by.x = "DICOFRE", by.y = "dicofre", all.x = TRUE, all.y = FALSE)

## Aggregate by LVS
mad_geo_lvs <- mad_geo_concelhos %>%
  group_by(svl) %>%
  summarise(area = sum(area))

### View map
ggplot(mad_geo_lvs) + geom_sf()


## Aggregate by FVRD
mad_geo_fvrd <- mad_geo_concelhos %>%
  group_by(dsavr) %>%
  summarise(area = sum(area))

### View geo 
ggplot(mad_geo_fvrd) + geom_sf()


## Export as SHP files
st_write(mad_geo_fvrd, dsn = "madeira_dsavr_map", driver = "ESRI Shapefile")
st_write(mad_geo_lvs, dsn = "madeira_svl_map", driver = "ESRI Shapefile")


###### FAZER TERMINATE R ########

# EASTERN AZORES MAP
temp_dir <- tempdir()
temp <- tempfile(tmpdir = temp_dir, fileext = ".zip")
download.file("http://mapas.dgterritorio.pt/ATOM-download/CAOP-RAA/ArqAcores_GOriental_AAd_CAOP2019.zip", destfile = temp)
unzip(temp, exdir = temp_dir)

## Table
aze_geo <- read_sf(temp_dir)

## Convert SRS to WGS84
aze_geo <- st_as_sf(aze_geo, 4326)
aze_geo <- st_transform(aze_geo, "+proj=longlat +datum=WGS84")

### View map
ggplot(aze_geo) + geom_sf()

## Add column with area
aze_geo$area <- st_area(aze_geo)

## Remove last 2 digits from DICOFRE
aze_geo$DICOFRE <- substr(aze_geo$DICOFRE, 1, nchar(aze_geo$DICOFRE) - 2)

## Merge with concelho's table
aze_geo_concelhos <- merge(aze_geo, concelhos, by.x = "DICOFRE", by.y = "dicofre", all.x = TRUE, all.y = FALSE)

## Aggregate by LVS
aze_geo_lvs <- aze_geo_concelhos %>%
  group_by(svl) %>%
  summarise(area = sum(area))

## Change LVS name
aze_geo_lvs$svl <- replace(aze_geo_lvs$svl, aze_geo_lvs$svl == "DSAVR Algarve", "DRADR AÇORES")

## Merge the 2 in one
aze_geo_lvs <- aze_geo_lvs %>%
  group_by(svl) %>%
  summarise(area = sum(area))

### View map
ggplot(aze_geo_lvs) + geom_sf()


## Aggregate by FVRD
aze_geo_fvrd <- aze_geo_concelhos %>%
  group_by(dsavr) %>%
  summarise(area = sum(area))

## Change FVRD name
aze_geo_fvrd$dsavr <- replace(aze_geo_fvrd$dsavr, aze_geo_fvrd$dsavr == "DSAVR Algarve", "DRADR - R. A. AÇORES")

## Merge the 2 in one
aze_geo_fvrd <- aze_geo_fvrd %>%
  group_by(dsavr) %>%
  summarise(area = sum(area))

### View geo 
ggplot(aze_geo_fvrd) + geom_sf()


## Export as SHP files
st_write(aze_geo_fvrd, dsn = "east_azores_dsavr_map", driver = "ESRI Shapefile")
st_write(aze_geo_lvs, dsn = "east_azores_svl_map", driver = "ESRI Shapefile")


###### FAZER TERMINATE R ########

# CENTRAL AZORES MAP
temp_dir <- tempdir()
temp <- tempfile(tmpdir = temp_dir, fileext = ".zip")
download.file("http://mapas.dgterritorio.pt/ATOM-download/CAOP-RAA/ArqAcores_GCentral_AAd_CAOP2019.zip", destfile = temp)
unzip(temp, exdir = temp_dir)

## Table
azc_geo <- read_sf(temp_dir)

## Convert SRS to WGS84
azc_geo <- st_as_sf(azc_geo, 4326)
azc_geo <- st_transform(azc_geo, "+proj=longlat +datum=WGS84")

### View map
ggplot(azc_geo) + geom_sf()

## Add column with area
azc_geo$area <- st_area(azc_geo)

## Remove last 2 digits from DICOFRE
azc_geo$DICOFRE <- substr(azc_geo$DICOFRE, 1, nchar(azc_geo$DICOFRE) - 2)

## Merge with concelho's table
azc_geo_concelhos <- merge(azc_geo, concelhos, by.x = "DICOFRE", by.y = "dicofre", all.x = TRUE, all.y = FALSE)

## Aggregate by LVS
azc_geo_lvs <- azc_geo_concelhos %>%
  group_by(svl) %>%
  summarise(area = sum(area))

### View map
ggplot(azc_geo_lvs) + geom_sf()


## Aggregate by FVRD
azc_geo_fvrd <- azc_geo_concelhos %>%
  group_by(dsavr) %>%
  summarise(area = sum(area))

### View geo 
ggplot(azc_geo_fvrd) + geom_sf()


## Export as SHP files
st_write(azc_geo_fvrd, dsn = "central_azores_dsavr_map", driver = "ESRI Shapefile")
st_write(azc_geo_lvs, dsn = "central_azores_svl_map", driver = "ESRI Shapefile")


###### FAZER TERMINATE R ########

# WESTERN AZORES MAP
temp_dir <- tempdir()
temp <- tempfile(tmpdir = temp_dir, fileext = ".zip")
download.file("http://mapas.dgterritorio.pt/ATOM-download/CAOP-RAA/ArqAcores_GOcidental_AAd_CAOP2019.zip", destfile = temp)
unzip(temp, exdir = temp_dir)

## Table
azw_geo <- read_sf(temp_dir)

## Convert SRS to WGS84
azw_geo <- st_as_sf(azw_geo, 4326)
azw_geo <- st_transform(azw_geo, "+proj=longlat +datum=WGS84")

### View map
ggplot(azw_geo) + geom_sf()

## Add column with area
azw_geo$area <- st_area(azw_geo)

## Remove last 2 digits from DICOFRE
azw_geo$DICOFRE <- substr(azw_geo$DICOFRE, 1, nchar(azw_geo$DICOFRE) - 2)

## Merge with concelho's table
azw_geo_concelhos <- merge(azw_geo, concelhos, by.x = "DICOFRE", by.y = "dicofre", all.x = TRUE, all.y = FALSE)

## Aggregate by LVS
azw_geo_lvs <- azw_geo_concelhos %>%
  group_by(svl) %>%
  summarise(area = sum(area))

### View map
ggplot(azw_geo_lvs) + geom_sf()


## Aggregate by FVRD
azw_geo_fvrd <- azw_geo_concelhos %>%
  group_by(dsavr) %>%
  summarise(area = sum(area))

### View geo 
ggplot(azw_geo_fvrd) + geom_sf()


## Export as SHP files
st_write(azw_geo_fvrd, dsn = "west_azores_dsavr_map", driver = "ESRI Shapefile")
st_write(azw_geo_lvs, dsn = "west_azores_svl_map", driver = "ESRI Shapefile")


# ISLANDS MAP BY LVS
## Read maps from github
azc_svl_map <- read_sf("central_azores_svl_map")
aze_svl_map <- read_sf("east_azores_svl_map")
azw_svl_map <- read_sf("west_azores_svl_map")

mad_svl_map <- read_sf("madeira_svl_map")

## Combine all in one
isl_svl_map <- rbind(azc_svl_map, aze_svl_map, azw_svl_map, mad_svl_map)

## View map 
ggplot(isl_svl_map) + geom_sf()




# ISLANDS MAP BY FVRD
## Read maps from github
azc_dsavr_map <- read_sf("central_azores_dsavr_map")
aze_dsavr_map <- read_sf("east_azores_dsavr_map")
azw_dsavr_map <- read_sf("west_azores_dsavr_map")
mad_dsavr_map <- read_sf("madeira_dsavr_map")

## Combine all in one
isl_dsavr_map <- rbind(azc_dsavr_map, aze_dsavr_map, azw_dsavr_map, mad_dsavr_map)

## View map 
ggplot(isl_dsavr_map) + geom_sf()

## Export as SHP files
st_write(isl_svl_map, dsn = "islands_svl_map", driver = "ESRI Shapefile")
st_write(isl_dsavr_map, dsn = "islands_dsavr_map", driver = "ESRI Shapefile")



# CONTINENT + ISLANDS MAP BY LVS
## Read maps from github
continent_svl_map <- read_sf("continent_svl_map")
islands_svl_map <- read_sf("islands_svl_map")

## Combine all in one 
portugal_svl_map <- rbind(continent_svl_map, islands_svl_map)

### Azores 3 in 1
portugal_svl_map <- portugal_svl_map %>%
  group_by(svl) %>%
  summarise(area = sum(area))

## View map 
ggplot(portugal_svl_map) + geom_sf()

## Export as SHP files
st_write(portugal_svl_map, dsn = "pt_svl_map", driver = "ESRI Shapefile")


# CONTINENT + ISLANDS MAP BY FVRD
## Read maps from github
continent_fvrd_map <- read_sf("continent_dsavr_map")
islands_fvrd_map <- read_sf("islands_dsavr_map")

## Combine all in one 
portugal_fvrd_map <- rbind(continent_fvrd_map, islands_fvrd_map)

### Azores 3 in 1
portugal_fvrd_map <- portugal_fvrd_map %>%
  group_by(dsavr) %>%
  summarise(area = sum(area))

## View map 
ggplot(portugal_fvrd_map) + geom_sf()

## Export as SHP files
st_write(portugal_fvrd_map, dsn = "pt_dsavr_map", driver = "ESRI Shapefile")
