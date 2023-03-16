#' Preparing and Mergining Datasets
#' 
#' This script merges household survey (LSMS)
#' with datasets from Google Earth Engine (GEE)
#' and the GAEZ v4 Data Portal 
#' 
#' The data from GEE was generated using scripts 
#' from the following repository:
#' 
#' https://github.com/l-gorman/earth-engine-farm-size-analysis
#' 
#' Data from GAEZ can be explored and downloaded here:
#' 
#' https://gaez.fao.org/pages/data-viewer
#' 
#' With more notes on Accessing GAEZ data here:
#' 
#' https://gaez.fao.org/pages/data-access-download
#' 
#' AEZ metadata (for class conversion) found here:
#' 
#' https://gaez-data-portal-hqfao.hub.arcgis.com/pages/data-access-download
#' 
#' For more information, please see the README of this
#' repository
#' 
#' 
#' Instructions on Parralel: https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
#' 
# -------------------------------------------------------------------------------------------------------------
# Loading Libraries -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# Data Cleaning and reformatting
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(jsonlite)
library(XML)
library(fastDummies)
library(magrittr)
library(readxl)
# Spatial Packages
library(sf)
library(stars)
library(terra)
library(raster)
library(sp)
library(geojsonsf)
library(rgdal)



# Defining Functions ------------------------------------------------------



convert_aez_classes <- function(aez_df,
                                aez_colname, 
                                aez_conversion_tbl){
  
  # aez_df <- rasValue
  # aez_colname <- "AEZ_Classes_57"
  # aez_conversion_tbl <- aez_57_class_conversions
  
  aez_df$index <- c(1:nrow(aez_df))
  
  aez_conversion_tbl$band <- as.integer(aez_conversion_tbl$band)
  aez_df[[aez_colname]] <- as.integer(aez_df[[aez_colname]])
  
  
  
  result <- aez_df %>% merge(aez_conversion_tbl, 
                             by.x=aez_colname, 
                             by.y="band",
                             all.x=T,
                             all.y=F)
  
  result <- result[order(result$index),]
  
  result$name <- result$name %>% 
    tolower() %>% 
    gsub("/", " or ", .) %>% 
    gsub(",", "", .) %>% 
    gsub(";", "", .) %>% 
    gsub(" ", "_", .) %>% 
    gsub("-", "_", .) 
  
  
  
  result <- fastDummies::dummy_cols(result, "name", remove_selected_columns=T)
  colnames(result) <- colnames(result) %>% 
    gsub("name_",paste0(aez_colname,"_"),.)
  
  aez_df$index <- NULL
  
  return(result)
}



convert_aez_column_name <- function(aez_df,
                                    aez_colname_pattern, 
                                    aez_conversion_tbl){
  
  # aez_df <- fao_level_2
  # aez_colname_pattern <- "level_2_aez_33_classes_"
  # aez_conversion_tbl <- aez_33_class_conversions
  
  aez_conversion_tbl$name <- aez_conversion_tbl$name %>% 
    tolower() %>% 
    gsub("/", " or ", .) %>% 
    gsub(",", "", .) %>% 
    gsub(";", "", .) %>% 
    gsub(" ", "_", .) %>% 
    gsub("-", "_", .) 
  
  
  columns_to_convert <- grep(aez_colname_pattern, colnames(aez_df@data), value=T)
  column_indices <- grep(aez_colname_pattern, colnames(aez_df@data))
  new_columns <- c()
  for (column in columns_to_convert){
    original_column <- column
    original_column_suffix <- gsub(aez_colname_pattern, "", original_column)
    if (!is.na(as.numeric(original_column_suffix))){
      new_column_suffix <- aez_conversion_tbl$name[aez_conversion_tbl$band==as.numeric(original_column_suffix)]
      new_column <- paste0(aez_colname_pattern,new_column_suffix)
      new_columns <- c(new_columns,new_column)
    }
    else{
      new_columns <- c(new_columns,column)
    }
    
  }
  
  colnames(aez_df@data)[column_indices] <- new_columns
  
  return(aez_df)
}




# Read in Survey Data -----------------------------------------------------

rhomis_data <- readr::read_csv("data/raw-data/rhomis/processed_data.csv", na=c("-999","NA", "n/a"))
indicator_data <- readr::read_csv("data/raw-data/rhomis/indicator_data.csv", na=c("-999","NA", "n/a"))

indicator_data <- indicator_data %>% merge(rhomis_data[c("id_unique","x_gps_latitude", "x_gps_longitude", "village")],by="id_unique")

# indicator_data <- indicator_data %>% merge(rhomis_data[c("id_unique","x_gps_latitude", "x_gps_longitude")],by="id_unique")

indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- st_as_sf(indicator_data, coords = c("x_gps_longitude", "x_gps_latitude"), 
                           crs = 4326, agr = "constant", remove = F)

indicator_data$index <- c(1:nrow(indicator_data))

rhomis_data <- NULL


# global data lab shapefile ---------------------------------------------------


gdl_shp <- sf::read_sf("./data/raw-data/global-data-lab/GDL Shapefiles V6.1/")



gdl_code <- readxl::read_excel("./data/raw-data/global-data-lab/GDL Codes V6.1.xlsx")
gdl_info <- readr::read_csv("./data/raw-data/global-data-lab/SHDI-SGDI-Total 7.0.csv")
country_codes <- readr::read_csv("./data/raw-data/country_conversions.csv")

three_letter_codes <- country_codes[country_codes[["alpha-2"]]%in%indicator_data$iso_country_code,][["alpha-3"]]

gdl_shp <- gdl_shp[gdl_shp$iso_code %in%three_letter_codes,]
gdl_code <- gdl_code[gdl_code$ISO_Code %in%three_letter_codes,]


joined_df_rhomis <- st_join(x=indicator_data,
                            y=gdl_shp,
                            left=T)


table(temp$gdlcode %in% gdl_info$GDLCODE)
table(duplicated(gdl_info$GDLCODE))

joined_df_rhomis <- joined_df_rhomis %>% merge(gdl_info, by.x=c("gdlcode","year"), by.y=c("GDLCODE","year"),all.x=T,all.y=F)



# Joining Raster Information ----------------------------------------------

# Agro-Eco Zone Data (GAEZ)
aez_33_classes <- raster::raster(x = "data/raw-data/gaez/33_classes.tif")
rasterToPoints(aez_33_classes)

xml_33_list <-  xmlParse('data/raw-data/aez/LR/aez/aez_v9v2red_5m_ENSEMBLE_rcp2p6_2020s.tif.aux.xml')
xml_33_list <- xmlToList(xml_33_list)
xml_33_list <- xml_33_list$PAMRasterBand$GDALRasterAttributeTable
xml_33_list <- xml_33_list[names(xml_33_list)=="Row"]


aez_33_class_conversions <- lapply(c(1:length(xml_33_list)), function(index){
  row <- xml_33_list[index]$Row
  names_of_row <- names(row)
  features <- unlist(as.list(as.character(row[names(row)=="F"])))
  features <- c(features,row$.attrs[["index"]])
  feature_names <- paste0("feature_",c(1:length(features)))
  
  
  row_df <- tibble::as_tibble(list(
    var=feature_names,
    value=features
  )) %>% pivot_wider(names_from = "var")
  
  result <- row_df[c("feature_2", "feature_8")]
  colnames(result) <- c("band", "name")
  
  return(result)
})  %>% dplyr::bind_rows()


adjusted_length_growing_period  <- raster("data/raw-data/aez/gaez_v4_57_class/adjusted_length_growing_period.tif")
adjusted_length_growing_period <- projectRaster(adjusted_length_growing_period,aez_33_classes)

travel_time_5k_to_10k <- raster("data/raw-data/travel-time/travel_time_to_cities_9.tif")
travel_time_5k_to_10k <- projectRaster(travel_time_5k_to_10k,aez_33_classes)

travel_time_10k_to_20k <- raster("data/raw-data/travel-time/travel_time_to_cities_8.tif")
travel_time_10k_to_20k <- projectRaster(travel_time_10k_to_20k,aez_33_classes)

travel_time_20k_to_50k <- raster("data/raw-data/travel-time/travel_time_to_cities_7.tif")
travel_time_20k_to_50k <- projectRaster(travel_time_20k_to_50k,aez_33_classes)

travel_time_50k_to_100k <- raster("data/raw-data/travel-time/travel_time_to_cities_7.tif")
travel_time_50k_to_100k <- projectRaster(travel_time_20k_to_50k,aez_33_classes)


r_stack <- raster::stack(aez_33_classes,
                         adjusted_length_growing_period,
                         travel_time_5k_to_10k,
                         travel_time_10k_to_20k,
                         travel_time_20k_to_50k,
                         travel_time_50k_to_100k)


rasValue_rhomis=raster::extract(r_stack, indicator_data[c("x_gps_longitude","x_gps_latitude")]) %>% tibble::as_tibble()
colnames(rasValue_rhomis) <- gsub("X33_classes", "AEZ_Classes_33", colnames(rasValue_rhomis))
rasValue_rhomis$AEZ_Classes_33 <- as.integer(rasValue_rhomis$AEZ_Classes_33)
rasValue_rhomis <- convert_aez_classes(rasValue_rhomis,
                                       "AEZ_Classes_33",
                                       aez_33_class_conversions)


joined_df_rhomis %>% group_by(gdlcode,village) %>% 
  summarise(number_per_village=n()) %>% ungroup() %>% 
  group_by(gdlcode) %>% 
  summarise(number_of_villages=n())



joined_df_rhomis <- cbind(joined_df_rhomis,rasValue_rhomis)


readr::write_csv(joined_df_rhomis,"./prepared-data/rhomis-gaez-gdl.csv")

readr::write_csv(joined_df_rhomis,"./prepared-data/gdl.csv")

gdl_shp <- sf::read_sf("./data/raw-data/global-data-lab/GDL Shapefiles V6.1/")
st_write(gdl_shp, "prepared-data/gdl_data.geojson")



