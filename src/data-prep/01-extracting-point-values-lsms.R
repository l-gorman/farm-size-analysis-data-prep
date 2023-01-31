#' This script is used
#' to extract point values from
#' multiple LSMS surveys 
#' 
#' The resulting table could
#' is then uploaded to google earth
#' engine as a "feature collection"
#' to link each household to a particular
#' point
#' 
#' See here:
#' https://developers.google.com/earth-engine/guides/table_upload

library(readr)
library(tibble)
library(dplyr)
library(magrittr)
library(ggplot2)
file_paths <- c(
  "LSMS_Burkina_landsizes.csv",
  "LSMS_Tanzania_landsizes.csv",
  "LSMS_Ethiopia_landsizes.csv",
  "LSMS_Uganda_2012_landsizes.csv",
  "LSMS_Malawi_2011_landsizes.csv",
  "LSMS_Mali_landsizes.csv",
  "LSMS_Niger_landsizes.csv",
  "LSMS_Nigeria_landsizes.csv"
  )

base_path <- "./data/raw-data/lsms/land-size/"

all_lsms_data_sets <- lapply(file_paths, function(x){
  
  
  new_df <- readr::read_csv(paste0(base_path,"/",x))
  new_df <- new_df[,!colSums(is.na(new_df))==nrow(new_df)]
  
  colnames(new_df) <- tolower(colnames(new_df))
  colnames(new_df) <- gsub("Nr  of plots", "nr of plots",colnames(new_df) )
  colnames(new_df) <- gsub("nr of plots", "nr of plots",colnames(new_df) )
  colnames(new_df) <- gsub("farm size [ha]", "farm_size_ha",colnames(new_df), fixed=T)
  
  return(new_df)
  
}) %>% dplyr::bind_rows()

all_lsms_data_sets <- all_lsms_data_sets[c("latitude", "longitude", "farm_size_ha")]
all_lsms_data_sets <- all_lsms_data_sets[complete.cases(all_lsms_data_sets),]

all_lsms_data_sets$index <- c(1:nrow(all_lsms_data_sets))


lsms_all <- readr::write_csv(all_lsms_data_sets,"./data/prepped-data/lsms/farm-size-all-points.csv")
