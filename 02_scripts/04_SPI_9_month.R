rm(list = ls())
library(reticulate)
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(tidyverse)


current_year = 2022
previous_year =  current_year-1

###############

source("02_scripts/surveyGEE functions.R")
###############



# read cluster ------------------------------------------------------------

grid <- st_read("01_inputs/01_hexagon/cluster_10_km.shp") %>% st_transform(4326)



# ee initiating  ----------------------------------------------------------

# ee_clean_pyenv()
# ee_install_set_pyenv("04_py_env/Scripts/python.exe")

ee_Initialize(user = "Mehedi",drive = T,gcs = T)



# read data ---------------------------------------------------------------

chirps <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$select("precipitation")$
  map(ee_utils_pyfunc(
    function(x){x$copyProperties(x,x$propertyNames())}
  )) # read CHIRPS with property info


chirps_tidy <- as_tidyee(chirps)




spi_image_collection <- ee_chirps_spi(chirps_tidy,window = 9,window_unit = "month",moi = c(1:8))




spi_image_collection_processed <- spi_image_collection %>% filter(
  year > previous_year
) %>% select("precipitation_sum_9_month_SPI")





spi_final <- list()

first_tier<-1:10000 
second_tier <- 10001:20000
third_tier <- 20001:30000
fourth_tier <- 30001:40000
fifth_tier <- 40001:43648

all_tier <- c("first_tier","second_tier","third_tier","fourth_tier","fifth_tier")

for ( i in all_tier){
  
  grid_filter <- grid[get(i),]
  # 1000o is working 
  spi_final[[i]] <- spi_image_collection_processed %>%  
    ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 5500,via = "drive")
}

spi_bind <- do.call("bind_rows",spi_final)
spi_bind <- spi_bind %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date,label = T,abbr = F)
)

write.csv(spi_bind,"05_outputs/01_precipitation_defict/nine_month_spi.csv")


                                      