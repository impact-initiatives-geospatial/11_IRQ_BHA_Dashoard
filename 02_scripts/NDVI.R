rm(list = ls())

library(reticulate)
# use_python("04_py_env/Scripts/python.exe")

library(reticulate)
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(tidyverse)

# read cluster ------------------------------------------------------------

grid <- st_read("01_inputs/01_hexagon/cluster_10_km.shp")
                                                             # to make things faster, temporary 


# initiatin gee -----------------------------------------------------------
# 

# ee_clean_pyenv()
# ee_install_set_pyenv("04_py_env/Scripts/python.exe")

ee_Initialize(user = "Mehedi",drive = T,gcs = T)


# ndvi --------------------------------------------------------------------

############################## READ modis data ###########################

modis_link <- "MODIS/006/MOD13Q1"
modisIC <- ee$ImageCollection(modis_link)


modis_ndvi <- modisIC$
  select("NDVI")$
  map(
    ee_utils_pyfunc(
      function(x){x$
          multiply(0.0001)$
          copyProperties(x,x$propertyNames())
      }
    )
  )

############################################################################

modis_ndvi_tidy <- as_tidyee(modis_ndvi) #as tidy object


############################ ####Monthly NDVI MEAN ##########################

monthly_baseline <- modis_ndvi_tidy |>                                           # Historical
  filter(year %in% 2000:2015) |> 
  group_by(month) |> 
  summarise(stat=list("mean","sd"))


ndvi_recent_monthly <- modis_ndvi_tidy |>                                        # RECENT 
  filter(year %in% c(2016:2022)) |> 
  group_by(year,month) |> 
  summarise(
    stat="mean"
  )

####################################################################

ndvi_recent_renamed <- ndvi_recent_monthly |> 
  dplyr::select(NDVI="NDVI_mean")


ndvi_recent_and_baseline<- inner_join(x = ndvi_recent_renamed,
                                      y = monthly_baseline,
                                      by = "month")


ndvi_recent_baseline_imageCol <- ndvi_recent_and_baseline |> 
  as_ee()

ndvi_zscore<- ndvi_recent_baseline_imageCol$map(
  function(img){
    zscore<- img$expression(
      "float((NDVI-NDVI_mean)/(NDVI_stdDev))",
      opt_map= list(NDVI= img$select("NDVI"),
                    NDVI_mean= img$select("NDVI_mean"),
                    NDVI_stdDev= img$select("NDVI_stdDev")
      )
    )$rename("NDVI_z_score")
    img$select("NDVI","NDVI_mean")$addBands(zscore)
  }
  
) 
ndvi_z <- as_tidyee(ndvi_zscore)

#
ndvi_z_pre_processed <- ndvi_z |> 
  filter(year>=2022) |> 
  dplyr::select("NDVI_z_score")

# leaflet::leaflet(grid ) |>
#   leaflet::addTiles() |> 
#   leaflet::addPolygons()


ndvi_final <- list()

first_tier<-1:10000 
second_tier <- 10001:20000
third_tier <- 20001:30000
fourth_tier <- 30001:40000
fifth_tier <- 40001:43648

all_tier <- c("first_tier","second_tier","third_tier","fourth_tier","fifth_tier")

for ( i in all_tier){
  
  grid_filter <- grid[get(i),]
  # 1000o is working 
  ndvi_final[[i]] <- ndvi_z_pre_processed %>%  
    ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 250,via = "drive")
}



ndvi_final_bind <- do.call("bind_rows",ndvi_final)
ndvi_final_bind <- ndvi_final_bind %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date,label = T,abbr = F)
)

write.csv(ndvi_final_bind,"05_outputs/02_NDVI_Z_value/NDVI_Z_value.csv")


