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

grid <- st_read("01_inputs/01_hexagon/cluster_10_km.shp") %>% st_transform(4326)



# initiatin gee -----------------------------------------------------------
# 

# ee_clean_pyenv()
# ee_install_set_pyenv("04_py_env/Scripts/python.exe")

ee_Initialize(user = "Mehedi",drive = T,gcs = T)


# ndwi --------------------------------------------------------------------

############################## READ modis data ###########################

modis_link <- "MODIS/MOD09GA_006_NDWI"
modisIC <- ee$ImageCollection(modis_link)



modis_ndWi <- modisIC$
  select("NDWI")$
  map(
    ee_utils_pyfunc(
      function(x){x$
          multiply(0.0001)$
          copyProperties(x,x$propertyNames())
      }
    )
  )

############################################################################

modis_ndWi_tidy <- as_tidyee(modis_ndWi) #as tidy object


############################ ####Monthly NDwI MEAN ##########################

monthly_baseline <- modis_ndWi_tidy |>                                           # Historical
  filter(year %in% 2000:2015) |> 
  group_by(month) |> 
  summarise(stat=list("mean","sd"))


ndWi_recent_monthly <- modis_ndWi_tidy |>                                        # RECENT 
  filter(year %in% c(2016:2022)) |> 
  group_by(year,month) |> 
  summarise(
    stat="mean"
  )

####################################################################

ndWi_recent_renamed <- ndWi_recent_monthly |> 
  dplyr::select(NDWI="NDWI_mean")


ndWi_recent_and_baseline<- inner_join(x = ndWi_recent_renamed,
                                      y = monthly_baseline,
                                      by = "month")


ndWi_recent_baseline_imageCol <- ndWi_recent_and_baseline |> 
  as_ee()

ndWi_zscore<- ndWi_recent_baseline_imageCol$map(
  function(img){
    zscore<- img$expression(
      "float((NDWI-NDWI_mean)/(NDWI_stdDev))",
      opt_map= list(NDWI= img$select("NDWI"),
                    NDWI_mean= img$select("NDWI_mean"),
                    NDWI_stdDev= img$select("NDWI_stdDev")
      )
    )$rename("NDWI_z_score")
    img$select("NDWI","NDWI_mean")$addBands(zscore)
  }
  
) 
ndWi_z <- as_tidyee(ndWi_zscore)

#
ndWi_z_pre_processed <- ndWi_z |> 
  filter(year>=2022) |> 
  dplyr::select("NDWI_z_score")

# leaflet::leaflet(grid ) |>
#   leaflet::addTiles() |> 
#   leaflet::addPolygons()


ndWi_final <- list()

first_tier<-1:10000 
second_tier <- 10001:20000
third_tier <- 20001:30000
fourth_tier <- 30001:40000
fifth_tier <- 40001:43648

all_tier <- c("first_tier","second_tier","third_tier","fourth_tier","fifth_tier")

for ( i in all_tier){
  
  grid_filter <- grid[get(i),]
  # 1000o is working 
  ndWi_final[[i]] <- ndWi_z_pre_processed %>%  
    ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 250,via = "drive")
}



ndWi_final_bind <- do.call("bind_rows",ndWi_final)
ndWi_final_bind <- ndWi_final_bind %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date,label = T,abbr = F)
)

write.csv(ndWi_final_bind,"05_outputs/03_NDWI_Z_value//NDWI_Z_value.csv")


