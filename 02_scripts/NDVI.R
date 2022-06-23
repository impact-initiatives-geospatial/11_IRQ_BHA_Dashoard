rm(list = ls())

library(reticulate)
use_python("04_py_env/Scripts/python.exe")

library(reticulate)
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(tidyverse)

# read cluster ------------------------------------------------------------

grid <- st_read("01_inputs/01_hexagon/cluster_10_km.shp")
grid <- grid[1:10,]                                                              # to make things faster, temporary 


# initiatin gee -----------------------------------------------------------
# 

# ee_clean_pyenv()
# ee_install_set_pyenv("04_py_env/Scripts/python.exe")

ee_Initialize()


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
  select(NDVI="NDVI_mean")


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
  select("NDVI_z_score")

# leaflet::leaflet(grid ) |>
#   leaflet::addTiles() |> 
#   leaflet::addPolygons()



grid_with_ndvi <- ndvi_z_pre_processed |> 
    ee_extract_tidy(y = grid,stat = "mean",scale = 250,via = "getinfo",)

