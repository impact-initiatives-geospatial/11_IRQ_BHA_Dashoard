rm(list = ls())
library(reticulate)
library(sf)
library(rgee)
library(surveyGEER)
library(tidyrgee)
library(tidyverse)



current_year = 2022

# read cluster ------------------------------------------------------------

grid <- st_read("01_inputs/01_hexagon/cluster_10_km.shp") %>% st_transform(4326)



# ee initiating  ----------------------------------------------------------

# ee_clean_pyenv()
# ee_install_set_pyenv("04_py_env/Scripts/python.exe")

ee_Initialize(user = "Mehedi",drive = T,gcs = T)



###################### Setup service accout ##################################################

# SaK_file <- "03_tor_dap_proposal/sak_file.json" # PUT HERE THE FULLNAME OF YOUR SAK.
# 
# # Assign the SaK to a EE user.
# ee_utils_sak_copy(
#   sakfile =  SaK_file,
#   users = c("Mehedi") # Unlike GD, we can use the same SaK for multiple users.
# )
# 
# ee_utils_sak_validate()


##########################################################################
# 
# project_id <- ee_get_earthengine_path() %>% 
#   list.files(., "\\.json$", full.names = TRUE) %>% 
#   jsonlite::read_json() %>% 
#   '$'(project_id)
# 
# googleCloudStorageR::gcs_create_bucket("mh_rgee_bha", projectId = project_id)


############################# READING CHIRPS DATA ############################################


CHIRPS <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$select("precipitation")$
  map(ee_utils_pyfunc(
    function(x){x$copyProperties(x,x$propertyNames())}
  )) # read CHIRPS with property info


chirps_tidy<- as_tidyee(CHIRPS) # make tidygee object

################################ Monthly PRECIPITATION MEAN ##########################
previous_year <- current_year-1

historical_mean <- chirps_tidy %>% 
  filter(year %in% 1981:previous_year) %>% 
  group_by(month) %>% 
  summarise(stat = "mean")

historical_mean_renamed <- historical_mean %>% select (mean_his_precipitation = "precipitation_mean")


current_year_mean <- chirps_tidy %>% filter(
  year == current_year
) %>% group_by(month) %>% summarise(stat = "mean")

current_year_mean_renamed <- current_year_mean %>% select(mean_current_precipitation = "precipitation_mean")


################################ JOIN HISTORICAL AND CURRENT PRECIPITATION ########################

precipitation_recent_and_historical<- inner_join(x = current_year_mean_renamed,
                                      y = historical_mean_renamed,
                                      by = "month")

precipitation_recent_and_historical_ee <- precipitation_recent_and_historical %>%  
  as_ee()



precipitaion_defict <- precipitation_recent_and_historical_ee$map(
  function(img){
    precipitation_defict <- img$expression(
      "float(current_year_mean_renamed - historical_mean_renamed)",
      opt_map= list(current_year_mean_renamed= img$select("mean_current_precipitation"),
                    historical_mean_renamed= img$select("mean_his_precipitation")
      )
    )$rename("precipitation_defict")
    img$select("mean_current_precipitation","mean_his_precipitation")$addBands(precipitation_defict)
  }
) %>% as_tidyee()



precipitaion_defict_processed <- precipitaion_defict %>% filter(
  year > previous_year
) %>% select("precipitation_defict")

precipitation_defict_final <- list()

first_tier<-1:10000 
second_tier <- 10001:20000
third_tier <- 20001:30000
fourth_tier <- 30001:40000
fifth_tier <- 40001:43648

all_tier <- c("first_tier","second_tier","third_tier","fourth_tier","fifth_tier")

for ( i in all_tier){

grid_filter <- grid[get(i),]
# 1000o is working 
precipitation_defict_final[[i]] <- precipitaion_defict_processed %>%  
  ee_extract_tidy(y = grid_filter,sf = T,stat = "mean",scale = 5500,via = "drive")
}

precipitation_defic_bind <- do.call("bind_rows",precipitation_defict_final)
precipitation_defic_bind <- precipitation_defic_bind %>% mutate(
  year = lubridate::year(date),
  month = lubridate::month(date,label = T,abbr = F)
)

write.csv(precipitation_defic_bind,"05_outputs/01_precipitation_defict/precipitation_deficit.csv")
