rm(list = ls())

# library -----------------------------------------------------------------


library(shiny)
library(shinyWidgets)
library(shinybrowser)
library(leaflet)
library(sf)
library(dplyr)
library(leaflet.extras)
library(htmltools)
library(leafgl)





# read_data ---------------------------------------------------------------

grid <- st_read("01_inputs/01_hexagon/cluster_10_km.shp") %>% st_transform(4326)


# indicator list ----------------------------------------------------------
indicator_list <- data.frame(
  name = c("Precipitation deficit","3-months SPI","9-months SPI","12-months SPI","NDVI anomaly","NDWI anomaly","Temperature anomaly"),
  group = c("Precipitation","Precipitation","Precipitation","Precipitation","Vegetation","Vegetation","Temerature")
)


###### input from gee
Precipitation_deficit <- read.csv("05_outputs/01_precipitation_defict/precipitation_deficit.csv")
NDVI_anomaly <- read.csv("05_outputs/02_NDVI_Z_value/NDVI_Z_value.csv")
NDWI_anomaly <- read.csv("05_outputs/03_NDWI_Z_value/NDWI_Z_value.csv")
SPI_12_month <-  read.csv("05_outputs/01_precipitation_defict/twelve_month_spi.csv")
SPI_9_month <-  read.csv("05_outputs/01_precipitation_defict/nine_month_spi.csv")
SPI_3_month <-  read.csv("05_outputs/01_precipitation_defict/three_month_spi.csv")

Temperature_anomaly <- read.csv("05_outputs/04_temperature_Z_value/temperature_Z_value.csv")


##### Admin boundary 
admin_boundary_path <-  "01_inputs/03_admin_boundary/"

admin_zero <-st_read(paste0(admin_boundary_path,"/irq_admbnda_adm0_cso_itos_20190603.shp"))
admin1_boundary  <-st_read(paste0(admin_boundary_path,"/irq_admbnda_adm1_cso_20190603.shp"))
# admin2_boundary  <-st_read(paste0(admin_boundary_path,"/irq_admbnda_adm2_cso_20190603.shp"))
# admin3_boundary  <-st_read(paste0(admin_boundary_path,"/irq_admbnda_adm3_cso_20190603.shp"))






# leaflet base map --------------------------------------------------------

base_map <- leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  leaflet::addPolygons(data = admin_zero,color = "#EE5859",fillColor = "transparent") %>% 
  leaflet::addPolygons(data = admin1_boundary,color = "#58585A",
                       #label = ~htmlEscape(ADM1_EN),
                       #labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = "15px"),
                       #popup = paste("Governorate:", admin1_boundary$ADM1_EN),
                       weight = 2,dashArray = "12",fillColor = "transparent")

# ui ---------------------------------------------------------------------


ui <-fluidPage(
    

# Styling -----------------------------------------------------------------
    
    tags$head(
      HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'), includeCSS("style.css")
    ),
   
    
    navbarPage(
      
      windowTitle = "IRAQ CLIMATE MONITORING DASHBOARD",
      HTML('<a style="padding-left:10px;" class = "navbar-brand" href = "https://www.reach-initiative.org" target="_blank"><img src = "reach_logo.png" height = "50"></a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>IRAQ CLIMATE MONITORING DASHBOARD</strong></span>'),
      
      
      
      tabPanel("Introduction",
               mainPanel(width = 12,
                         br(),
                         
                         h4(strong("Background: ")),
                            p(style="text-align: justify; font-size: 15px;", 
                              " Iraq is classified as the fifth most vulnerable country in the world to climate change. The country faces serious challenges like increasing temperatures and declining rainfall. in the past year, many organizations and agencies have awakened to the looming crisis of water scarcity and so are leading research to better understand and prepare in their areas of intervention but there is no countrywide climate monitoring or predictive tool that can support humanitarian agencies or the government to support and prioritize the area of intervention to deal with climate change. To mitigate the gap, ACF and REACH prepared this dashboard to monitor the different climate indicators and predict them indicators. " 
                            ),
                        
                         hr(),
                         
                         h4(strong("Methodology:")),
                         p(style="text-align: justify; font-size: 15px;",
                           "To monitor the climate indicator, a remote sensing tool has been used. Data from 8 indicators are collected from google earth engine API. All the climate indicators are aggregated to 10 km by 10 km Hexagon as it is assumed that there will almost no variation within this range. The indicators Specific details with some background to understand the outputs are given below- "
                         ),
                         
                          # p(style="text-align: justify; font-size: 15px;", 
                           tags$ol(
                           tags$li(em(strong("Precipitation Deficit:")), 
                                   "The precipitation deficit was calculated using CHIRPS data for the period between 1981 – Current year (2022). Initially, the monthly average for 1981 to 2021 and 2022 was calculated, and then the 2022 monthly average was subtracted from the historical average."
                                   ),
                           
                           p(style="text-align: justify; font-size: 15px;",
                           tags$li(em(strong("SPI:")), 
                            "The Standardized Precipitation Index (SPI) is the most commonly used indicator worldwide for detecting and characterizing meteorological droughts.  Meteorological drought is defined as a period with an abnormal precipitation deficit, in relation to the long-term average conditions for a region. The SPI indicator shows the anomalies (deviations from the mean) of the observed total precipitation, for any given location and accumulation period of interest. The magnitude of the departure from the mean is a probabilistic measure of the severity of a wet or dry event. For any given region, increasingly severe rainfall deficits (i.e.,meteorological droughts) are indicated as SPI decreases below ‒1.0, while increasingly severe excess rainfall are indicated as SPI increases above 1.0. "
                           ,
                            tags$ul( # list order of different spi
                              tags$li(em("3-months SPI:"), 
                                      "When SPI is computed for shorter accumulation periods (e.g., 1 to 3 months), it can be used as an indicator for immediate impacts such as reduced soil moisture, snowpack, and flow in smaller creeks."
                              ), # end 3 month spi   
                            
                              tags$li(em("9-months SPI:"), 
                                      "When SPI is computed for medium accumulation periods (e.g., 3 to 12 months), it can be used as an indicator for reduced stream flow and reservoir storage."
                              ), # end 6 month spi   
                              
                              
                              tags$li(em("12-months SPI:"), 
                               "When SPI is computed for longer accumulation periods (e.g., 12 to 48 months), it can be used as an indicator for reduced reservoir and groundwater recharge. "       
                              )
                              
                              
                              ) # end SPIs ul list
                            
                            
                            ) # end tag$li for SPI
                           
                           ), # end p()
                         
                         
                        
                         
                         
                         p(style="text-align: justify; font-size: 15px;",
                           tags$li(em(strong("NDVI Anomaly: ")), 
                                   "The Normalized Difference Vegetation Index (NDVI) measures how dense and green plant leaves are which suggests overall vegetative health. Scientists calculate average NDVI values over a span of years to find out what is normal at each time of year. They then compare the current NDVI value for each day of year to the average computed over the set of base years to determine if the areas are more or less productive than the average. This comparison is called the NDVI Anomaly. MODIS 16 days of NDVI data has been used to calculate the NDVI Anomaly considering 2000-2015 as the base year. "
                           )),
                         
                             
                             
                             p(style="text-align: justify; font-size: 15px;",
                               tags$li(em(strong("NDWI Anomaly: ")), 
                                       "The Normalized Difference Water Index (NDWI) is known to be strongly related to the plant water content. It is therefore a very good proxy for plant water stress. MODIS 16 days NDWI data has been used to calculate the NDWI Anomaly considering 2000-2015 as the base year. "      
                               )),
                               
                               
                               
                               p(style="text-align: justify; font-size: 15px;", 
                                 tags$li(em(strong("Temperature Anomaly: ")), 
                                         "A temperature anomaly is the departure, positive or negative, of a temperature from a base temperature that is normally chosen as an average of temperatures over a certain reference period, often called a base period. MODIS 16 days temperature data has been used to calculate temperature anomaly."      
                                 ))
                         
                         ), # tag$ol  
                        
                         
                         
                         hr(),
                         
                         h4(strong("Limitation:")),
                     
                         p(style="text-align: justify; font-size: 14px;", 
                          "For NDVI and NDWI crop masking were not considered for now however crop masking process in on going!"),
                      
                      
                         
                         hr(),
                         
                         h4(strong("Contact:"),tags$br(),
                            p("Md. Mehedi Hasan Khan",br(),
                              "GIS Specialist",br(),
                              "Email:", tags$a(href="mailto:mh.khan@reach-initiative.org","mh.khan@reach-initiative.org"))
                         )
                         
                         
                     
                         
                          
                        
               ) # end main panel 0
      ), # end tab panel 0 
      
      
      
      
      
  tabPanel("Climate Indicators",
      mainPanel(width = 12,
        br(),
        h5(strong("Climate change is affecting Iraq very adversely. To help the humanitarian community, REACH has developed this climate monitoring dashboard, so that the implementing agencies can work immediately wherever there is a hotspot")),
        
        hr(),
        
        ##################### input ###############################
        tags$div(pickerInput("select_climate_indicator",
                             label = "Select Climate Indicator:",
                             choices = lapply(split(indicator_list$name, indicator_list$group), as.list),
                             selected = "Temperature anomaly",
                             multiple = F,
                             options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
        ),style="display:inline-block"),
        
        tags$div(pickerInput("select_month",
                             label = "Select Month:",
                             choices = NULL,
                             selected = NULL,
                             multiple = F,
                             options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
        ),style="display:inline-block"),
        
        actionButton("run", "Show result"), 
        
        
        div(class = "outer", tags$style(type = "text/css", ".outer {position: fixed; top: 200px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
        
            leafglOutput("map",width ="100%", height = "100%"))
        
        
      ) # end main panel  
  ), # tab 1 end 
  
  tabPanel("Global Surface Water",
       mainPanel(

         htmlOutput("frame"),
         em("Source: Joint Research Centre (JRC)"),
       ) # end mainpanel 2   
           
  ), # End table 2
  
  
  tabPanel("ACF water scracity anticipatory model",
           mainPanel(
             
             htmlOutput("frame_acf_model"),
             em("Source: ACF/UoM"),
           ) # end mainpanel 3   
           
  ) # End table 3
  
      ) # end navar page
) # end fluid page



# server ------------------------------------------------------------------



server <- function(input, output,session){
  
  

  
  
  indicator <-  reactive({
    
    case_when(input$select_climate_indicator == "NDVI anomaly"~ "NDVI_anomaly", 
              
              input$select_climate_indicator == "NDWI anomaly" ~ "NDWI_anomaly",  
              
              input$select_climate_indicator == "Temperature anomaly"~"Temperature_anomaly" ,  
              
              input$select_climate_indicator == "Precipitation deficit"~"Precipitation_deficit" ,  
              input$select_climate_indicator == "12-months SPI"~ "SPI_12_month",
              input$select_climate_indicator == "9-months SPI"~ "SPI_9_month",
              input$select_climate_indicator == "3-months SPI"~ "SPI_3_month")  
              
  })
  
  
  
  
  
  
  month_selected <-  reactive({input$select_month})
  
  indicator_df1 <- reactive({get(indicator())})


  
  ####################### available month in name in the selected cilamte indicator ############
  
  available_month <- reactive({indicator_df1()$month %>% unique()})
  
  observe({
    updatePickerInput(session, "select_month", choices = paste0(c(available_month(),"Yearly Average (Till last month)")))
  })
  
  
  #################### yearly average #######################################
  
  
  
  indicator_df <- eventReactive(input$run,{

    if(input$select_month == "Yearly Average (Till last month)"){

      indicator_df_avg <- indicator_df1() %>% dplyr::group_by(FID) %>% dplyr::summarise(
        value = mean(value,na.rm= T)
      ) %>% dplyr::mutate(
        month = "Yearly Average (Till last month)",
        parameter = unique(indicator_df1()$parameter)
      )

      df <- indicator_df1() %>% dplyr::bind_rows(indicator_df_avg)
    }
    else {
      df <- indicator_df1()
    }
    return(df)
  })

  
  
  
  
  
  
  ########################## FILTER AND JOIN ###########################
  df <- eventReactive(input$run,{indicator_df() %>% filter(month == month_selected())})
  
  grid_with_df <- eventReactive(input$run,{grid %>% left_join(df(),by = "FID") %>% filter(!is.na(value))})
  


# values for coloring ------------------------------------------------------

  
  grid_with_df1 <- eventReactive(input$run,{
    
    
    if(input$select_climate_indicator %in%  c("Precipitation deficit","12-months SPI","9-months SPI","3-months SPI")) {
      
      grid_with_df1 <- grid_with_df() %>% mutate(
        value2 = case_when(value < -2 ~ 0,
                           value < -1.5 ~ 1,
                           value < -1 ~ 2,
                           value < 1 ~ 3,
                           value < 1.5 ~4,
                           value < 2 ~ 5, 
                           value >= 2~6))
      
      return(grid_with_df1)
    }
    
    
    
    if(input$select_climate_indicator %in%  c("Temperature anomaly")) {
      
      grid_with_df1 <- grid_with_df() %>% mutate(
        value2 = value)
      
      return(grid_with_df1)
    }

    
    if(input$select_climate_indicator %in%  c("NDWI anomaly","NDVI anomaly")) {
      
      grid_with_df1 <- grid_with_df() %>% mutate(
        value2 = value)
      
      return(grid_with_df1)
    }    

    
  }) 
  
  
    

# color -------------------------------------------------------------------


clr <- eventReactive(input$run,{

  
  
  if(input$select_climate_indicator %in%  c("Precipitation deficit","12-months SPI","9-months SPI","3-months SPI")) {
    bin <- c(0,1,2,3,4,5,6)

    clr <- colorBin(c("#F28282",
                      "#F5A6A6" ,
                      "#F9C6C7",
                      "#FFFFFF",
                      "#DCE5EF",
                      "#A1BCD7",
                      "#568CBC"
    ), domain =  grid_with_df1()$value2, bins = bin)
    return(clr)
  }
  
  
  
  
  if(input$select_climate_indicator %in% c("Temperature anomaly")) {
    bin <- c(-Inf,-2,-1,-.5,.5,1,2,Inf)
    clr <- colorBin(c("#367BB3",
                      "#729DC6" ,
                      "#A1BCD7",
                      "#FFFFFF",
                      "#eac435",
                      "#f3b700",
                      "#dd7230"
                      # "#d00000"
    ), domain =  grid_with_df1()$value2, bins = bin)
    return(clr)
  }
  
  
  if(input$select_climate_indicator %in% c("NDWI anomaly","NDVI anomaly")) {
    bin <- c(-Inf,-2,-1,1,2,Inf)
    clr <- colorBin(c("#f2022a",
                      "#fc4765",
                      "#FFFFFF",
                      "#71f575",
                      "#00971B"), domain =  grid_with_df1()$value2, bins = bin)
    return(clr)
  }

    
  })
  
  

# legend ------------------------------------------------------------------

  
  
  add_legend_df <- eventReactive(input$run,{

    
    if(input$select_climate_indicator %in%  c("Precipitation deficit","12-months SPI","9-months SPI","3-months SPI")) {
      
      add_legend_df <- list(
        color = c("#F28282", "#F5A6A6" ,"#F9C6C7", 
                  "#FFFFFF", 
                  "#DCE5EF", "#A1BCD7","#568CBC"),
        label = c("2.00 and less (extremely dry)","-1.50 to -1.99 (severely dry),","-1.00 to -1.49 (moderately dry)",
                  "-0.99 to +0.99 (near normal)",
                  "+1.00 to +1.49 (moderately wet)","+1.50 to +1.99 (very wet)","+2.00 and above (extremely wet)"),
        unit = "Legend"
      ) 
      
      return(add_legend_df)
    }
    
    
    
    
    if(input$select_climate_indicator %in% c("Temperature anomaly")) {
      
      
      add_legend_df <- list(
        color = c( "#367BB3", "#729DC6", "#A1BCD7", "#FFFFFF", "#eac435", 
                  "#f3b700", "#dd7230"),
        label = c("Less than -2","-1.99 to -1","-.99 to -.5", "-.49 to .5 (nearly normal)",".51 to 1","1 to 2","Greater than 2"),
        
      unit = "Legend (Degree celcious)")
      
      
      return(add_legend_df)
    }
    
    
    if(input$select_climate_indicator %in% c("NDWI anomaly","NDVI anomaly")) {

      
      add_legend_df <- list(
        color = c("#f2022a", "#fc4765", "#FFFFFF", "#71f575", "#00971B"),
        label = c("Less than -2 (extremely dryer)","-1.99 to -1 (dryer)","-.99 to .99 (nearly normal)", "1 to 2 (wetter)", "Greater than 2 (extremely wetter)"),
        
      unit = "Legend")
      
      
      
      return(add_legend_df)
    }

    
  }) 
  
  
  
  
  
  
  
  
  ##############################################################################
  
  
  
  
  
  output$map <-  renderLeaflet({
    
    base_map %>%  leafgl::addGlPolygons(data = grid_with_df1(),
                                        fillOpacity = 0.75,
                                        color =   ~clr()(grid_with_df1()$value2),
                                        stroke = F,
                                        popup = paste("Row_id:", grid_with_df1()$FID, "<br>",
                                                      "Parameter:", grid_with_df1()$parameter, "<br>",
                                                      "Value:",  grid_with_df1()$value)
                                        
    )  %>% setView(lat = 33.312805,lng = 44.361488,zoom = 6) %>% addMiniMap() %>% 
      addSearchOSM()%>% 
      # addLegend("topright",pal = clr(), values = grid_with_df()$value)
      addLegend(
        colors = add_legend_df()$color,
        labels =add_legend_df()$label,
        opacity = 1, 
        title = add_legend_df()$unit
      )
    
    
  })
  
   frame <- tags$iframe(src="https://global-surface-water.appspot.com/map", style="height: 100vh;",scrolling = 'no',width="150%", frameborder = "0")
    
    output$frame <- renderUI({
     frame
      })
    
    
    frame_acf_model <- tags$iframe(src="https://docs.google.com/spreadsheets/d/e/2PACX-1vTrpOkXJSINvpomxOgQOlvmmkdVy29uL5KPn_r7fGN2FU4nPo4VTCI_IbuBaUfAX735RVjyeC0cWkzI/pubhtml", style="height: 100vh;",scrolling = 'no',width="150%", frameborder = "0")
    
    output$frame_acf_model <- renderUI({
      frame_acf_model
    })
    
}


## App 
shinyApp(ui = ui, server = server)
