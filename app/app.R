
# --------------- Load library

library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(shinycssloaders) 

options(spinner.color = "#c1adea") # Choose Spinner colour

# ------------------------------ Map data

# --- Read-in data

# Point coordinates of all schools
school_sf <- readRDS(here::here("data/school_sf.rds"))

# Polygon mapping of Statistical Areas in Queensland
sa4_polygons <- read_rds(here::here("data/sa4_polygons.rds"))

## Extract lat and lon from geometry
school_sf <- school_sf %>%
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


# ------------------------------ Enrolments data

# Read-in data
mathsci_all <- read_csv(here::here("data/mathsci_all.csv"),
                        col_types = cols(doe_centre_code = col_factor()))

# Convert data types
mathsci_all <- mathsci_all %>% 
  mutate(across(.cols = c(qcaa_subject_id, 
                          subject_type,
                          qcaa_school_id,
                          sector,
                          school_postcode,
  ),
  ~ as.factor(.x))) 

# Remove schools that existed before year 2002 and not after
mathsci_all <- mathsci_all %>% 
  filter(!is.na(doe_centre_code)) 

# Convert to long format for plotting purposes
mathsci_all_long <- mathsci_all %>%
  pivot_longer(cols = c(year_11_enrolments, year_12_enrolments),
               names_to = "unit",
               values_to = "enrolments")


# ------------------------------ Web application


# --- User interface
ui <- fluidPage(
  
  # Suppress all errors
  tags$style(type = "text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"), 
  
  # Add default theme
  theme = shinythemes::shinytheme("cerulean"),
  
  br(),
  
  sidebarLayout(sidebarPanel(
    h1("Schools"),
    
    # --- Drop down menu to select subject
    pickerInput(
      inputId = "subject",
      label = "Select Subject",
      choices = sort(unique(mathsci_all_long$subject_name))
    ),
    
    # --- Drop down menu to select school
    pickerInput(
      inputId = "school",
      label = "Select School",
      choices = unique(mathsci_all_long$school_name),
      options = list(`live-search` = TRUE)
    ),
    
    # --- Drop down menu to select districts
    
    pickerInput(
      inputId = "district",
      label = "Select district(s)", 
      choices = sort(unique(mathsci_all_long$qcaa_district)),
      selected = unique(mathsci_all_long$qcaa_district),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    ),
    
    leaflet::leafletOutput("map")
  ),
  
  mainPanel(
    br(), 
    # Time plot of enrolments
    plotlyOutput("time_plot")
  )
  )
  
  
)

# --------------------------------- Server output

server <- function(input, output, session) {
  
  # -------------- Update select input
  
  rval_all_schools <- reactive({
    
    # If no district is chosen
    if(is.null(input$district)){
      
      # All schools offering selected subject
      all_schools <- mathsci_all_long %>% 
        filter(subject_name == input$subject,
               unit == "year_12_enrolments") %>% 
        distinct(school_name) %>% 
        pull()
      
      # If selected school offers selected subject
      if(input$school %in% all_schools){
        mathsci_all_long %>% 
          filter(subject_name == input$subject,
                 school_name == input$school,
                 unit == "year_12_enrolments")
      }
      
      # If selected school does not offer selected subject
      else{
        mathsci_all_long %>% 
          filter(subject_name == input$subject,
                 unit == "year_12_enrolments")
      }
      
      
      # If at least one district is chosen
    } else{
      
      mathsci_all_long %>%
        filter(subject_name == input$subject
               & qcaa_district %in% input$district
               & unit == "year_12_enrolments") %>%
        # Arrange school names in alphabetical order
        arrange(school_name, qcaa_district)
      
    }
    
    
  })
  
  # --- Select input filters to schools that offer the selected subject
  observe({
    
    # Filter to all schools that offer the selected subject
    school_choice <- unique(rval_all_schools()$school_name)
    
    # Update school input
    updatePickerInput(session = session,
                      inputId = "school",
                      choices = school_choice)
    
  })
  
  # ---------------- Time plot
  
  # --- Reactive data frame for selected district and subject
  
  rval_district <- reactive({
    
    mathsci_all_long %>%
      filter(subject_name == input$subject
             & qcaa_district %in% input$district
             & unit == "year_12_enrolments") %>%
      # Fill missing years with NA values
      tidyr::complete(completion_year = min(completion_year):max(completion_year),
                      fill = list(value = NA)) %>%
      group_by(qcaa_school_id)
    
    
  })
  
  # --- Reactive data frame for selected school
  rval_selected_school <- reactive({
    
    mathsci_all_long %>% 
      filter(subject_name == input$subject 
             & school_name == input$school
             & unit == "year_12_enrolments") %>% 
      # Fill missing years with NA values
      tidyr::complete(completion_year = min(completion_year):max(completion_year),
                      fill = list(value = NA))
    
  })
  
  # --- Time plot for enrolments 
  output$time_plot <- renderPlotly({
    
    # If no district is chosen
    if(is.null(input$district)){
      
      # Remove all schools except selected school
      plot_ly(x = ~ completion_year,
              y = ~ enrolments,
              hoverinfo = "text",
              showlegend = FALSE,
              text = ~ paste0("<b>Expected Graduation</b>: ", completion_year, "<br>",
                              "<b>School</b>: ", school_name, "<br>",
                              "<b>District</b>: ", qcaa_district, "<br>",
                              "<b>Enrolments</b>: ", enrolments)) %>%
        # Line plot for selected school
        add_lines(data = rval_selected_school(),
                  color = I("steelblue")) %>%
        layout(hovermode = "x",
               title = "Year 12 Enrolments",
               xaxis = list(title = "Completion year",
                            tickvals = seq(1992, 2022, by = 2)),
               yaxis = list(title = "Enrolments"))
      
      # If at least one district is chosen
    } else{
      
      plot_ly(x = ~ completion_year,
              y = ~ enrolments,
              hoverinfo = "text",
              showlegend = FALSE,
              text = ~ paste0("<b>Expected Graduation</b>: ", completion_year, "<br>",
                              "<b>School</b>: ", school_name, "<br>",
                              "<b>District</b>: ", qcaa_district, "<br>",
                              "<b>Enrolments</b>: ", enrolments)) %>%
        # Line plot for all schools
        add_lines(data = rval_district(),
                  color = I("grey80"),
                  alpha = 0.4,
                  hoverinfo = "none") %>% 
        # # Line plot for selected school
        add_lines(data = rval_selected_school(),
                  color = I("steelblue")) %>%
        layout(hovermode = "x",
               title = list(text = "Year 12 Enrolments",
                            x = 0.05),
               xaxis = list(title = "Completion year",
                            tickvals = seq(1992, 2022, by = 2)),
               yaxis = list(title = "Enrolments"))
      
    }
  })
  
  
  # ---------------- Leaflet map
  
  rval_school_sf <- reactive({
    
    # Extract all schools offering selected subject
    all_schools <- mathsci_all_long %>% 
      filter(subject_name == input$subject) %>% 
      distinct(school_name) %>% 
      pull()
    
    # --- Extract point coordinates of schools offering subject
    
    # If no district is selected, filter to selected school
    if(is.null(input$district)){
      school_sf %>% 
        filter(school_name == input$school)
      
      # If one or more district is selected
    } else{
      school_sf %>% 
        filter(school_name %in% all_schools,
               qcaa_district %in% input$district)
    }
    
  })
  
  output$map <- leaflet::renderLeaflet({
    
    if(is.null(input$district)){
      map <- rval_school_sf() %>% 
        
        leaflet() %>% 
        
        # --- Set bounding box to show map of selected school
        
        setView(lng = sf::st_coordinates(rval_school_sf())[,1],
                lat = sf::st_coordinates(rval_school_sf())[,2],
                zoom = 8) 
    } else{
      
      map <- rval_school_sf() %>% 
        
        leaflet() %>% 
        
        # --- Set bounding box to show map of all schools in selected district
        
        fitBounds(lng1 = min(rval_school_sf()$lon), lat1 = min(rval_school_sf()$lat),
                  lng2 = max(rval_school_sf()$lon), lat2 = max(rval_school_sf()$lat)) 
    }
    
    
    map %>% 
      
      # --- Overlay base groups (toggle between map provider tiles)
      
      # Default: CartoDB
      addProviderTiles("CartoDB", group = "CartoDB") %>%
      
      # Open Street Map
      addProviderTiles(provider = "OpenStreetMap", group = "OSM") %>%
      
      # Esri
      addProviderTiles("Esri", group = "Esri") %>%
      
      # Add layering groups
      addLayersControl(baseGroups = c("CartoDB", "OSM", "Esri")) %>%
      
      # --- Add points (allow user to select which districts to display)
      
      addCircleMarkers(data = rval_school_sf(), # Reactive in app
                       color = I("steelblue"),
                       radius = 1.5,
                       popup = ~ paste0("<b>School</b>: ", school_name, "<br>",
                                        "<b>District</b>: ", qcaa_district, "<br>",
                                        "<b>Postcode</b>: ", school_postcode),
                       layerId = ~ school_name,
                       label = ~ htmltools::htmlEscape(paste0(school_name, " (", qcaa_district, ")"))) %>% 
      
      # --- Allow user to reset to default view
      
      leaflet.extras::addResetMapButton() %>% 
      
      # --- Add search feature options (Allow user to search for school)
      
      addMarkers(data = rval_school_sf(),
                 group = "schools",
                 label = rval_school_sf()$school_name,
                 # make custom icon so small it cannot be seen
                 icon = makeIcon(
                   iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                   iconWidth = 1,
                   iconHeight = 1)) %>%
      
      addSearchFeatures(targetGroups = "schools",
                        options = searchFeaturesOptions(zoom = 10,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE)) 
      
      # --- Add polygons
      
      # Polygon lines
      # addPolylines(data = sa4_polygons,
      #              color = I("black"),
      #              weight = 2) %>%
      # 
      # # Set hover corresponding to the SA4 for each polygons
      # addPolygons(data = sa4_polygons, 
      #             label = ~ paste0(SA4_NAME21),
    #             color = I("white"),
    #             opacity = 0) 
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
    updateSelectInput(session = session,
                      inputId = "school",
                      selected = click$id)
  })
  
  
  rval_selected <- reactive({
    school_sf %>% filter(school_name == input$school)
  })
  
  observe({
    
    leafletProxy("map") %>% 
      flyTo(lng = sf::st_coordinates(rval_selected())[,1],
            lat = sf::st_coordinates(rval_selected())[,2], 
            zoom = 5)
    
  })
  
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>% 
      addCircleMarkers(data = rval_school_sf(), # Reactive in app
                       color = I("steelblue"),
                       radius = 1.5,
                       popup = ~ paste0("<b>School</b>: ", school_name, "<br>",
                                        "<b>District</b>: ", qcaa_district, "<br>",
                                        "<b>Postcode</b>: ", school_postcode),
                       layerId = ~ school_name,
                       label = ~ htmltools::htmlEscape(paste0(school_name, " (", qcaa_district, ")"))) %>% 
      addCircleMarkers(data = rval_selected(),
                       color = I("red"),
                       radius = 1.5,
                       popup = ~ paste0("<b>School</b>: ", school_name, "<br>",
                                        "<b>District</b>: ", qcaa_district, "<br>",
                                        "<b>Postcode</b>: ", school_postcode),
                       label = ~ htmltools::htmlEscape(paste0(school_name, " (", qcaa_district, ")")))
  })
  
}

# ------------------------------ Run the application  
shinyApp(ui = ui, server = server)

