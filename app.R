options(repos = c(CRAN = "https://cloud.r-project.org"))  # Or another CRAN mirror URL

# Load all necessary libraries
packages <- c("readxl", "dplyr", "dataRetrieval", "shiny", "plotly", "rstudioapi", "tidyr", "tidyverse", "pdftools", "rmarkdown", "ggpubr", "grid", "gridExtra")

# Function to check and install missing packages
install_missing_packages <- function(packages) {
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) {
    install.packages(new.packages, repos = c(CRAN = "https://cloud.r-project.org"))  # Set CRAN mirror here
  }
}

# Install missing packages
install_missing_packages(packages)

# Explicitly load readxl
library(readxl)

# Load all packages at once
lapply(packages, library, character.only = TRUE)

# Define the data directory
data_dir <- "data"

# Define file paths relative to the data directory
File_DamOps_2015 <- file.path(data_dir, "CRR 2015 Gates.xlsx")
File_DamOps_2023 <- file.path(data_dir, "CRR 2023 Gates.xlsx")
File_Licking_2023 <- file.path(data_dir, "Licking_WaterTemp_2023.xlsx")
File_SFLicking_2015 <- file.path(data_dir, "SFLicking_WaterTemp2_2015.xlsx")
File_Licking_2015 <- file.path(data_dir, "Licking_WaterTemp_cage_2015.xlsx")
File_SouthFork_2023 <- file.path(data_dir, "SouthFork_WaterTemp_2023.xlsx")


# Preprocessing Gate Data ----------------------------------------------------
# Function to import and clean data
process_gate_data <- function(file_path) {
  df <- read_excel(file_path, skip = 7)  # Skip first 7 rows
  
  # Rename columns explicitly based on observed structure
  colnames(df) <- c("Ignore", "DateTime_GMT", "CaveRun_BP1", "CaveRun_BP2", "CaveRun_MG1")
  
  # Select only the necessary columns
  df <- df %>%
    select(DateTime_GMT, CaveRun_BP1, CaveRun_BP2, CaveRun_MG1) %>%
    mutate(DateTime_GMT = as.POSIXct(DateTime_GMT, format = "%m-%d-%Y %H:%M:%S", tz = "GMT"))  # Convert to proper datetime
  
  return(df)
}

# Process both datasets
DamOps_2015 <- process_gate_data(File_DamOps_2015)
DamOps_2023 <- process_gate_data(File_DamOps_2023)


# Preprocessing USGS Gages --------------------------------------------------------------
# Define parameters for flow USGS gauges
site_numbers <- c("03253000", "03253500")  # USGS site numbers
parameter_code <- "00060"  # Discharge (cfs)
start_dates <- c("2015-01-01", "2023-01-01")
end_dates <- c("2015-12-31", "2023-12-31")

# Function to retrieve and process 15-minute interval flow data
get_flow_data <- function(site, start_date, end_date) {
  readNWISuv(siteNumbers = site, parameterCd = parameter_code, startDate = start_date, endDate = end_date) %>%
    rename(DateTime_GMT = dateTime, Flow_cfs = X_00060_00000) %>%
    mutate(DateTime_GMT = as.POSIXct(DateTime_GMT, tz = "GMT"))  # Convert to proper datetime
}

# Retrieve data for each gauge and year
Flow_2015_Hayes <- get_flow_data("03253000", "2015-01-01", "2015-12-31")
Flow_2023_Hayes <- get_flow_data("03253000", "2023-01-01", "2023-12-31")
Flow_2015_Catawba <- get_flow_data("03253500", "2015-01-01", "2015-12-31")
Flow_2023_Catawba <- get_flow_data("03253500", "2023-01-01", "2023-12-31")

# USGS site numbers for "natural" source comparisons
site_numbers_2023 <- c("03307000", "03265000", "03321500", "03290500", "03287500") # Russell Creek, Stillwater River, Green River at Lock 1, Kentucky River Lock 2, Kentucky River Lock 4
site_numbers_2015 <- c("03307000", "03321500") # Russell Creek, Green River at Lock 1
parameter_code <- "00010"  # Temperature (Celsius)

# Function to retrieve and process 15-minute interval temperature data
get_temperature_data <- function(site, start_date, end_date) {
  readNWISuv(siteNumbers = site, parameterCd = parameter_code, startDate = start_date, endDate = end_date) %>%
    select(dateTime, X_00010_00000) %>%  # Select only dateTime and X_00010_00000
    rename(DateTime_GMT = dateTime, Temp_Celsius = X_00010_00000) %>%  # Rename columns
    mutate(DateTime_GMT = as.POSIXct(DateTime_GMT, tz = "GMT"))  # Convert to proper datetime
}

# Retrieve 2023 data
Temperature_2023_Russell_Creek <- get_temperature_data("03307000", "2023-01-01", "2023-12-31")
Temperature_2023_Stillwater <- get_temperature_data("03265000", "2023-01-01", "2023-12-31")
Temperature_2023_Green_River_atLock1 <- get_temperature_data("03321500", "2023-01-01", "2023-12-31")
Temperature_2023_Kentucky_Lock2 <- get_temperature_data("03290500", "2023-01-01", "2023-12-31")
Temperature_2023_Kentucky_Lock4 <- get_temperature_data("03287500", "2023-01-01", "2023-12-31")

# Retrieve 2015 data
Temperature_2015_Russell_Creek <- get_temperature_data("03307000", "2015-01-01", "2015-12-31")
Temperature_2015_Green_River_atLock1 <- get_temperature_data("03321500", "2015-01-01", "2015-12-31")


# Preprocessing Logger Data -----------------------------------------------
# Function to process water temperature data
process_temp_data <- function(file_path, start_row) {
  # Read the first sheet, starting from start_row and skipping the first rows
  df <- read_excel(file_path, skip = start_row - 1, col_names = FALSE) %>%
    select(1, 3)  # Select columns A (1st) and C (3rd)
  
  # Rename the columns
  colnames(df) <- c("DateTime_GMT", "Temp_Celsius")
  
  # Convert DateTime_GMT from 12-hour format with AM/PM to 24-hour format
  df$DateTime_GMT <- as.POSIXct(df$DateTime_GMT, format = "%m/%d/%Y %I:%M:%S %p", tz = "GMT")
  
  return(df)
}

# Process the water temperature data
Licking_2023 <- process_temp_data(File_Licking_2023, start_row = 2)
SFLicking_2015 <- process_temp_data(File_SFLicking_2015, start_row = 32)
Licking_2015 <- process_temp_data(File_Licking_2015, start_row = 32)
SouthFork_2023 <- process_temp_data(File_SouthFork_2023, start_row = 2)


# Shiny App ---------------------------------------------------------------
# Define UI for application
ui <- fluidPage(
  titlePanel("Dam Operations, Temperature, and Flow Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = c("2015", "2023")),
      
      checkboxGroupInput("temp_sources", "Select Temperature Sources:",
                         choices = c("South Fork" = "SFLicking", 
                                     "Licking" = "Licking",
                                     "Russell Creek" = "Russell Creek",
                                     "Green River @ Lock 1" = "Green River @ Lock 1",
                                     "Stillwater" = "Stillwater",
                                     "Kentucky Lock 2" = "Kentucky Lock 2",
                                     "Kentucky Lock 4" = "Kentucky Lock 4"),
                         selected = c("SFLicking", "Licking")),
      
      checkboxGroupInput("flow_sources", "Select Flow Sources:",
                         choices = c("Catawba (USGS)" = "Catawba", 
                                     "Hayes (USGS)" = "Hayes"),
                         selected = c("Catawba", "Hayes")),
      
      hr(),
      h4("Metadata"),
      p("Coordinates for the Licking and South Fork 2015 and 2023 Locations"),
      p("Licking (2015): 38.78980, -84.36740"),
      p("Licking (2023): 38.76801, -84.34460"),
      p("South Fork (2015 and 2023): 37.47938, -83.67592"),
      
      p("Gauges used for Catawba and Hayes"),
      p("Hayes (USGS 03253000)"),
      p("Catawba (USGS 03253500)"),
      
      p("Gagues used for 'natural' source comparisons"),
      p("Russell Creek (USGS 03307000"),
      p("Green River at Lock 1 (USGS 03321500"),
      p("Stillwater at Pleasant Hill, OH (USGS 03265000"),
      p("Kentucky River at Lock 2 (USGS 03290500"),
      p("Kentucky River at Lock 4 (USGS 03287500")
    ),
    
    mainPanel(
      div(style = "display: flex; flex-direction: column; align-items: center;",
          plotlyOutput("dam_ops_plot", height = "400px", width = "100%"),
          plotlyOutput("temp_plot", height = "400px", width = "100%"),
          plotlyOutput("flow_plot", height = "400px", width = "100%")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Ensure app closes cleanly
  onStop(function() {
    cat("Shiny app closing successfully.\n")
  })
  
  # Define color mapping for sources
  source_colors <- c(
    "South Fork" = "#1f77b4",          # Blue
    "Licking" = "#ff7f0e",            # Orange
    "Russell Creek" = "#9467bd",     # Purple
    "Green River @ Lock 1" = "#8c564b", # Brown
    "Stillwater" = "#e377c2",         # Pink
    "Kentucky Lock 2" = "#bcbd22",     # Yellowish-green
    "Kentucky Lock 4" = "#17becf",     # Cyan
    "Catawba (USGS)" = "#2ca02c",     # Green
    "Hayes (USGS)" = "#d62728"        # Red
  )
  
  # Reactive expression for dam operations data
  dam_data <- reactive({
    if (input$year == "2015") {
      DamOps_2015 %>%
        filter(DateTime_GMT >= "2015-06-01" & DateTime_GMT <= "2015-09-30")
    } else {
      DamOps_2023 %>%
        filter(DateTime_GMT >= "2023-06-01" & DateTime_GMT <= "2023-09-30")
    }
  })
  
  # Reactive expression for temperature data
  temp_data <- reactive({
    temp_df <- data.frame()
    
    if ("SFLicking" %in% input$temp_sources) {
      if (input$year == "2015") {
        temp_df <- bind_rows(temp_df, SFLicking_2015 %>%
                               filter(DateTime_GMT >= "2015-06-01" & DateTime_GMT <= "2015-09-30") %>%
                               mutate(Source = "South Fork"))
      } else {
        temp_df <- bind_rows(temp_df, SouthFork_2023 %>%
                               filter(DateTime_GMT >= "2023-06-01" & DateTime_GMT <= "2023-09-30") %>%
                               mutate(Source = "South Fork"))
      }
    }
    
    if ("Licking" %in% input$temp_sources) {
      if (input$year == "2015") {
        temp_df <- bind_rows(temp_df, Licking_2015 %>%
                               filter(DateTime_GMT >= "2015-06-01" & DateTime_GMT <= "2015-09-30") %>%
                               mutate(Source = "Licking"))
      } else {
        temp_df <- bind_rows(temp_df, Licking_2023 %>%
                               filter(DateTime_GMT >= "2023-06-01" & DateTime_GMT <= "2023-09-30") %>%
                               mutate(Source = "Licking"))
      }
    }
    
    if ("Russell Creek" %in% input$temp_sources) {
      if (input$year == "2015") {
        temp_df <- bind_rows(temp_df, Temperature_2015_Russell_Creek %>%
                               filter(DateTime_GMT >= "2015-06-01" & DateTime_GMT <= "2015-09-30") %>%
                               mutate(Source = "Russell Creek"))
      } else {
        temp_df <- bind_rows(temp_df, Temperature_2023_Russell_Creek %>%
                               filter(DateTime_GMT >= "2023-06-01" & DateTime_GMT <= "2023-09-30") %>%
                               mutate(Source = "Russell Creek"))
      }
    }
    
    if ("Green River @ Lock 1" %in% input$temp_sources) {
      if (input$year == "2015") {
        temp_df <- bind_rows(temp_df, Temperature_2015_Green_River_atLock1 %>%
                               filter(DateTime_GMT >= "2015-06-01" & DateTime_GMT <= "2015-09-30") %>%
                               mutate(Source = "Green River @ Lock 1"))
      } else {
        temp_df <- bind_rows(temp_df, Temperature_2023_Green_River_atLock1 %>%
                               filter(DateTime_GMT >= "2023-06-01" & DateTime_GMT <= "2023-09-30") %>%
                               mutate(Source = "Green River @ Lock 1"))
      }
    }
    
    if (input$year == "2023") {
      if ("Stillwater" %in% input$temp_sources) {
        temp_df <- bind_rows(temp_df, Temperature_2023_Stillwater %>%
                               filter(DateTime_GMT >= "2023-06-01" & DateTime_GMT <= "2023-09-30") %>%
                               mutate(Source = "Stillwater"))
      }
      
      if ("Kentucky Lock 2" %in% input$temp_sources) {
        temp_df <- bind_rows(temp_df, Temperature_2023_Kentucky_Lock2 %>%
                               filter(DateTime_GMT >= "2023-06-01" & DateTime_GMT <= "2023-09-30") %>%
                               mutate(Source = "Kentucky Lock 2"))
      }
      
      if ("Kentucky Lock 4" %in% input$temp_sources) {
        temp_df <- bind_rows(temp_df, Temperature_2023_Kentucky_Lock4 %>%
                               filter(DateTime_GMT >= "2023-06-01" & DateTime_GMT <= "2023-09-30") %>%
                               mutate(Source = "Kentucky Lock 4"))
      }
    }
    
    return(temp_df)
  })
  
  # Reactive expression for flow data
  flow_data <- reactive({
    flow_df <- data.frame()
    
    if ("Catawba" %in% input$flow_sources) {
      if (input$year == "2015") {
        flow_df <- bind_rows(flow_df, Flow_2015_Catawba %>%
                               filter(DateTime_GMT >= "2015-06-01" & DateTime_GMT <= "2015-09-30") %>%
                               mutate(Source = "Catawba (USGS)"))
      } else {
        flow_df <- bind_rows(flow_df, Flow_2023_Catawba %>%
                               filter(DateTime_GMT >= "2023-06-01" & DateTime_GMT <= "2023-09-30") %>%
                               mutate(Source = "Catawba (USGS)"))
      }
    }
    
    if ("Hayes" %in% input$flow_sources) {
      if (input$year == "2015") {
        flow_df <- bind_rows(flow_df, Flow_2015_Hayes %>%
                               filter(DateTime_GMT >= "2015-06-01" & DateTime_GMT <= "2015-09-30") %>%
                               mutate(Source = "Hayes (USGS)"))
      } else {
        flow_df <- bind_rows(flow_df, Flow_2023_Hayes %>%
                               filter(DateTime_GMT >= "2023-06-01" & DateTime_GMT <= "2023-09-30") %>%
                               mutate(Source = "Hayes (USGS)"))
      }
    }
    return(flow_df)
  })
  
  # Determine the common x-axis range
  x_axis_range <- reactive({
    c(min(c(dam_data()$DateTime_GMT, temp_data()$DateTime_GMT, flow_data()$DateTime_GMT), na.rm = TRUE),
      max(c(dam_data()$DateTime_GMT, temp_data()$DateTime_GMT, flow_data()$DateTime_GMT), na.rm = TRUE))
  })
  
  # Render plots
  output$dam_ops_plot <- renderPlotly({
    plot_ly(dam_data(), x = ~DateTime_GMT) %>%
      add_trace(y = ~CaveRun_BP1, type = 'scatter', mode = 'lines', name = 'BP1') %>%
      add_trace(y = ~CaveRun_BP2, type = 'scatter', mode = 'lines', name = 'BP2') %>%
      add_trace(y = ~CaveRun_MG1, type = 'scatter', mode = 'lines', name = 'MG1') %>%
      layout(title = "Dam Operations", xaxis = list(range = x_axis_range()))
  })
  
  output$temp_plot <- renderPlotly({
    plot_ly(temp_data(), x = ~DateTime_GMT, y = ~Temp_Celsius, color = ~Source, colors = source_colors, type = 'scatter', mode = 'lines') %>%
      layout(title = "Temperature Data", xaxis = list(range = x_axis_range()))
  })
  
  output$flow_plot <- renderPlotly({
    plot_ly(flow_data(), x = ~DateTime_GMT, y = ~Flow_cfs, color = ~Source, colors = source_colors, type = 'scatter', mode = 'lines') %>%
      layout(title = "Flow Data", xaxis = list(range = x_axis_range()))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

