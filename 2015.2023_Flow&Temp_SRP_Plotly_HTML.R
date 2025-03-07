library(plotly)
library(htmlwidgets)
library(dplyr)

# Define color mapping for sources
source_colors <- c(
  "South Fork" = "#1f77b4",   # Blue
  "Licking" = "#ff7f0e",    # Orange
  "Catawba (USGS)" = "#2ca02c", # Green
  "Hayes (USGS)" = "#d62728"  # Red
)

# Filter data for the required date range
date_range_2015 <- c("2015-06-01", "2015-09-30")
date_range_2023 <- c("2023-06-01", "2023-09-30")

# --- 2015 Data ---
dam_ops_2015 <- DamOps_2015 %>% filter(DateTime_GMT >= date_range_2015[1] & DateTime_GMT <= date_range_2015[2])
temp_2015 <- bind_rows(
  SFLicking_2015 %>% filter(DateTime_GMT >= date_range_2015[1] & DateTime_GMT <= date_range_2015[2]) %>% mutate(Source = "South Fork"),
  Licking_2015 %>% filter(DateTime_GMT >= date_range_2015[1] & DateTime_GMT <= date_range_2015[2]) %>% mutate(Source = "Licking")
)
flow_2015 <- bind_rows(
  Flow_2015_Catawba %>% filter(DateTime_GMT >= date_range_2015[1] & DateTime_GMT <= date_range_2015[2]) %>% mutate(Source = "Catawba (USGS)"),
  Flow_2015_Hayes %>% filter(DateTime_GMT >= date_range_2015[1] & DateTime_GMT <= date_range_2015[2]) %>% mutate(Source = "Hayes (USGS)")
)

# --- 2023 Data ---
dam_ops_2023 <- DamOps_2023 %>% filter(DateTime_GMT >= date_range_2023[1] & DateTime_GMT <= date_range_2023[2])
temp_2023 <- bind_rows(
  SouthFork_2023 %>% filter(DateTime_GMT >= date_range_2023[1] & DateTime_GMT <= date_range_2023[2]) %>% mutate(Source = "South Fork"),
  Licking_2023 %>% filter(DateTime_GMT >= date_range_2023[1] & DateTime_GMT <= date_range_2023[2]) %>% mutate(Source = "Licking")
)
flow_2023 <- bind_rows(
  Flow_2023_Catawba %>% filter(DateTime_GMT >= date_range_2023[1] & DateTime_GMT <= date_range_2023[2]) %>% mutate(Source = "Catawba (USGS)"),
  Flow_2023_Hayes %>% filter(DateTime_GMT >= date_range_2023[1] & DateTime_GMT <= date_range_2023[2]) %>% mutate(Source = "Hayes (USGS)")
)

# Define the common x-axis range
x_range_2015 <- range(c(dam_ops_2015$DateTime_GMT, temp_2015$DateTime_GMT, flow_2015$DateTime_GMT), na.rm = TRUE)
x_range_2023 <- range(c(dam_ops_2023$DateTime_GMT, temp_2023$DateTime_GMT, flow_2023$DateTime_GMT), na.rm = TRUE)

# --- Create 2015 Plots ---
dam_ops_plot_2015 <- plot_ly(dam_ops_2015, x = ~DateTime_GMT) %>%
  add_trace(y = ~CaveRun_BP1, type = 'scatter', mode = 'lines', name = 'BP1') %>%
  add_trace(y = ~CaveRun_BP2, type = 'scatter', mode = 'lines', name = 'BP2') %>%
  add_trace(y = ~CaveRun_MG1, type = 'scatter', mode = 'lines', name = 'MG1') %>%
  layout(title = "Dam Operations (2015)", xaxis = list(range = x_range_2015))

temp_plot_2015 <- plot_ly(temp_2015, x = ~DateTime_GMT, y = ~Temp_Celsius, color = ~Source, colors = source_colors, type = 'scatter', mode = 'lines') %>%
  layout(title = "Temperature Data (2015)", xaxis = list(range = x_range_2015))

flow_plot_2015 <- plot_ly(flow_2015, x = ~DateTime_GMT, y = ~Flow_cfs, color = ~Source, colors = source_colors, type = 'scatter', mode = 'lines') %>%
  layout(title = "Flow Data (2015)", xaxis = list(range = x_range_2015))

# --- Create 2023 Plots ---
dam_ops_plot_2023 <- plot_ly(dam_ops_2023, x = ~DateTime_GMT) %>%
  add_trace(y = ~CaveRun_BP1, type = 'scatter', mode = 'lines', name = 'BP1') %>%
  add_trace(y = ~CaveRun_BP2, type = 'scatter', mode = 'lines', name = 'BP2') %>%
  add_trace(y = ~CaveRun_MG1, type = 'scatter', mode = 'lines', name = 'MG1') %>%
  layout(title = "Dam Operations (2023)", xaxis = list(range = x_range_2023))

temp_plot_2023 <- plot_ly(temp_2023, x = ~DateTime_GMT, y = ~Temp_Celsius, color = ~Source, colors = source_colors, type = 'scatter', mode = 'lines') %>%
  layout(title = "Temperature Data (2023)", xaxis = list(range = x_range_2023))

flow_plot_2023 <- plot_ly(flow_2023, x = ~DateTime_GMT, y = ~Flow_cfs, color = ~Source, colors = source_colors, type = 'scatter', mode = 'lines') %>%
  layout(title = "Flow Data (2023)", xaxis = list(range = x_range_2023))

# --- Combine All Plots in a 3-row x 2-column Grid Layout ---
final_plot <- subplot(
  dam_ops_plot_2015, dam_ops_plot_2023,
  temp_plot_2015, temp_plot_2023,
  flow_plot_2015, flow_plot_2023,
  nrows = 3, titleY = TRUE
) %>% layout(title = "Dam Operations, Temperature, and Flow Data Comparison")

# Define the output directory
output_dir <- "O:/ED/Private/Water Quality/Co-op & Others Oversight/Matthew Schaaf/Data Loggers Analysis/Wendell_SRP_Flow&Temp_Relationship/2015.2023_Flow.Temp.DamOps_SRP/html_output"

# Construct the full file path
output_file <- file.path(output_dir, "Dam_Temp_Flow_Plots.html")

# Save as an HTML file for sharing
saveWidget(final_plot, output_file, selfcontained = TRUE, title = "Dam Operations, Temperature, and Flow Data Comparison")

cat("HTML file 'Dam_Temp_Flow_Plots.html' created successfully in", output_dir, "\n")

