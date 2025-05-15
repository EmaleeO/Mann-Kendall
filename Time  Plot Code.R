
plot_analyte_timeseries_by_location <- function(data, date_col, chemical_col, location_col, location, analyte, action_level, value_col) {
  # Load required libraries
  library(ggplot2)
  library(dplyr)
  
  
  
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }
  # Filter the data for the specified location and analyte
  filtered_data <- data %>%
    filter(!!sym(location_col) == location, 
           !!sym(chemical_col) == analyte) %>%
    mutate(!!sym(date_col) := as.Date(!!sym(date_col), format = "%m/%d/%Y"))
  
  # Check for "<" in the values and convert those values to half the number
  value_col_data <- filtered_data %>% pull(!!sym(value_col))
  matches <- grepl("<", value_col_data)
  value_col_data[matches] <- as.numeric(sub("<", "", value_col_data[matches])) / 2
  
  filtered_data <- filtered_data %>% 
    mutate(!!sym(value_col) := as.numeric(value_col_data),
           detect_type = ifelse(matches, "Non-detect", "Detect"))  # Add a new column to distinguish between detects and non-detects
  
  # Create a time series plot
  ggplot(filtered_data, aes_string(x = date_col, y = value_col)) +
    geom_line(color = "black") +                          # Connecting line between data points
    geom_point(aes(shape = detect_type, fill = detect_type), size = 3, color = "black") +        # Dots for all data points
    geom_hline(yintercept = action_level, color = "orange", linetype = "dashed") +  # Horizontal line for action level
    scale_shape_manual(name = "Detection Type", values = c("Detect" = 16, "Non-detect" = 8)) +  # Custom shapes for points
    scale_fill_manual(values = c("Detect" = "blue", "Non-detect" = "orange")) +  # Custom fill colors for points
    ggtitle(paste("Carbon Tetrachloride", "at", location)) +
    xlab("Date") +
    ylab("Value") +
    theme_minimal() +
    scale_x_date(breaks = as.Date(c("2010-01-01", "2015-01-01", "2020-01-01", "2025-01-01")), date_labels = "%Y", limits = as.Date(c("2007-01-01", "2025-12-31"))) +
    scale_y_continuous(trans = "log10", limits = c(0.01, 150),  # Logarithmic scale starting from 0.01
                       breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + 
    coord_cartesian(xlim = as.Date(c("2007-01-01", "2025-12-31"))) +  # Set x-axis limits from 2007 to 2024
    theme(plot.title = element_text(hjust = 0.5, size =18),
          legend.position = "none",  # Remove the legend
          axis.title = element_text(size = 16),  # Increase font size for axis titles
          axis.text = element_text(size = 14))
  
}

plot_analyte_timeseries_by_location(
  data = sharpe,
  date_col = "Date",
  chemical_col = "Chemical",
  location_col = "Fil",
  location = "EWB4",
  analyte = "Carbon tetrachloride",
  action_level = 5,
  value_col = "Value"
)


