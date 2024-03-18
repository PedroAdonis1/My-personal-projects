library(ggplot2)
library(dplyr)
library(readr)

# needs to be replaced to the fixed path of each environment!!
file_path <- "/Users/peterfalterbaum/Library/CloudStorage/OneDrive-Personal/business topics/master/ims/T1/Statistics/project/StatsProject/Proj. Data Factory 2/submission/chocolate_Factory_Panel_Data_final.csv"

pt()
plot_numeric_feature_over_years <- function(file_path, feature_name) {
  # Read the dataset
  chocolate_data <- read_csv(file_path)
  
  # Convert Year to a date or numeric format if necessary
  chocolate_data$Year <- as.numeric(as.character(chocolate_data$Year))
  
  
  # Create a line chart for the specific feature over the years with a trend line and data points for each room
  p <- ggplot(chocolate_data, aes(x = Year, y = .data[[feature_name]])) +
    geom_smooth(method = "lm", se = FALSE, color = rgb(190/255, 214/255, 47/255), aes(group = 1)) +  # Trend line color and ensure one trend line for all points
    geom_point() +  # Add data points
    theme_minimal() +
    labs(title = bquote(bold("Scatter of"~.(feature_name)~"Over the Years")),
         x = "Year",
         y = feature_name)
  
  # Print the plot
  print(p)
}




numeric_features <- c(
  "ChocolateProductionInKg",
  "NumberOfOompaLoompas",
  "RawMaterialConsumptionInKg",
  "OperationalCostInDollars",
  "EnergyConsumption",
  "OompaLoompaSatisfaction"
)


plot_numeric_feature_over_years_totals <- function(file_path, feature_name) {
  # Read the dataset
  chocolate_data <- read_csv(file_path)
  
  # Convert Year to a date or numeric format if necessary
  chocolate_data$Year <- as.numeric(as.character(chocolate_data$Year))
  
  # Summarize the data by year to calculate totals for the specific feature
  summarized_data <- chocolate_data %>%
    group_by(Year) %>%
    summarize(Total = sum(.data[[feature_name]], na.rm = TRUE)) %>%
    ungroup()
  
  # Create a line chart for the specific feature over the years with a trend line and data points
  p <- ggplot(summarized_data, aes(x = Year, y = Total)) +
    geom_smooth(method = "lm", se = FALSE, color = rgb(190/255, 214/255, 47/255)) +  # Trend line color
    geom_point() +  # Add data points
    theme_minimal() +
    labs(title = bquote(bold("Total"~.(feature_name)~"Over the Years")),
         x = "Year",
         y = "")
  
  # Print the plot
  print(p)
}

plot_numeric_feature_over_years_average <- function(file_path, feature_name) {
  chocolate_data <- read_csv(file_path)
  
  # Convert Year to a date or numeric format if necessary
  chocolate_data$Year <- as.numeric(as.character(chocolate_data$Year))
  
  # Summarize the data by year to calculate the average for the specific feature
  summarized_data <- chocolate_data %>%
    group_by(Year) %>%
    summarize(Average = mean(.data[[feature_name]], na.rm = TRUE)) %>%
    ungroup()
  
  # Create a line chart for the specific feature over the years with a trend line and data points
  p <- ggplot(summarized_data, aes(x = Year, y = Average)) +
    geom_smooth(method = "lm", se = FALSE, color = rgb(190/255, 214/255, 47/255)) +  # Trend line color
    geom_point() +  # Add data points
    theme_minimal() +
    labs(title = bquote(bold("Yearly Average"~.(feature_name)~"Per Room")),
         x = "Year",
         y = "") +
    theme(plot.title = element_text(size = 12))
  
  # Print the plot
  print(p)
}

# Loop through the list of numeric features and generate line charts for each
for (feature in numeric_features) {
  plot_numeric_feature_over_years_average(file_path, feature)
}

