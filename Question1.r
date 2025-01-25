df <- read.table("Group_Assignment_Dataset_test.txt", header = T, sep = ",")

library(dplyr)
library(zoo)
library(ggplot2)

df <- df %>% mutate(across(where(is.numeric), ~ na.approx(.x, na.rm = FALSE)))

print('linear interpolation done')
class(df)

# Apply Z-score calculation to each numeric feature
calculate_z_scores <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
z_scores <- df %>% mutate(across(where(is.numeric), calculate_z_scores))

print("Z-score calculation done")

# Identify anomalies where |z| > 3
anomalies <- z_scores %>% mutate(across(where(is.numeric), ~ abs(.x) > 3, .names = "anomaly_{col}"))
num_anomalies <- anomalies %>% mutate(across("anomaly_Global_active_power", as.numeric)) %>% mutate(across("anomaly_Global_reactive_power", as.numeric)) %>% mutate(across("anomaly_Voltage", as.numeric)) %>% mutate(across("anomaly_Global_intensity", as.numeric)) %>% mutate(across("anomaly_Sub_metering_1", as.numeric)) %>% mutate(across("anomaly_Sub_metering_2", as.numeric)) %>% mutate(across("anomaly_Sub_metering_3", as.numeric))
num_anomalies <- num_anomalies %>% select(-Date, -Time, -Global_active_power, -Global_reactive_power, -Voltage, -Global_intensity, -Sub_metering_1, -Sub_metering_2, -Sub_metering_3)
print("anomaly detection done")

# Calculate the percentage of anomalies for each feature
anomaly_percentages <- colMeans(num_anomalies, na.rm = TRUE) * 100

# Calculate the overall percentage of anomalies
overall_anomaly_percentage <- mean(as.matrix(num_anomalies), na.rm = TRUE) * 100

# Print results
print("Anomaly percentages for each feature:")
print(anomaly_percentages)
print(paste("Overall anomaly percentage:", overall_anomaly_percentage))

# Use as.POSIXlt to extract week information


# Filter data for the 21st week


# Extract data from Monday to Sunday
