library(zoo)
library(dplyr)
library(ggplot2)

# Read dataset
data <- read.table("Group_Assignment_Dataset.txt", header=TRUE, sep = ",")

# PART 1
# Handle missing data using Linear Interpolation
data <- data %>% mutate(across(where(is.numeric), ~na.approx(.x, na.rm=FALSE)))

# Detect point anomalies
compute_zscore <- function(x){
  (x - mean(x, na.rm=TRUE))/ sd(x, na.rm=TRUE)
}

z_scores <- data %>% mutate(across(where(is.numeric), compute_zscore))

# Identify anomalies (|Z| > 3)
anomalies <- z_scores %>% mutate(across(where(is.numeric), ~ abs(.x) > 3, .names = "anomaly_{col}"))
num_anomalies <- anomalies %>% mutate(across("anomaly_Global_active_power", as.numeric)) %>% mutate(across("anomaly_Global_reactive_power", as.numeric)) %>% mutate(across("anomaly_Voltage", as.numeric)) %>% mutate(across("anomaly_Global_intensity", as.numeric)) %>% mutate(across("anomaly_Sub_metering_1", as.numeric)) %>% mutate(across("anomaly_Sub_metering_2", as.numeric)) %>% mutate(across("anomaly_Sub_metering_3", as.numeric))
num_anomalies <- num_anomalies %>% select(-Date, -Time, -Global_active_power, -Global_reactive_power, -Voltage, -Global_intensity, -Sub_metering_1, -Sub_metering_2, -Sub_metering_3)

# Calculate the percentage of anomalies for each feature
anomaly_percentages <- colMeans(num_anomalies, na.rm = TRUE) * 100

# Calculate the overall percentage of anomalies
overall_anomaly_percentage <- mean(as.matrix(num_anomalies), na.rm = TRUE) * 100

print(anomaly_percentages)

# Use as.POSIXlt to extract week information
data$Date <- as.POSIXlt(data$Date, format = "%d/%m/%Y")

# Filter data for the 21st week
group_number <- 21
start_date <- as.Date("2007-1-1") + (group_number - 1) * 7
end_date <- start_date + 7

data %>% filter(between(Date, as.Date(start_date), as.Date(end_date)))


# PART 2
filtered_data <- data %>% filter(between(Date, as.Date(start_date), as.Date(end_date)))

# Columns, minus date/time
data_vars <- c("Global_active_power",
               "Global_reactive_power", 
               "Voltage",
               "Global_intensity", 
               "Sub_metering_1",
               "Sub_metering_2",
               "Sub_metering_3")

# Precautionary, remove empty things
filtered_data <- na.omit(filtered_data)

# Matrix calculated with command taken directly from assignment info
matrix <- cor(filtered_data[data_vars], method = "pearson")

# Visualize matrix
png("matrix.png", width = 500, height = 500)

corrplot(matrix,
         method = "color",
         type = "upper", 
         tl.col = "black",
         tl.srt = 45, 
         col = colorRampPalette(c("blue", "white", "red"))(200))

print(matrix)


# PART 3

