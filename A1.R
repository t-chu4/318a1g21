library(zoo)
library(dplyr)
library(ggplot2)
library(corrplot)

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
filtered_data$Time <- format(as.POSIXct(filtered_data$Time, format = "%H:%M:%S"), "%H:%M:%S")
startTime <- "07:30:00"
endTime <- "17:00:00"

# Add column for Day of Week
filtered_data$DayofWeek <- weekdays(filtered_data$Date)

# Add column for Weekday/Weekend classification
filtered_data$dayType <- ifelse(filtered_data$DayofWeek %in% c("Saturday", "Sunday"), "Weekend", "Weekdays")

# Create different df for day/night of weekday/weekend
day_weekday <- filtered_data %>%
  filter(dayType == "Weekdays" & Time >= startTime & Time <= endTime)

night_weekday_before_start <- filtered_data %>%
  filter(dayType == "Weekdays" & Time < startTime)

night_weekday_after_end <- filtered_data %>%
  filter(dayType == "Weekdays" & Time > endTime)

day_weekend <- filtered_data %>%
  filter(dayType == "Weekend" & Time >= startTime & Time <= endTime)

night_weekend_before_start <- filtered_data %>%
  filter(dayType == "Weekend" & Time < startTime)

night_weekend_after_end <- filtered_data %>%
  filter(dayType == "Weekend" & Time > endTime)

# Create different df to calculate avg of day/night of weekday/weekend
avg_day_weekday <- day_weekday %>%
  group_by(Time) %>%
  summarize(Average_Global_intensity = mean(Global_intensity, na.rm = TRUE))

avg_night_weekday_before_start <- night_weekday_before_start %>%
  group_by(Time) %>%
  summarize(Average_Global_intensity = mean(Global_intensity, na.rm = TRUE))

avg_night_weekday_after_end <- night_weekday_after_end %>%
  group_by(Time) %>%
  summarize(Average_Global_intensity = mean(Global_intensity, na.rm = TRUE))

avg_day_weekend <- day_weekend %>%
  group_by(Time) %>%
  summarize(Average_Global_intensity = mean(Global_intensity, na.rm = TRUE))

avg_night_weekend_before_start <- night_weekend_before_start %>%
  group_by(Time) %>%
  summarize(Average_Global_intensity = mean(Global_intensity, na.rm = TRUE))

avg_night_weekend_after_end <- night_weekend_after_end %>%
  group_by(Time) %>%
  summarize(Average_Global_intensity = mean(Global_intensity, na.rm = TRUE))


# Convert Time to numeric (seconds since midnight)
convert_time_to_minutes <- function(time) {
  (as.numeric(as.POSIXct(time, format="%H:%M:%S")) - as.numeric(as.POSIXct("00:00:00", format="%H:%M:%S"))) / 60
}

avg_day_weekday$Time_Seconds <- convert_time_to_minutes(avg_day_weekday$Time)
avg_night_weekday_before_start$Time_Seconds <- convert_time_to_minutes(avg_night_weekday_before_start$Time)
avg_night_weekday_after_end$Time_Seconds <- convert_time_to_minutes(avg_night_weekday_after_end$Time)
avg_day_weekend$Time_Seconds <- convert_time_to_minutes(avg_day_weekend$Time)
avg_night_weekend_before_start$Time_Seconds <- convert_time_to_minutes(avg_night_weekend_before_start$Time)
avg_night_weekend_after_end$Time_Seconds <- convert_time_to_minutes(avg_night_weekend_after_end$Time)


avg_day_weekday$Category <- "Day - Weekdays"
avg_night_weekday_before_start$Category <- "Night - Before - Weekdays"
avg_night_weekday_after_end$Category <- "Night - After - Weekdays"
avg_day_weekend$Category <- "Day - Weekends"
avg_night_weekend_before_start$Category <- "Night - Before - Weekends"
avg_night_weekend_after_end$Category <- "Night - After - Weekends"


# Merge into a single dataframe
combined_data <- bind_rows(avg_day_weekday, avg_night_weekday_before_start, avg_night_weekday_after_end, avg_day_weekend, avg_night_weekend_before_start, avg_night_weekend_after_end)

#Plot Linear Regression
p1 <- ggplot(combined_data, aes(x = Time_Seconds, y = Average_Global_intensity, color = Category)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(title = "Linear Regression of Global Intensity",
       x = "Time (Minuntes since Midnight)",
       y = "Average Global Intensity")

#Plot Polynomial Regression
p2 <- ggplot(combined_data, aes(x = Time_Seconds, y = Average_Global_intensity, color = Category)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, linewidth = 1) +  
  labs(title = "Polynomial Regression (Degree 2) of Global Intensity",
       x = "Time (Minutes since Midnight)",
       y = "Average Global Intensity")

library(gridExtra)
grid.arrange(p1, p2, ncol=1)
