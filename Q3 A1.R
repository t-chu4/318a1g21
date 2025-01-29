library(ggplot2)
library(dplyr)

# Need to change the file location
setwd("E:/Spring 2025/CMPT 318")
df <- read.table("Group_Assignment_Dataset.txt", header = TRUE, sep = ",")

df$Date <- as.Date(df$Date, format = "%d/%m/%Y")

# Filter the df to 21st week only
start <- as.Date("2007-05-21")
end <- as.Date("2007-05-27")
filtered <- df[df$Date >= start & df$Date <= end,]

filtered$Time <- format(as.POSIXct(filtered$Time, format = "%H:%M:%S"), "%H:%M:%S")
startTime <- "07:30:00"
endTime <- "17:00:00"

# Add column for Day of Week
filtered$DayofWeek <- weekdays(filtered$Date)

# Add column for Weekday/Weekend classification
filtered$dayType <- ifelse(filtered$DayofWeek %in% c("Saturday", "Sunday"), "Weekend", "Weekdays")

# Create different df for day/night of weekday/weekend
day_weekday <- filtered %>%
  filter(dayType == "Weekdays" & Time >= startTime & Time <= endTime)

night_weekday <- filtered %>%
  filter(dayType == "Weekdays" & (Time < startTime | Time > endTime))

day_weekend <- filtered %>%
  filter(dayType == "Weekend" & Time >= startTime & Time <= endTime)

night_weekend <- filtered %>%
  filter(dayType == "Weekend" & (Time < startTime | Time > endTime))

# Create different df to calculate avg of day/night of weekday/weekend
avg_day_weekday <- day_weekday %>%
  group_by(Time) %>%
  summarize(Average_Global_intensity = mean(Global_intensity, na.rm = TRUE))

avg_night_weekday <- night_weekday %>%
  group_by(Time) %>%
  summarize(Average_Global_intensity = mean(Global_intensity, na.rm = TRUE))

avg_day_weekend <- day_weekend %>%
  group_by(Time) %>%
  summarize(Average_Global_intensity = mean(Global_intensity, na.rm = TRUE))

avg_night_weekend <- night_weekend %>%
  group_by(Time) %>%
  summarize(Average_Global_intensity = mean(Global_intensity, na.rm = TRUE))

# Convert Time to numeric (seconds since midnight)
convert_time_to_seconds <- function(time) {
  as.numeric(as.POSIXct(time, format="%H:%M:%S")) - as.numeric(as.POSIXct("00:00:00", format="%H:%M:%S"))
}

avg_day_weekday$Time_Seconds <- convert_time_to_seconds(avg_day_weekday$Time)
avg_night_weekday$Time_Seconds <- convert_time_to_seconds(avg_night_weekday$Time)
avg_day_weekend$Time_Seconds <- convert_time_to_seconds(avg_day_weekend$Time)
avg_night_weekend$Time_Seconds <- convert_time_to_seconds(avg_night_weekend$Time)
                     

avg_day_weekday$Category <- "Day - Weekdays"
avg_night_weekday$Category <- "Night - Weekdays"
avg_day_weekend$Category <- "Day - Weekends"
avg_night_weekend$Category <- "Night - Weekends"

# Merge into a single dataframe
combined_data <- bind_rows(avg_day_weekday, avg_night_weekday, avg_day_weekend, avg_night_weekend)

#Plot Linear Regression
p1 <- ggplot(combined_data, aes(x = Time_Seconds, y = Average_Global_intensity, color = Category)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +  # Linear regression lines
  labs(title = "Linear Regression of Global Intensity",
       x = "Time (Seconds since Midnight)",
       y = "Average Global Intensity")

#Plot Polynomial Regression
p2 <- ggplot(combined_data, aes(x = Time_Seconds, y = Average_Global_intensity, color = Category)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, linewidth = 1) +  # Polynomial regression (degree 2)
  labs(title = "Polynomial Regression (Degree 2) of Global Intensity",
       x = "Time (Seconds since Midnight)",
       y = "Average Global Intensity")
                     
library(gridExtra)
grid.arrange(p1, p2, ncol=1)                     
                     
