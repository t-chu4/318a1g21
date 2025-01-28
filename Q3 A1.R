library(ggplot2)
setwd("E:/Spring 2025/CMPT 318")
df <- read.table("Group_Assignment_Dataset.txt", header = TRUE, sep = ",")

# Filter the df to 21st week only
df$Date <- strptime(df$Date, format = "%d/%m/%Y")
start <- strptime("28/5/2007", format = "%d/%m/%Y")
end <- strptime("3/6/2007", format = "%d/%m/%Y")
filtered <- df[df$Date >= start & df$Date <= end,]

# Day hours and night hours
filtered$Time <- strptime(filtered$Time, format = "%H:%M:%S")
startTime <- strptime("07:30:00", format = "%H:%M:%S")
endTime <- strptime("17:00:00", format = "%H:%M:%S")
dayHours <- filtered[filtered$Time >= startTime & filtered$Time <= endTime,]
nrow(dayHours)
nightHours <- filtered[filtered$Time < startTime | filtered$Time > endTime,]
nrow(nightHours)

#Add col Day of Week (Monday/Tuesday/...)
filtered$DayofWeek <- weekdays(filtered$Date)
