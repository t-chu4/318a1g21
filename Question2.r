# 318 A1 Q2;
# Timothy Chua;

library(dplyr)
library(corrplot)
library(ggplot2) # Missed the tutorial for this, whoops
library(zoo)

# Execution Instructions: Simply run "Rscript Question2.r" in bash.

# Basic table read
data <- read.table("Group_Assignment_Dataset.txt",
                 sep = ",",
                 header = T,)

# Handle missing data using Linear Interpolation
data <- data %>% mutate(across(where(is.numeric), ~na.approx(.x, na.rm=FALSE)))

# Use as.POSIXlt to extract week information
data$Date <- as.POSIXlt(data$Date, format = "%d/%m/%Y")

# Filter data for the 21st week
group_number <- 21
start_date <- as.Date("2007-1-1") + (group_number - 1) * 7
end_date <- start_date + 7
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

