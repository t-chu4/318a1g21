# 318 A1 Q2;
# Timothy Chua;

library(dplyr)
library(corrplot)
library(ggplot2) # Missed the tutorial for this, whoops

# Execution Instructions: Simply run "./Question2.r" in bash.

# Basic table read
data <- read.csv("Group_Assignment_Dataset.txt",
                 sep = ",",
                 header = T,)

# Columns, minus date/time
data_vars <- c("Global_active_power",
                   "Global_reactive_power", 
                   "Voltage",
                   "Global_intensity", 
                   "Sub_metering_1",
                   "Sub_metering_2",
                   "Sub_metering_3")

# Precautionary, remove empty things
data <- na.omit(data)

# Matrix calculated with command taken directly from assignment info
matrix <- cor(data[data_vars],
                  method = "pearson")

# Assume math is correct as calculation shown in class. Display appropriate...?
print(matrix)

