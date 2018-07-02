## load the data.table package
library(data.table)
## load the dplyr package
library(dplyr)
## load the lubridate package
library(lubridate)
## name of the unzipped data file
data_file <- "household_power_consumption.txt"
## read the household power consumption data as a data frame
power_df <- fread(data_file, colClasses = "character", data.table = FALSE)
power_df <- power_df %>%
      distinct(.keep_all = TRUE) %>% # remove duplicate rows
      filter_all(all_vars(. != "?")) # remove rows with at least one "?"
## put the dates into YYYY-MM-DD form using the lubridate package
power_df$Date <- dmy(power_df$Date)
## set the required dates
date1 <- dmy("1/2/2007")
date2 <- dmy("2/2/2007")
## get the data from the required dates
plot_data <- filter(power_df, (Date == date1) | (Date == date2))
## convert the time column to lubridate Period form
plot_data$Time <- hms(plot_data$Time)
## be sure that the data is ordered in time
plot_data <- plot_data[order(plot_data$Date, plot_data$Time),]
## convert global active power to numeric class
plot_data$Global_active_power <- as.numeric(plot_data$Global_active_power)
## set the istogram color to red
colour <- "red"
## set the x axis label
xlabel <- "Global Active Power (kilowatts)"
## set y axis label
ylabel <- "Frequency"
## set the title of the plot
the_title <- "Global Active Power"
## set the output file name
file_name <- "plot1.png"
## start the png file device
png(file_name)
## plot the histogram to the png file
with(plot_data, hist(Global_active_power, col = colour, xlab = xlabel, 
                     ylab = ylabel, main = the_title))
## close the png file device
dev.off()
