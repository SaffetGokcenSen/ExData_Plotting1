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
## add an index column to the plot data to make the x axis labelling
plot_data$index <- 1:nrow(plot_data)
## conditions to get the index of the x axis label "Fri"
cond1 <- plot_data[, "Date"] == ymd("2007-02-02")
cond2 <- plot_data[, "Time"] == hms("00:00:00")
## the filter to get the index of the x axis label "Fri"
the_filter <- (cond1) & (cond2)
## get the index of the x axis label "Fri"
x_label_pos2 <- plot_data[the_filter, ][, "index"]
## the index of the x axis label "Thu"
x_label_pos1 <- 1
## the index of the x axis label "Sat"
x_label_pos3 <- nrow(plot_data)
## label positions are put  in an array
x_label_pos_arr <- c(x_label_pos1, x_label_pos2, x_label_pos3)
## the array of x axis labels
x_label_arr <- c("Thu", "Fri", "Sat")
## label for the y axis
ylabel <- "Global Active Power (kilowatts)"
## file name of the plot
file_name <- "plot2.png"
## start the png file device
png(file_name)
## plot is put in the png file
with(plot_data, {
      ## the plot is prepared without an x axis label, without x axis tick 
      ## labels, without lines
      plot(index, Global_active_power, xlab = "", ylab = ylabel, type = "n", xaxt = "n")
      ## put the x axis tick labels
      axis(side = 1, at = x_label_pos_arr, labels = x_label_arr)
      ## put the lines
      lines(index, Global_active_power)
      }
     )
## close the png file device 
dev.off()
