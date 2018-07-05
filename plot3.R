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
      ## date and time must be known
      filter_at(vars(Date, Time), all_vars(. != "?")) %>%
      # remove rows completely equal to "?"
      filter_at(vars(Global_active_power:Sub_metering_3), any_vars(. != "?"))
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
## convert Sub_metering readings to numeric class
plot_data$Sub_metering_1 <- as.numeric(plot_data$Sub_metering_1)
plot_data$Sub_metering_2 <- as.numeric(plot_data$Sub_metering_2)
plot_data$Sub_metering_3 <- as.numeric(plot_data$Sub_metering_3)
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
ylabel <- "Energy sub metering"
## file name of the plot
file_name <- "plot3.png"
## start the png file device
png(file_name)
## plot is put in the png file
with(plot_data, {
      ## the plot is prepared without an x axis label, without x axis tick 
      ## labels, without lines
      plot(index, Sub_metering_1, xlab = "", ylab = ylabel, type = "n", xaxt = "n")
      ## put the x axis tick labels
      axis(side = 1, at = x_label_pos_arr, labels = x_label_arr)
      ## put the lines
      lines(index, Sub_metering_1, col = "black")
      lines(index, Sub_metering_2, col = "red")
      lines(index, Sub_metering_3, col = "blue")
      pos <- "topright" # legend position
      l_type <- c(1, 1, 1) # legend lines
      the_colors <- c("black", "red", "blue") # legend colors
      ## legend names
      names <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
      ## put the legend
      legend(pos, lty = l_type, names, col = the_colors)
      }
     )
## close the png file device 
dev.off()
