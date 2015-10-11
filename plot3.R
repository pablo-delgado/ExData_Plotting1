
# Note: 
# This method could be refactored to remove some of the temp variables created
# in the process, but I decided to leave them so that the scripts are easier to 
# read by others during peer assessment.

library(dplyr, quietly = TRUE)
library(lubridate, quietly = TRUE)

plot3 <- function() {
    ############################################################################
    # Load and Clean Data Routines 
    ############################################################################
    # These could be moved into a seprate R file and sourced.
    # I placed this function here since the assignment required the file to 
    # include the code to load the data files as well as rendering the plot. I
    # also did not place it outside the scope of this function to avoid loading
    # a utility method in the environment of any other script that may be using
    # or sourcing this file. This would avoid accidentally overriding some other
    # definition of load_data.
    ############################################################################
    data_file <- "household_power_consumption.txt"
    if(file.exists(data_file) == FALSE) {
        # Prefix the the filename and change the extension to zip, we will 
        # use this filename to download the file from cloundfont.
        zip_file <- paste("exdata%2Fdata%2F", sub(".txt", ".zip", data_file), sep = "")
        
        # First check if the zip file contaning the data is present int the directory
        if(file.exists(zip_file) == FALSE) {
            #"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
            download_url <- paste("https://d396qusza40orc.cloudfront.net/", zip_file, sep = "")
            
            # Determine the OS, if the script is being run on MacOSX then switch the
            # method from auto to "curl" or the download.file call might fail.
            download_method <- ( if(Sys.info()['sysname'] == "Darwin") "curl" else "auto" )
            download.file(download_url, zip_file, method = download_method, mode = "w")
        }
        
        # Now that we are sure we have the file unzip it
        unzip(zipfile = paste("", zip_file, sep = "./"))
    }
    
    # At this point we should have the data_file
    #data <- fread(input = data_file, sep = ";", header = TRUE, na.strings = c("?"), colClasses = "character")
    data <- read.csv(data_file, sep = ";")
    
    # Use dply for simplicity
    #   * Remove all NA from the dataset
    #   * Convert Date to date
    #   * Convert Global_active_power to numeric
    #   * Filter dataset to only include the data between 2007-02-01 and 2007-02-02
    data <- tbl_df(data)
    data <- na.omit(data) %>%
        mutate(DateTime = dmy_hms(paste(Date, Time)), 
               Date = dmy(Date), 
               DateOfWeek = wday(Date, label = TRUE), 
               Global_active_power = as.numeric(Global_active_power)/1000 ) %>%
        filter(Date >= ymd("2007-02-01"), Date <= ymd("2007-02-02"))
    
    # Open png decide
    png(filename="plot3.png", width = 480, height = 480, units = "px")
    
    # Create the plot
    plot(data$DateTime, data$Sub_metering_1, type="l", 
         xlab = "", 
         ylab = "Energy sub metering")
    
    lines(data$DateTime, data$Sub_metering_2, col = "red")
    
    lines(data$DateTime, data$Sub_metering_3, col = "blue")
    
    # Add a legend
    legend("topright", col=c("black", "red", "blue"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1)
    
    # Close the device
    dev.off()
    
    # Return clean dataset 
    return(data)
    
}