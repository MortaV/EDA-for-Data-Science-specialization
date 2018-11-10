##----------------------------------------------------------------------------

## Author: Morta
## Date: 2018-11-09
## Version: 1
## Code for Coursera, Exploratory Data Analysis Course Project, Plot1

##----------------------------------------------------------------------------

#uploading needed libraries
require(tidyverse)

# you can skip data upload part if you already have data on your working 
# directory
data_folder <- "data"

if (!file.exists(data_folder)){
    dir.create(data_folder)
}

download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
              "data/data.zip")

unzip("data/data.zip", exdir = data_folder)

# reading the data

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

sum_per_year <- NEI %>%
    group_by(year) %>%
    summarize(total = sum(Emissions, na.rm = TRUE)/1000000)

# making the plot and saving it to plot1.png, I chose to leave it with the background
# so that frame around is ok and it won't be seen when added to presentation - only 
# when previewing:) but it can be changed by adding bg = "transparent"
png("plot1.png")

with(sum_per_year, 
     {plot(year, 
          total, 
          type = "l",
          col = "#F7DC6F",
          xlab = "Year",
          ylab = "Mln. tones",
          main = "Total PM2.5 emission",
          # to have space for labels:
          ylim = c(min(sum_per_year$total), max(sum_per_year$total) + 1))
     points(year, 
            total,
            col = "#F7DC6F",
            pch = 19)
     text(year, 
          total,
          round(total, 2),
          pos = 3)}
     )

dev.off()
