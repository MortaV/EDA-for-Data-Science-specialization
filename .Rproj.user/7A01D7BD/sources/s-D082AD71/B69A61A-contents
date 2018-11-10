##----------------------------------------------------------------------------

## Author: Morta
## Date: 2018-11-09
## Version: 1
## Code for Coursera, Exploratory Data Analysis Course Project, Plot2

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

# creating aggregated tibble for the graph
sum_per_year2 <- NEI %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarize(total = sum(Emissions, na.rm = TRUE))

# variable for colors
bigger <- sum_per_year2[sum_per_year2$year == 1999, "total"] < sum_per_year2[sum_per_year2$year == 2008, "total"]

# annotation to the graph
text_value <- paste0("Total PM2.5 emission in Baltimore City\n has ",
                     ifelse(bigger, "increased ", "decreased "),
                     "by ",
                     round(abs(sum_per_year2[sum_per_year2$year == 2008, "total"] - sum_per_year2[sum_per_year2$year == 1999, "total"]), 2),
                     " tones")

# making the plot and saving it to plot2.png, I chose to leave it with the background
# so that frame around is ok and it won't be seen when added to presentation - only 
# when previewing:) but it can be changed by adding bg = "transparent"
png("plot2.png")

with(sum_per_year2, 
     {plot(year, 
           total, 
           type = "l",
           col = "#F4D03F",
           xlab = "Year",
           ylab = "Tones",
           main = "Total PM2.5 emission in Baltimore City",
           #making space for annotation:
           ylim = c(min(sum_per_year2$total), max(sum_per_year2$total) + 600),
           xlim = c(min(sum_per_year2$year)-1, max(sum_per_year2$year) + 1))
         points(year, 
                total,
                col = "#F7DC6F",
                pch = 19)
         text(year, 
              total,
              round(total, 2),
              pos = 3)
         text(2006,
              3700,
              text_value)}
)

dev.off()
