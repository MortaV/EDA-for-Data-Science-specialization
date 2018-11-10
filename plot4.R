##----------------------------------------------------------------------------

## Author: Morta
## Date: 2018-11-09
## Version: 1
## Code for Coursera, Exploratory Data Analysis Course Project, Plot4

##----------------------------------------------------------------------------

#uploading needed libraries
require(tidyverse)
require(extrafont)

# I really liked ggplot, but I don't like its default font so I found out how to
# change it. For that you need to load fonts to R once (and again only if you 
# reinstall it or dowload some new fonts)
font_import() #takes some time...
loadfonts(device="win") 

# you can skip data upload part if you already have data on your working 
# directory
data_folder <- "data"

if (!file.exists(data_folder)){
    dir.create(data_folder)
}

download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
              "data/data.zip")

unzip("data/data.zip", exdir = data_folder)

#seting up the theme for ggplot

theme <- theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14), 
          text = element_text(family = "Lucida Sans Unicode", colour = "#003C5A"), 
          legend.position = "bottom",
          strip.background = element_rect(fill = "#D4E6F1"),
          strip.text = element_text(colour = "#003C5A"))

# reading the data

NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# joining SCC names
NEI <- NEI %>%
    left_join(SCC, by = "SCC")

# filtering only coal related sectors and aggregating the data
coal <- NEI %>%
    filter(grepl('[Cc]oal',EI.Sector)) %>%
    group_by(year, fips) %>%
    summarize(emi = sum(Emissions))

# making the plot and saving it to plot4.png, I chose to leave it with the background
# so that frame around is ok and it won't be seen when added to presentation - only 
# when previewing:) but it can be changed by adding bg = "transparent"
png("plot4.png", width = 480, height = 480, units = "px")

coal %>%
    ggplot(aes(x = emi)) + 
    geom_histogram(alpha = 0.5,
                   color = "#F7DC6F",
                   fill = "#F7DC6F") +
    labs(x = expression(log[10]~(Emission)),
         y = "Amount of US Counties",
         title = "Emissions from coal combustion-related sources") +
    theme +
    facet_wrap(~year, 
               ncol = 1) +
    # I am aware that I am loosing zero values:
    scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))

dev.off()
