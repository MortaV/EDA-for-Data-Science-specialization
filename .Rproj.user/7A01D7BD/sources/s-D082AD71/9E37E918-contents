##----------------------------------------------------------------------------

## Author: Morta
## Date: 2018-11-09
## Version: 1
## Code for Coursera, Exploratory Data Analysis Course Project, Plot3

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

# creating aggregated tibble
sum_per_year_and_type <- NEI %>%
    filter(fips == "24510") %>%
    group_by(year, type) %>%
    summarize(total = sum(Emissions, na.rm = TRUE)) %>%
    ungroup()

# help tibble for colors
years_spread <- sum_per_year_and_type %>%
    spread(year, total) %>%
    mutate(diff = `2008` > `1999`) %>%
    select(type, diff)

# final tibble for the graph
for_graph <- sum_per_year_and_type %>%
    left_join(years_spread,
              by = "type") %>%
    mutate(diff_name = factor(ifelse(diff, "Increased", "Decreased or no change"),
                                 ordered = TRUE,
                                 levels = c("Increased", "Decreased or no change")))

# making the plot and saving it to plot3.png, I chose to leave it with the background
# so that frame around is ok and it won't be seen when added to presentation - only 
# when previewing:) but it can be changed by adding bg = "transparent"
png("plot3.png", width = 600, height = 480, units = "px")

for_graph %>%
    ggplot(aes(x = year, 
               y = total, 
               color = diff_name,
               fill = diff_name)) +
    geom_area(alpha = 0.2) +
    geom_point() +
    geom_line() +
    facet_wrap(~type) +
    labs(x = "Year",
         y = "Tones",
         title = "Total PM2.5 emission in Baltimore City by Type",
         color = "2008 vs. 1999") +
    theme +
    scale_color_manual(values = c("#C0392B", "#229954")) +
    scale_fill_manual(values = c("#C0392B", "#229954")) +
    scale_y_continuous(labels = comma_format()) +
    guides(fill = FALSE)

dev.off()
