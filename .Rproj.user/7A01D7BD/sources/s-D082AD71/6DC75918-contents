##----------------------------------------------------------------------------

## Author: Morta
## Date: 2018-11-09
## Version: 1
## Code for Coursera, Exploratory Data Analysis Course Project, Plot6

##----------------------------------------------------------------------------

#uploading needed libraries
require(tidyverse)
require(extrafont)
require(scales)

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
          plot.subtitle = element_text(hjust = 0.5),
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

# filtering only vehicles data and only Baltimore and LA
vehicles2 <- NEI %>%
    filter(grepl('Vehicles', EI.Sector),
           fips %in% c("24510", "06037")) %>%
    group_by(year, SCC.Level.Two, fips) %>%
    summarize(emi = sum(Emissions, na.rm = TRUE)) %>%
    ungroup() %>%
    spread(SCC.Level.Two, emi) %>%
    mutate(Total = `Highway Vehicles - Diesel` + `Highway Vehicles - Gasoline`) %>%
    gather("SCC.Level.Two", "emi",`Highway Vehicles - Diesel`:Total) %>%
    group_by(SCC.Level.Two, fips) %>%
    # adding names and preparing YoY data
    mutate(fips_name = ifelse(fips == "24510", "Baltimore", "LA"),
           SCC_level = gsub('-', '\n', SCC.Level.Two),
           lag = lag(emi, order_by = year),
           diff_val = (emi - lag)/lag,
           col = factor(ifelse(diff_val > 0, "Increase", "Decrease"),
                        ordered = TRUE,
                        levels = c("Decrease", "Increase"))) %>%
    ungroup()

# preparation of annotation data set (1999 vs 2008)
for_annotation <- vehicles2 %>%
    select(-c(lag:col)) %>%
    spread(year, emi) %>%
    mutate(label = (`2008` - `1999`) / `1999`) %>%
    select(-(`1999`:`2008`)) %>%
    mutate(col = factor(ifelse(label > 0, "Increase", "Decrease"),
                 ordered = TRUE,
                 levels = c("Decrease", "Increase")))
    
# making the plot and saving it to plot6.png, I chose to leave it with the background
# so that frame around is ok and it won't be seen when added to presentation - only 
# when previewing:) but it can be changed by adding bg = "transparent"
png("plot6.png", width = 700, height = 400, units = "px")

vehicles2 %>%
    ggplot(aes(x = as.factor(year), 
               y = emi,
               group = 1)) + 
    geom_col(alpha = 0.5,
             fill = "#F7DC6F") +
    geom_line(color = "#003C5A") +
    geom_point(color = "#003C5A") +
    labs(x = "Year",
         y = "Tones",
         title = "Total PM2.5 emissions from motor vehicle sources") +
    theme +
    facet_grid(fips_name ~ SCC_level) +
    coord_cartesian(ylim = c(0, 7100)) +
    geom_text(
        aes(label = ifelse(is.na(lag), 
                           NA, 
                           paste0(percent_format()(diff_val), " YoY")),
            color = col),
        size = 3.5,
        vjust = -1,
        hjust = 0.5,
        family = "Lucida Sans Unicode") +
    geom_text(data = for_annotation,
              aes(x = 1, y = 6000), 
              label = "2008 vs 1999:",
              hjust   = -0.2,
              vjust   = 0,
              family = "Lucida Sans Unicode",
              size = 3.75) +
    geom_text(data = for_annotation,
              aes(x = 3, y = 6000, 
                  label = percent_format()(label),
                  color = col),
              hjust   = 0,
              vjust   = 0,
              family = "Lucida Sans Unicode",
              size = 4.5,
              fontface = "bold") +
    # colors are reversed on purpose as negative numbers are good
    scale_color_manual(values = c("#229954", "#C0392B")) +
    guides(color = FALSE) +
    scale_y_continuous(labels = comma_format())


dev.off()
