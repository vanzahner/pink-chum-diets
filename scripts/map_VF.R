##############################################################################################
# Load the various R packages that are needed for the tables and plots. 
# If not already installed, they can be installed using the install.packages() command. 
library(knitr)
library(pander)
library(ggplot2)
library(scales)
library(reshape2)
library(MASS)
library(tidyverse)
library(maps)
library(mapdata)
library(mapproj)
library(PBSmapping)
library(grid)
library(scales)
library(gridExtra)
library(cowplot)
library(ggmap)
library(ggsn)
library(ggforce)
library(ggrepel)
library(here)

##############################################################################################
# Read in a csv file that contains the latitude and longitude for each of the stations/sets
setwd("/Users/Vanessa/Desktop/msc_project")
temp_coords <- read_csv("Map code/temp_sites.csv")
spat_coords <- read_csv("Map code/spat_sites.csv")

##############################################################################################
# Load North Pacific map data using the PBSmapping package from the Pacific Biological Station
# in Nanaimo, Canada
data('nepacLLhigh') 

# View first few rows of data
nepacLLhigh[1:5,]

# rename the columns of nepacLLhigh
colnames(nepacLLhigh) <- c("group", "POS", "long", "lat")

##############################################################################################
# Now that we've loaded the maps and have the data read, let's now create a base map of entire region
GOAmap1 <- ggplot()+
        theme_bw() +
        geom_polygon(data=nepacLLhigh, aes(long,lat,group=group), fill='grey90', size = 0.1, color="black")
GOAmap1 

##############################################################################################
# Now zoom in a bit to create map of survey area, onto which we will plot the site locations
#General Study Area for Introduction
study_area1 <- GOAmap1 + coord_map(projection="gilbert") +
        geom_ellipse(aes(x0 = -126.1, y0 = 50.45, a = 0.1, b = 0.6, angle = pi/2.25), size = 0.8, color = "grey30") +
        geom_ellipse(aes(x0 = -125.05, y0 = 50.25, a = 0.4, b = 0.25, angle = 0), size = 0.8, color = "grey30") +
        geom_point(aes(x = -123.2, y = 49.15), color = "dodgerblue4", size = 8, alpha = 0.6) +
        geom_segment(aes(x = -123.3, xend = -123.55, y = 49.15, yend = 49.3), size = 1, color = "dodgerblue3", arrow = arrow(type = "open", length = unit(0.2, "cm"))) +
        geom_segment(aes(x = -124.7, xend = -124.95, y = 49.8, yend = 49.95), size = 1, color = "dodgerblue3", arrow = arrow(type = "open", length = unit(0.2, "cm"))) +
        geom_segment(aes(x = -127.6, xend = -127.8, y = 50.95, yend = 51.1), size = 1, color = "dodgerblue3", arrow = arrow(type = "open", length = unit(0.2, "cm"))) +
        theme_bw() +
        theme(
                axis.text=element_text(size=10, color="black"),
                axis.title=element_blank(),
                panel.grid = element_blank(),
                plot.background = element_rect(fill = "white")
        ) +
        coord_cartesian(xlim = c(-128.5, -122.75), ylim=c(49, 51.5)) +
        north(x.min = -123.4, x.max = -123.1, 
              y.min = 51.3, y.max = 51.55, scale = 1.5, symbol = 3) + 
        scalebar(x.min = -125.5, x.max = -123.9, 
                 y.min = 51.4, y.max = 51.5, 
                 dist = 50, dist_unit="km", dd2km = TRUE, transform = FALSE,
                 model = "WGS84", height = 0.5, 
                 st.dist = 0.8,
                 box.fill = c("grey30", "white"),
                 st.color = "grey30",
                 border.size = 0.5,
                 st.size = 3.5) +
        annotate("label", x = -123.9, y = 49.35, label = "Strait of\nGeorgia", color = "dodgerblue4", size = 3.5, fontface = "italic") +
        annotate("label", x = -127.2, y = 50.78, label = "Queen\nCharlotte\nStrait", color = "dodgerblue4", size = 3.5, fontface = "italic") +
        annotate("label", x = -128.35, y = 51.25, label = "Queen\nCharlotte\nSound", color = "dodgerblue4", size = 3.5, fontface = "italic") +
        annotate("text", x = -125.4, y = 49.5, label = "Vancouver\nIsland", color = "grey30", size = 5) +
        annotate("text", x = -123.3, y = 50.9, label = "Mainland\nBritish Columbia", color = "grey30", size = 5) +
        annotate("label", x = -124.3, y = 50.4, label = "Discovery Islands", color = "grey30", size = 4) +
        annotate("label", x = -126.7, y = 50.35, label = "Johnstone Strait", color = "grey30", size = 4) +
        annotate("label", x = -122.8, y = 49.3, label = "Fraser\nRiver\nOutflow", color = "dodgerblue4", size = 3.5)
        
study_area1
ggsave("study_area_bigscale.png", path = "./Map code/figs/map",
       width = 16, height = 12, units = "cm", dpi = 300)

#inset map of the BC coast
study_area2 <- GOAmap1 + coord_map(projection="gilbert") +
        theme_bw() +
        theme(
                axis.text=element_blank(),
                axis.title=element_blank(),
                panel.grid = element_blank(),
                plot.background = element_rect(fill = "white")
        ) +
        coord_cartesian(xlim = c(-135, -122), ylim=c(47, 58)) +
        annotate(geom = "rect", ymax = 51.5, ymin = 49, xmax = -123, xmin = -128.5, colour = "red", fill = NA, size = 1) +
        theme_inset() +
        annotate("text", x = -132, y = 49, label = "North Pacific\nOcean", color = "dodgerblue4", size = 3.5, fontface = "italic") +
        annotate("text", x = -126, y = 56, label = "British\nColumbia", color = "grey30", size = 4)
study_area2
ggsave("study_area2.png", path = "./Map code/figs/map",
       width = 4, height = 5, units = "cm", dpi = 300)

# Run this whole piece of code to generate map with inset and save in your file folder
png(filename = "./figs/map/big_area_map.png", width = 7, height = 5, units = "in", res = 300)#, type = "cairo") #'cairo' smooths the pixelated edges of curved lines in your figures

# create a viewport for inset
# vp_inset width/height arguments set the size of the inset; x and y arguments set the position (from 0 to 1) of the left, top corner of the inset along each axis (i.e. not map coordinates as you have in your annotation custom). You can adjust these as you see fit.
vp_inset <- grid::viewport(width = 0.25, height = 0.45, x = 0.075, y = 0.07, just = c("left", "bottom"))
print(study_area1)
print(study_area2, vp = vp_inset)
dev.off()



# TEMPORAL map zoomed in on DI and JS highlighting D07 and J07
temp_area1 <- GOAmap1 + coord_map(projection="gilbert") +
        geom_point(data = temp_coords, aes(x = long, y = lat), size = 4) +
        geom_ellipse(aes(x0 = -126.28, y0 = 50.47, a = 0.1, b = 0.66, angle = pi/2.27), size = 0.8, color = "grey30") +
        geom_ellipse(aes(x0 = -125.1, y0 = 50.23, a = 0.45, b = 0.275, angle = 0), size = 0.8, color = "grey30") +
        theme_bw() +
        theme(axis.text=element_text(size=10, color="black"),
              axis.title=element_text(size=10),
              panel.grid = element_blank(),
              plot.background = element_rect(fill = "white")) +
        coord_cartesian(xlim = c(-127, -124.5), ylim=c(49.8, 51)) +
#        north(x.min = -124.7, x.max = -124.5, 
#                    y.min = 50.85, y.max = 51, scale = 1.5, symbol = 3) + 
        scalebar(x.min = -126.3, x.max = -125.4, 
                y.min = 49.8, y.max = 49.85, 
                dist = 25, dist_unit="km", dd2km = TRUE, transform = FALSE,
                model = "WGS84", height = 0.5, 
                st.dist = 0.5,
                box.fill = c("grey30", "white"),
                st.color = "grey30",
                border.size = 0.5,
                st.size = 3.5) +
        annotate("label", x = -124.8, y = 50.52, label = "Discovery Islands", color = "grey30", size = 5) +
        annotate("label", x = -126.7, y = 50.42, label = "Johnstone Strait", color = "grey30", size = 5) +
        annotate("text", x = -125.25, y = 50.2, label = "D07",  size = 4) +
        annotate("text", x = -126.18, y = 50.41, label = "J07",  size = 4) +
        annotate("text", x = -124.85, y = 49.85, label = "Strait of\nGeorgia", color = "dodgerblue4", size = 3.5, fontface = "italic") +
        annotate("text", x = -126.98, y = 50.74, label = "Queen\nCharlotte\nStrait", color = "dodgerblue4", size = 3.5, fontface = "italic") +
        annotate("text", x = -125.75, y = 49.9, label = "Vancouver Island", color = "grey30", size = 5) 
#+
#        annotate("text", x = -125.25, y = 50.9, label = "Mainland\nBritish\nColumbia", color = "grey30", size = 5)
temp_area1
ggsave("temp_area1.png", path = "./Map code/figs/map", width = 16, height = 12, units = "cm", dpi = 300)

#inset map of BC coast
temp_area2 <- GOAmap1 + coord_map(projection="gilbert") +
        theme_bw() +
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              panel.grid = element_blank(),
              plot.background = element_rect(fill = "white")) +
        coord_cartesian(xlim = c(-135, -122), ylim=c(47, 58)) +
        annotate(geom = "rect", ymax = 50.8, ymin = 49.9, xmax = -124.5, xmin = -126.5, colour = "red", fill = NA, size = 1) +
        annotate("text", x = -131.5, y = 48.9, label = "North Pacific\nOcean", color = "dodgerblue4", size = 3, fontface = "italic") +
        annotate("text", x = -125.9, y = 56, label = "British\nColumbia", color = "grey30", size = 4) +
        theme_inset()
temp_area2
ggsave("temp_area2.png", path = "./Map code/figs/map", width = 4, height = 5, units = "cm", dpi = 300)


png(filename = "./Map code/figs/map/temp_map.png", width = 7, height = 5, units = "in", res = 300)#, type = "cairo")

# create a viewport for inset
# vp_inset width/height arguments set the size of the inset; x and y arguments set the position (from 0 to 1) of the left, top corner of the inset along each axis (i.e. not map coordinates as you have in your annotation custom). You can adjust these as you see fit.
vp_inset <- grid::viewport(width = 0.22, height = 0.4, x = 0.1, y = 0.1, just = c("left", "bottom"))
print(temp_area1)
print(temp_area2, vp = vp_inset)
dev.off()

##
# SPATIAL map
spat_area1 <- GOAmap1 + coord_map(projection="gilbert") +
        geom_point(data = spat_coords, aes(x = long, y = lat), size = 3,
                   color=c("black"#"#B2182B", "#E41A1C", "#F781BF", "#053061", "#A6CEE3", "#1F78B4"
                           ))+
  # D07=darkred, D09=red, D11=pink, J06=lightblue, J08=blue, J02=darkblue
        geom_text(data = spat_coords, aes(x = long, y = lat, label = site), hjust=-0.5, vjust=-1, fontface = "bold") +
        #geom_label_repel
        #geom_ellipse(aes(x0 = -126.28, y0 = 50.47, a = 0.1, b = 0.66, angle = pi/2.27), size = 0.8, color = "grey30") +
        #geom_ellipse(aes(x0 = -125.1, y0 = 50.23, a = 0.45, b = 0.275, angle = 0), size = 0.8, color = "grey30") +
        theme_bw() +
        theme(
                axis.text=element_text(size=10, color="black"),
                axis.title=element_text(size=10),
                panel.grid = element_blank(),
                plot.background = element_rect(fill = "white")
        ) +
        coord_cartesian(xlim = c(-127, -124.5), ylim=c(49.8, 51)) +
        #north(x.min = -124.7, x.max = -124.5, 
        #      y.min = 50.85, y.max = 51, scale = 1.5, symbol = 3) + 
        #scalebar(x.min = -126.3, x.max = -125.4, 
        #         y.min = 49.8, y.max = 49.85, 
        #         dist = 25, dist_unit="km", transform = TRUE,
        #         model = "WGS84", height = 0.5, 
        #         st.dist = 0.5,
        #         box.fill = c("grey30", "white"),
        #         st.color = "grey30",
        #         border.size = 0.5,
        #         st.size = 3.5)+
        annotate("label", x = -124.8, y = 50.52, label = "Discovery Islands", color = "grey30", size = 5) +
        annotate("label", x = -126.7, y = 50.42, label = "Johnstone Strait", color = "grey30", size = 5) +
        annotate("text", x = -124.85, y = 49.85, label = "Strait of\nGeorgia", color = "dodgerblue4", size = 3.5, fontface = "italic") +
        annotate("text", x = -126.98, y = 50.74, label = "Queen\nCharlotte\nStrait", color = "dodgerblue4", size = 3.5, fontface = "italic") +
        annotate("text", x = -125.75, y = 49.9, label = "Vancouver Island", color = "grey30", size = 5) 
#+
        #annotate("text", x = -125.25, y = 50.9, label = "Mainland\nBritish\nColumbia", color = "grey30", size = 5)
spat_area1
ggsave("spat_area_basic.png", path = "./Map code/figs/map", width = 16, height = 12, units = "cm", dpi = 300)

png(filename = "./Map code/figs/map/spat_map.png", width = 7, height = 5, units = "in", res = 300)#, type = "cairo")

# create a viewport for inset
# vp_inset width/height arguments set the size of the inset; x and y arguments set the position (from 0 to 1) of the left, top corner of the inset along each axis (i.e. not map coordinates as you have in your annotation custom). You can adjust these as you see fit.
vp_inset <- grid::viewport(width = 0.22, height = 0.4, x = 0.1, y = 0.1, just = c("left", "bottom"))
print(spat_area1)
print(temp_area2, vp = vp_inset)
dev.off()

