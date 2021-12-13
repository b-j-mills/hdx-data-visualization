rm(list=ls())

library("sf")
library("sp")
library("ggplot2")
library("gifski")
library("gganimate")
library("gridExtra")
library("RColorBrewer")
library("stringr")

setwd("")

# input iso or isos
iso <- c("")

# graph titles
plot_title <- ""
subtitle <- ""
label_position <- c("b", "r") # top or bottom, left or right

# input data
main_data <- st_read("")
admin1_bound <- st_read("")
country_bound <- st_read("data/geoBoundariesCGAZ_ADM0/geoBoundariesCGAZ_ADM0.shp")

# set data visualization class and manipulate
viz_attribute <- "place"
viz_type <- "seq"
viz_type <- "qual"
if (viz_attribute != ""){
  main_data$viz <- st_drop_geometry(main_data)[, viz_attribute]
  if (viz_type == "qual"){
    levels(main_data$viz) <- str_to_title(gsub("[^[:alnum:] ]", " ", levels(main_data$viz)))
  }
}

# choose singular color if not symbolizing by category (the ones below have been used before)
viz_color <- "cyan2" # used for health facilities
viz_color <- "gold1" # used for populated places
viz_color <- "orange" # used for many things
viz_color <- "YlOrRd"
viz_color <- "YlGnBu"
viz_color <- "Set1"
viz_color <- "Accent"

# data class
type <- st_geometry_type(main_data)
type <- names(which.max(summary(type)))

# subset and simplify admin boundaries
country_bound <- country_bound[country_bound$ISO_CODE %in% iso, ]
country_bound <- st_simplify(country_bound, dTolerance = 500)
admin1_bound <- st_simplify(admin1_bound, dTolerance = 500)

# calculate bounding box and aspect ratio
plot_size <- 4
country_bb <- st_bbox(country_bound)
x_range <- (country_bb$xmax - country_bb$xmin)
y_range <- (country_bb$ymax - country_bb$ymin)
country_ratio <- y_range / x_range * (1/cos((y_range/2 + country_bb$ymin)*pi/180))
if (country_ratio >= 0.9375){
  plot_height <- plot_size
  plot_width <- (plot_size - 0.25) / country_ratio
} else {
  plot_height <- plot_size * country_ratio + 0.25
  plot_width <- plot_size
}
plot_margin_x <- (plot_size - plot_width) / 2
plot_margin_y <- (plot_size - plot_height) / 2

# calculate title positions
if ("b" %in% label_position){
  y_meas <- ifelse(plot_margin_y == 0, y_range * 0.05, 
                   (plot_margin_y * 2 / plot_size - 0.1) * y_range)
  title_ypos <- country_bb$ymin - y_meas
} else {
  y_meas <- ifelse(plot_margin_y == 0, y_range * 0.1, 
                   (plot_margin_y * 2 / plot_size - 0.05) * y_range)
  title_ypos <- country_bb$ymax + y_meas
}
x_meas <- ifelse(plot_margin_x == 0, 0, (plot_margin_x * 2 / plot_size + 0.09) * x_range)
if ("r" %in% label_position){
  title_xpos <- country_bb$xmax + x_meas
} else {
  title_xpos <- country_bb$xmin - x_meas
}
subtitle_ypos <- title_ypos - 0.05*y_range
text_just <- ifelse("r" %in% label_position, 1, 0)
ann_just <- ifelse("r" %in% label_position, 0.6, 0.2)

p <- ggplot() + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "black"),
        legend.position = "none")

if (type %in% c("LINESTRING", "MULTILINESTRING")) {
  data_plot <- p + 
    geom_sf(data = country_bound, color = "gray30", fill=NA, size = 0.3) + 
    geom_sf(data = main_data, color = viz_color, size = 0.4, alpha = 0.1) + 
    geom_sf(data = main_data, color = viz_color, size = 0.15, alpha = 0.3) + 
    geom_sf(data = main_data, color = viz_color, size = 0.05)

} else if (type %in% c("POINT", "MULTIPOINT")) {
  if (viz_attribute != ""){
    p_size <- ifelse(nrow(main_data) > 10000, 0.3, 1.5)
    data_plot <- p +
      geom_sf(data = admin1_bound, color = "gray30", fill = NA, size = 0.3) +
      geom_sf(data = main_data, aes(color = viz), shape = 16, size = p_size, alpha = 0.8) + 
      scale_color_brewer(type = viz_type, palette = viz_color)
  } else {
    if (nrow(main_data) > 10000){ # too many points to have glow effect
      data_plot <- p +
        geom_sf(data = admin1_bound, color = "gray30", fill = NA, size = 0.3) +
        geom_sf(data = main_data, color = viz_color, shape = 16, size = 0.2, alpha = 0.5)
    } else { # sparse enough for glow effect
      data_plot <- p +
        geom_sf(data = admin1_bound, color = "gray30", fill = NA, size = 0.3) +
        geom_sf(data = main_data, color = viz_color, shape = 16, size = 0.25, alpha = 1) + 
        geom_sf(data = main_data, color = viz_color, shape = 16, size = 1.5, alpha = 0.25) +
        geom_sf(data = main_data, color = viz_color, shape = 16, size = 1, alpha = 0.4) +
        geom_sf(data = main_data, color = viz_color, shape = 16, size = 0.5, alpha = 0.5)
    }
  }
    
} else if (type %in% c("POLYGON","MULTIPOLYGON")) {
  data_plot <- p + 
    geom_sf(data = country_bound, fill = "gray10", color = NA) +
    geom_sf(data = main_data, aes(color = viz, size = viz), fill = NA, alpha = 0.8) + 
    scale_color_brewer(type = viz_type, palette = viz_color) + 
    scale_size_manual(values = c(0.1, 0.2, 0.3)) #+ 
    #theme(legend.text = element_text(colour="gray90", size = 10), legend.position = "bottom")
}

# Add title and subtitle and save
data_plot <- data_plot + 
  annotate("text", label = plot_title, x = title_xpos, y = title_ypos, 
           col = "gray90", size = 3, hjust = text_just) + 
  annotate("text", label = subtitle, x = title_xpos, y = subtitle_ypos, 
           col = "gray90", size = 2, hjust = text_just) + 
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black", color = NA),
        plot.margin = margin(ifelse("b" %in% label_position, plot_margin_y, 0),
                             ifelse("r" %in% label_position, 0, plot_margin_x),
                             ifelse("b" %in% label_position, 0, plot_margin_y), 
                             ifelse("r" %in% label_position, plot_margin_x, 0), "in"))

ggsave(paste0("maps/",plot_title,".png"), plot = data_plot, 
       dpi = 500, units = "in", width = plot_size, height = plot_size)

# Create animation
if (viz_attribute != ""){
  data_animation <- data_plot + 
    theme(plot.title = element_text(colour="gray90", size = 6, vjust = 1, hjust = 0.3)) +
    transition_manual(viz, cumulative = FALSE) + 
    labs(title = "{current_frame}")
  anim_save(paste0("maps/",plot_title,".gif"), data_animation, renderer = gifski_renderer(),
            width = plot_size, height = plot_size + 0.13, units = "in", res = 200, duration = 5)
}
