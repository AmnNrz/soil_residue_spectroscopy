library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                        'Ph.D/Projects/Soil_Residue_Spectroscopy/Plots/00/')

# ## Plot Reflect vs Wvl across RWCs
# Residue
residue = read.csv(paste0(path_to_data, "Residue.csv"))
residue <- residue %>%
  mutate(RWC = round(RWC, 2))

# Filter data for "Wheat Norwest Duet"
for (type in unique(residue$Type)){
  filtered_data <- residue %>%
    filter(Type == type)
  
  base_size <- 14
  
  # Plot
  plot <- ggplot(filtered_data, aes(x = Wvl, y = Reflect, color = as.factor(RWC))) +
    geom_line() + # Use geom_point() if you want points instead of lines
    labs(title = type,
         x = "Wavelength, nm",
         y = "Reflectance factor",
         color = "RWC") +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
          legend.text = element_text(size = base_size), # Legend text at base size
          plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
          axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
          axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
          panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
          plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
          panel.grid = element_blank(),
          axis.ticks = element_line(color = "black"),
          axis.line = element_line(color = "black"),
          legend.key.size = unit(0.5, "cm"))
  
  ggsave(paste0(path_to_plots, 'ref_wvl_rwc/residue/', type, '.png'), plot, width = 10, height = 7, dpi = 300)
}



# Plot dry reflectances for all crops
# Filter for RWC = 0

filtered_data <- residue %>%
  group_by(Type) %>% 
  filter(RWC == min(RWC))

filtered_data <- filtered_data %>%
  filter(Type != "Wheat Pritchett")

print(unique(filtered_data$Type))


custom_colors <- c("Canola" = "#E41A1C",   # red
                   "Garbanzo Beans" = "#99e1d9",   # blue
                   "Peas" = "#4DAF4A",   # green
                   "Weathered Canola" = "#f9c823",   # purple
                    "Weathered Wheat" = "#2d00f7",   # purple
                    "Wheat Norwest Duet" = "#0e1c26")  # purple
# Add more if needed

plot <- ggplot(filtered_data, aes(x = Wvl, y = Reflect, color = as.factor(Type))) +
  geom_line() + # Use geom_point() if you want points instead of lines
  # scale_color_brewer(palette = "Set1") +
  scale_color_manual(values = custom_colors) +
  labs(title = type,
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = "") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
        legend.text = element_text(size = base_size), # Legend text at base size
        plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
        axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
        axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
        panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm"))

print(plot)
ggsave(paste0("/Users/aminnorouzi/Downloads/", type, '.png'), plot, width = 14, height = 7, dpi = 300)






library(ggplot2)
# if you want separate tick‐colour control, install.packages("ggh4x") and library(ggh4x)

# 1. compute the break positions
x_min <- min(filtered_data$Wvl, na.rm = TRUE)
x_max <- max(filtered_data$Wvl, na.rm = TRUE)

major_breaks <- seq(floor(x_min/100)*100,
                    ceiling(x_max/100)*100,
                    by = 100)
minor_breaks <- seq(floor(x_min/20)*20,
                    ceiling(x_max/20)*20,
                    by = 20)

# Zoom in for the indices by filtering wavelength
# NDTI
filtered_data_NDTI <- dplyr::filter(filtered_data, Wvl >=1500 & Wvl <=2400)

# SINDRI
filtered_data_SINDRI <- dplyr::filter(filtered_data, Wvl >=2100 & Wvl <=2400)

# CAI
filtered_data_CAI <- dplyr::filter(filtered_data, Wvl >=2000 & Wvl <=2400)

name_index <- "CAI"
# 2. build the plot
plot <- ggplot(filtered_data_CAI, aes(x = Wvl, y = Reflect, color = as.factor(Type))) +
  geom_line() +
  scale_color_manual(values = custom_colors) +
  
  # set both major and minor x‐breaks
  scale_x_continuous(breaks      = major_breaks,
                     minor_breaks = minor_breaks) +
  
  labs(title = "",
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = "") +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    # major grid lines darker
    panel.grid.major.x = element_line(color = "grey70", size = 0.5),
    # minor grid lines lighter/faded
    panel.grid.minor.x = element_line(color = "grey90", size = 0.25),
    
    # if you just want one tick colour for both:
    axis.ticks.length = unit(0.3, "cm"),
    
    # (rest of your styling…)
    legend.position     = "right",
    legend.title        = element_text(size = base_size * 1.2),
    legend.text         = element_text(size = base_size),
    plot.title          = element_text(size = base_size * 1.5, hjust = 0.5),
    axis.title          = element_text(size = base_size * 1.2),
    axis.text           = element_text(size = base_size, color = "black"),
    panel.background    = element_rect(fill = "white", colour = "white"),
    plot.background     = element_rect(fill = "white", colour = "white"),
    panel.grid.minor.y  = element_blank(),  # keep only vertical grids
    panel.grid.major.y  = element_blank(),
    axis.ticks          = element_line(color = "black")
  )

print(plot)
ggsave(paste0("/Users/aminnorouzi/Downloads/", name_index, '.png'),
       plot, width = 14, height = 7, dpi = 300)











# Soil
soil = read.csv(paste0(path_to_data, "Soil.csv"))
soil <- soil %>%
  mutate(RWC = round(RWC, 2))

# type = "Wheat Norwest Duet"
# Filter data for "Wheat Norwest Duet"
for (type in unique(soil$Type)){
  filtered_data <- soil %>%
    filter(Type == type)
  
  base_size <- 14
  
  # Plot
  plot <- ggplot(filtered_data, aes(x = Wvl, y = Reflect, color = as.factor(RWC))) +
    geom_line() + # Use geom_point() if you want points instead of lines
    labs(title = type,
         x = "Wavelength, nm",
         y = "Reflectance factor",
         color = "RWC") +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
          legend.text = element_text(size = base_size), # Legend text at base size
          plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
          axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
          axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
          panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
          plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
          panel.grid = element_blank(),
          axis.ticks = element_line(color = "black"),
          axis.line = element_line(color = "black"),
          legend.key.size = unit(0.5, "cm"))
  
  ggsave(paste0(path_to_plots, 'ref_wvl_rwc/soil/', type, '.png'), plot, width = 10, height = 7, dpi = 300)
}

#######################################
              #Dry signatures
#######################################

# Residue
driest_residue <- residue %>%
  group_by(Type) %>%
  filter(RWC == min(RWC)) %>%
  ungroup()

base_size <- 14

# Plot
plot <- ggplot(driest_residue, aes(x = Wvl, y = Reflect, color = as.factor(Type))) +
  geom_line() + # Use geom_point() if you want points instead of lines
  labs(title = "Dry signature",
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = "Crop") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
        legend.text = element_text(size = base_size), # Legend text at base size
        plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
        axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
        axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
        panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm"))
print(plot)
ggsave(paste0(path_to_plots, 'dry_sig/', 'residues.png'), plot, width = 10, height = 7, dpi = 300)


# Soil
driest_soil <- soil %>%
  group_by(Type) %>%
  filter(RWC == min(RWC)) %>%
  ungroup()

base_size <- 14

# Plot
plot <- ggplot(driest_soil, aes(x = Wvl, y = Reflect, color = as.factor(Type))) +
  geom_line() + # Use geom_point() if you want points instead of lines
  labs(title = "Dry signature",
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = "Soil") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
        legend.text = element_text(size = base_size), # Legend text at base size
        plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
        axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
        axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
        panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm"))
print(plot)
ggsave(paste0(path_to_plots, 'dry_sig/', 'soil.png'), plot, width = 10, height = 7, dpi = 300)






# install.packages("RColorBrewer")  # if you haven't already
library(RColorBrewer)

# get the 12 soil types
soil_types <- unique(driest_soil$Type)

# grab a 12‐color palette (e.g. “Set3” has up to 12 colors)
soil_palette <- brewer.pal(n = length(soil_types), name = "Set3")

# build your named vector
custom_colors_soil <- setNames(soil_palette, soil_types)

# View it
custom_colors_soil

# Add more if needed
# 1. compute the break positions
x_min <- min(filtered_data$Wvl, na.rm = TRUE)
x_max <- max(filtered_data$Wvl, na.rm = TRUE)

major_breaks <- seq(floor(x_min/100)*100,
                    ceiling(x_max/100)*100,
                    by = 100)
minor_breaks <- seq(floor(x_min/20)*20,
                    ceiling(x_max/20)*20,
                    by = 20)

# 2. build the plot
plot <- ggplot(driest_soil, aes(x = Wvl, y = Reflect, color = as.factor(Type))) +
  geom_line() +
  scale_color_manual(values = custom_colors_soil) +
  
  # set both major and minor x‐breaks
  scale_x_continuous(breaks      = major_breaks,
                     minor_breaks = minor_breaks) +
  
  labs(title = "",
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = "") +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    # major grid lines darker
    panel.grid.major.x = element_line(color = "grey70", size = 0.5),
    # minor grid lines lighter/faded
    panel.grid.minor.x = element_line(color = "grey90", size = 0.25),
    
    # if you just want one tick colour for both:
    axis.ticks.length = unit(0.3, "cm"),
    
    # (rest of your styling…)
    legend.position     = "right",
    legend.title        = element_text(size = base_size * 1.2),
    legend.text         = element_text(size = base_size),
    plot.title          = element_text(size = base_size * 1.5, hjust = 0.5),
    axis.title          = element_text(size = base_size * 1.2),
    axis.text           = element_text(size = base_size, color = "black"),
    panel.background    = element_rect(fill = "white", colour = "white"),
    plot.background     = element_rect(fill = "white", colour = "white"),
    panel.grid.minor.y  = element_blank(),  # keep only vertical grids
    panel.grid.major.y  = element_blank(),
    axis.ticks          = element_line(color = "black")
  )

print(plot)
ggsave(paste0("/Users/aminnorouzi/Downloads/", type, '.png'),
       plot, width = 14, height = 7, dpi = 300)







# Combined
merged_df <- rbind(driest_residue, driest_soil)
base_size <- 14

# Plot
plot <- ggplot(merged_df, aes(x = Wvl, y = Reflect, color = as.factor(Sample))) +
  geom_line() + # Use geom_point() if you want points instead of lines
  labs(title = "Dry signatures",
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = " ") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
        legend.text = element_text(size = base_size), # Legend text at base size
        plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
        axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
        axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
        panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm"))
print(plot)
ggsave(paste0(path_to_plots, 'dry_sig/', 'merged.png'), plot, width = 10, height = 7, dpi = 300)


###################################
# 0.5 and dry RWC difference
###################################

######## Residue ########
# Assuming your data frame is named 'df'
df <- residue


# Calculate minimum RWC and RWC closest to 0.5 for each crop type
min_and_closest_rwc <- df %>%
  group_by(Type) %>%
  summarise(
    Min_RWC = min(RWC),
    Closest_RWC = RWC[which.min(abs(RWC - 0.5))]
  ) %>%
  ungroup()

# Filter original data to include only rows matching the Min_RWC or Closest_RWC for each Type
filtered_df <- df %>%
  inner_join(min_and_closest_rwc, by = "Type") %>%
  filter(RWC == Min_RWC | RWC == Closest_RWC)

# Separate the data into two parts for easier manipulation
driest_df <- filtered_df %>% filter(RWC == Min_RWC)
closest_df <- filtered_df %>% filter(RWC == Closest_RWC)

# Assuming same wavelengths are used in all measurements and each type has a unique driest and closest RWC
combined_df <- driest_df %>%
  select(Type, Wvl, Reflect) %>%
  rename(Driest_Reflect = Reflect) %>%
  inner_join(closest_df %>% select(Type, Wvl, Reflect) %>% rename(Closest_Reflect = Reflect), by = c("Type", "Wvl"))

# Calculate the difference
combined_df <- combined_df %>%
  mutate(ref_dif = Driest_Reflect - Closest_Reflect)

base_size <- 14

# Plot
plot <- ggplot(combined_df, aes(x = Wvl, y = ref_dif, color = as.factor(Type))) +
  geom_line() + # Use geom_point() if you want points instead of lines
  labs(title = 'reflectance difference (Driest & ~0.5 RWC)',
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = "") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
        legend.text = element_text(size = base_size), # Legend text at base size
        plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
        axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
        axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
        panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm"))
print(plot)
ggsave(paste0(path_to_plots, 'Reflect_difference/', 'residue_0.5.png'), plot, width = 10, height = 7, dpi = 300)

######## Soil ########
# Assuming your data frame is named 'df'
df <- soil


# Calculate minimum RWC and RWC closest to 0.5 for each crop type
min_and_closest_rwc <- df %>%
  group_by(Type) %>%
  summarise(
    Min_RWC = min(RWC),
    Closest_RWC = RWC[which.min(abs(RWC - 0.5))]
  ) %>%
  ungroup()

# Filter original data to include only rows matching the Min_RWC or Closest_RWC for each Type
filtered_df <- df %>%
  inner_join(min_and_closest_rwc, by = "Type") %>%
  filter(RWC == Min_RWC | RWC == Closest_RWC)

# Separate the data into two parts for easier manipulation
driest_df <- filtered_df %>% filter(RWC == Min_RWC)
closest_df <- filtered_df %>% filter(RWC == Closest_RWC)

# Assuming same wavelengths are used in all measurements and each type has a unique driest and closest RWC
combined_df <- driest_df %>%
  select(Type, Wvl, Reflect) %>%
  rename(Driest_Reflect = Reflect) %>%
  inner_join(closest_df %>% select(Type, Wvl, Reflect) %>% rename(Closest_Reflect = Reflect), by = c("Type", "Wvl"))

# Calculate the difference
combined_df <- combined_df %>%
  mutate(ref_dif = Driest_Reflect - Closest_Reflect)

base_size <- 14

# Plot
plot <- ggplot(combined_df, aes(x = Wvl, y = ref_dif, color = as.factor(Type))) +
  geom_line() + # Use geom_point() if you want points instead of lines
  labs(title = 'reflectance difference (Driest & ~0.5 RWC)',
       x = "Wavelength, nm",
       y = "Reflectance factor",
       color = "") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
        legend.text = element_text(size = base_size), # Legend text at base size
        plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
        axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
        axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
        panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm"))
print(plot)
ggsave(paste0(path_to_plots, 'Reflect_difference/', 'soil_0.5.png'), plot, width = 10, height = 7, dpi = 300)


###################################
# All RWCs and driest difference
###################################

# Assuming your data frame is named 'df'
df <- soil

# Identify the driest (minimum RWC) for each crop type
driest_df <- df %>%
  group_by(Type) %>%
  summarise(Min_RWC = min(RWC)) %>%
  ungroup() %>%
  inner_join(df, by = "Type") %>%
  filter(RWC == Min_RWC) %>%
  select(-Min_RWC)

# Calculate reflectance difference for each RWC level compared to the driest
reflectance_diff_df <- df %>%
  inner_join(driest_df, by = c("Type", "Wvl"), suffix = c("", "_driest")) %>%
  mutate(ref_dif = Reflect_driest - Reflect, 
         Difference_Label = paste("Diff to RWC", RWC)) %>%
  select(Type, Wvl, RWC, Reflect, Reflect_driest, ref_dif, Difference_Label)


for (type in unique(reflectance_diff_df$Type)){
  filtered_df <- reflectance_diff_df %>%
    dplyr::filter( Type == type)
  base_size <- 14
  
  # Plot
  plot <- ggplot(filtered_df, aes(x = Wvl, y = ref_dif, color = as.factor(Difference_Label))) +
    geom_line() + # Use geom_point() if you want points instead of lines
    labs(title = type,
         x = "Wavelength, nm",
         y = "Reflectance factor",
         color = "") +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_text(size = base_size * 1.2), # Legend title larger than base size
          legend.text = element_text(size = base_size), # Legend text at base size
          plot.title = element_text(size = base_size * 1.5, hjust = 0.5), # Title larger than base size
          axis.title = element_text(size = base_size * 1.2), # Axis titles larger than base size
          axis.text = element_text(size = base_size, color = "black"), # Axis text at base size
          panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
          plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
          panel.grid = element_blank(),
          axis.ticks = element_line(color = "black"),
          axis.line = element_line(color = "black"),
          legend.key.size = unit(0.5, "cm"))
  print(plot)
  ggsave(paste0(path_to_plots, 'Reflect_difference/all_RWCs/soil/', type, '.png'), plot, width = 10, height = 7, dpi = 300)
  
}



