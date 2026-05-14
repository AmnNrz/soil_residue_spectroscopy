library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

# path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
#                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
#                        'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')
# 
# path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
#                         'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
#                         'Ph.D/Projects/Soil_Residue_Spectroscopy/Plots/00/')

path_to_data <- paste0('/home/amin-norouzi/OneDrive/Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')

path_to_plots <- paste0('/home/amin-norouzi/OneDrive/Ph.D/Projects/Soil_Residue_Spectroscopy/Plots/00/')

residue <- read.csv(paste0(path_to_data, 
                                  "Residue_RWCinterpolated.csv"),
                           header = TRUE, row.names = NULL)

soil <- read.csv(paste0(path_to_data, 
                               "Soil_RWCinterpolated.csv"),
                        header = TRUE, row.names = NULL)



residue <- residue %>%
  rename(Type = Crop)

soil <- soil %>%
  rename(Type = Soil)

residue <- residue %>%
  rename(Reflect = Reflectance)

soil <- soil %>%
  rename(Reflect = Reflectance)

residue <- residue[residue$Wvl > 1500, ]
soil <- soil[soil$Wvl > 1500, ]

select_columns_range <- function(df, start_col_name, end_col_name) {
  start_col <- which(names(df) == start_col_name)
  end_col <- which(names(df) == end_col_name)
  if (start_col == 0 || end_col == 0) {
    stop("One of the specified column names does not exist in the dataframe.")
  }
  selected_df <- df[, start_col:end_col]
  return(selected_df)
}

residue <- residue %>%
  dplyr::filter(Wvl >= 1500 | Wvl <= 2400)


res_index <- residue %>%
  spread(Wvl, Reflect) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000)
  # mutate(R2220 = 2200 / 2000) %>%
  # mutate(R1620 = 2200 / 2000) %>%
  # mutate(RSWIR = 2200 / 2000) %>%
  # mutate(ROLI = 2200 / 2000)


res_index$R2025_2035 <- rowMeans(select_columns_range(res_index, '2025', '2035'))
res_index$R2095_2105 <- rowMeans(select_columns_range(res_index, '2095', '2105'))
res_index$R2245_2255 <- rowMeans(select_columns_range(res_index, '2245', '2255'))
res_index$CAI <- (0.5 * (res_index$R2025_2035 + res_index$R2245_2255) - res_index$R2095_2105)

res_index$R2220_2260 <-  rowMeans(select_columns_range(res_index, '2220', '2260'))
res_index$R2310_2360 <-  rowMeans(select_columns_range(res_index, '2310', '2360'))
res_index$SINDRI <- 100 * (res_index$R2220_2260 - res_index$R2310_2360) / (res_index$R2220_2260 + res_index$R2310_2360)

res_index$R1600_1680 <-  rowMeans(select_columns_range(res_index, '1600', '1680'))
res_index$R2160_2340 <-  rowMeans(select_columns_range(res_index, '2160', '2340'))
res_index$NDTI <- (res_index$R1600_1680 - res_index$R2160_2340) / (res_index$R1600_1680 + res_index$R2160_2340)

# res_index$R2220 <- res_index$`2250`/res_index$R2025_2035
# res_index$R1620 <- res_index$`1600`/res_index$R2025_2035
# res_index$RSWIR <- res_index$`1660`/res_index$`R2260_2280`
# res_index$ROLI <- res_index$`1660`/res_index$R2160_2340

# desired_column <- c("Sample", "Type", "RWC", "CAI", "SINDRI", "NDTI", "R2025_2035", "R2095_2105", "R2245_2255",
#                     "R2220_2260", "R2310_2360", "R1600_1680", "R2160_2340")

desired_column <- c("Sample", "Type", "RWC", "CAI", "SINDRI", "NDTI")

res_index <- res_index[, desired_column]

write.csv(res_index, file = paste0(path_to_data, "residue_index_df.csv"), row.names = FALSE)


soil <- soil %>%
  dplyr::filter(Wvl >= 1500 | Wvl <= 2400)


soil_index <- soil %>%
  spread(Wvl, Reflect) %>%
  mutate(CAI = 2200 / 2000) %>%
  mutate(SINDRI = 2200 / 2000) %>%
  mutate(NDTI = 2200 / 2000)
  # mutate(R2220 = 2200 / 2000)%>%
  # mutate(R1620 = 2200 / 2000)%>%
  # mutate(RSWIR = 2200 / 2000)%>%
  # mutate(ROLI = 2200 / 2000)

soil_index$R2025_2035 <- rowMeans(select_columns_range(soil_index, '2025', '2035'))
soil_index$R2095_2105 <- rowMeans(select_columns_range(soil_index, '2095', '2105'))
soil_index$R2245_2255 <- rowMeans(select_columns_range(soil_index, '2245', '2255'))
soil_index$CAI <- (0.5 * (soil_index$R2025_2035 + soil_index$R2245_2255) - soil_index$R2095_2105)

soil_index$R2220_2260 <-  rowMeans(select_columns_range(soil_index, '2220', '2260'))
soil_index$R2310_2360 <-  rowMeans(select_columns_range(soil_index, '2310', '2360'))
soil_index$SINDRI <- 100 * (soil_index$R2220_2260 - soil_index$R2310_2360) / (soil_index$R2220_2260 + soil_index$R2310_2360)

soil_index$R1600_1680 <-  rowMeans(select_columns_range(soil_index, '1600', '1680'))
soil_index$R2160_2340 <-  rowMeans(select_columns_range(soil_index, '2160', '2340'))
soil_index$NDTI <- (soil_index$R1600_1680 - soil_index$R2160_2340) / (soil_index$R1600_1680 + soil_index$R2160_2340)


soil_index <- soil_index[, desired_column]

write.csv(soil_index, file = paste0(path_to_data, "soil_index_df.csv"), row.names = FALSE)

comb_index <- rbind(soil_index, res_index)

write.csv(comb_index, file = paste0(path_to_data, "index_combined.csv"), row.names = FALSE)

##########################################################
##########################################################
#############  RWC ~ INDEX RATIO (FACET)   ###############
library(ggplot2)
library(patchwork)
library(viridis)

res_index$color_var <- ifelse(res_index$Sample == "Soil", "Soil", res_index$Type)
color_levels <- unique(res_index$color_var)
res_index$color_var <- factor(res_index$color_var, levels = c("Soil", color_levels[color_levels != "Soil"]))

p3 <- ggplot(res_index, aes(x = RWC, y = R2220, shape = Sample, color = Sample)) + 
  scale_shape_manual(values=seq_along(unique(res_index$color_var))) +
  geom_point(size=2) +
  geom_smooth(method = "loess", se=FALSE, span = TRUE,
              fullrange = TRUE, size = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0.8, 2), breaks = seq(0.8, 2, by = 0.2)) +
  scale_color_viridis(discrete = TRUE) +  # Add this line for viridis color scale
  labs(x = "RWC", y = "R2.2/R2.0", color = "Sample", shape = "Sample") +
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),           # Remove major grid lines
        panel.grid.minor = element_blank(),           # Remove minor grid lines
        axis.line = element_line(color = "black"),
        strip.background = element_rect(fill = "white") # Remove facet strip background
  ) +
  coord_flip()
  # scale_color_manual(values = my_colors) # This line sets the colors.

p4 <- ggplot(res_index, aes(x = RWC, y = R1620, group = Sample, shape = Sample, color = Sample)) + 
  scale_shape_manual(values=seq_along(unique(res_index$Sample))) +
  geom_point(size=2) +
  geom_smooth(method = "loess", se=FALSE, span = TRUE,
              fullrange = TRUE, size = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0.8, 2), breaks = seq(0.8, 2, by = 0.4)) +
  scale_color_viridis(discrete = TRUE) +  # Add this line for viridis color scale
  labs(x = "RWC", y = "R1.6/R2.0", color = "Sample", shape = "Sample") +
  theme(text = element_text(size = 10), legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),           # Remove major grid lines
        panel.grid.minor = element_blank(),           # Remove minor grid lines
        axis.line = element_line(color = "black"),
        strip.background = element_rect(fill = "white") # Remove facet strip background
  ) +
  coord_flip()
  # scale_color_manual(values = my_colors) # This line sets the colors.

p5 <- ggplot(res_index, aes(x = RWC, y = RSWIR, group = Sample, shape = Sample, color = Sample)) + 
  scale_shape_manual(values=seq_along(unique(res_index$Sample))) +
  geom_point(size=2) +
  geom_smooth(method = "loess", se=FALSE, span = TRUE,
              fullrange = TRUE, size = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_color_viridis(discrete = TRUE) +  # Add this line for viridis color scale
  labs(x = "RWC", y = "SWIR3/SWIR6", color = "Sample", shape = "Sample") +
  theme(text = element_text(size = 10), legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),           # Remove major grid lines
        panel.grid.minor = element_blank(),           # Remove minor grid lines
        axis.line = element_line(color = "black"),
        strip.background = element_rect(fill = "white") # Remove facet strip background
  ) +
  coord_flip()
  # scale_color_manual(values = my_colors) # This line sets the colors.

p6 <- ggplot(res_index, aes(x = RWC, y = ROLI, group = Sample, shape = Sample, color = Sample)) + 
  scale_shape_manual(values=seq_along(unique(res_index$Sample))) +
  geom_point(size=2) +
  geom_smooth(method = "loess", se=FALSE, span = TRUE,
              fullrange = TRUE, size = 0.4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_color_viridis(discrete = TRUE) +  # Add this line for viridis color scale
  labs(x = "RWC", y = "OLI6/OLI7", color = "Sample", shape = "Sample") +
  theme(text = element_text(size = 10), legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),           # Remove major grid lines
        panel.grid.minor = element_blank(),           # Remove minor grid lines
        axis.line = element_line(color = "black"),
        strip.background = element_rect(fill = "white") # Remove facet strip background
  )+
  coord_flip()
  # scale_color_manual(values = my_colors) # This line sets the colors.

library(patchwork)

print(p6)
combined_plot <- (p3 + p4 + p5 + p6) + plot_layout(ncol=2)
combined_plot <- combined_plot & theme(legend.position = "bottom")
print(combined_plot)

ggsave(filename = paste0(path_to_plots, "ReflectRatio_RWC.png"), plot = combined_plot, width = 8.3, height = 5.8, dpi = 200)


## Chatgpt's suggestion

library(RColorBrewer)
library(viridis)

n_colors <- length(color_levels)
my_colors <- viridis(n_colors)
my_colors <- viridis_pal(option = "viridis")(n_colors)
scale_color_manual(values = my_colors)


##########################################################
##########################################################
################  RWC ~ INDEX (FACET)   ###############
library(ggplot2)
library(tidyr)
library(viridis)

# Reshape the data into a tidy format
tidy_data <- res_index %>%
  gather(variable, value, SINDRI, CAI, NDTI)

tidy_data$color_group <- ifelse(tidy_data$Sample == "Residue", 
                                paste(tidy_data$Sample, tidy_data$Type),
                                tidy_data$Sample)
# Define shapes
my_shapes <- c(16:25)  # Adjust based on the number of unique values in color_group
unique_groups <- unique(tidy_data$color_group)
if (length(unique_groups) > length(my_shapes)) {
  stop("You need more shapes!")
}
# color_palette <- c(residue_colors, soil_color)

distinct_shapes <- c(1, 2, 5, 8, 0, 6, 3, 4, 7, 9)  # Add more shapes here as needed
unique_groups <- unique(tidy_data$color_group)
if (length(unique_groups) > length(distinct_shapes)) {
  stop("You need more shapes!")
}

ggplot(tidy_data, aes(x = RWC, y = value, group = Type)) +
  geom_point(aes(shape = color_group), size = 1.5) +
  geom_smooth(method = 'loess', se = FALSE, size = 0.7, aes(color = color_group)) + # Smooth line
  # geom_line(color = viridis(1)[1], size = 0.4) +  # Set a fixed viridis color for all lines
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "") +
  scale_shape_manual(values = distinct_shapes) +  # Use distinct shapes here
  scale_color_viridis(discrete = TRUE) + # Use viridis color palette
  facet_grid(variable ~ ., scales = "free_y") +
  theme_minimal() + # Apply the minimalistic theme
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),           # Remove major grid lines
    panel.grid.minor = element_blank(),           # Remove minor grid lines
    strip.background = element_rect(fill = "white"), # Remove facet strip background
    axis.text.y = element_text(size = 12), 
    text = element_text(size = 12)
  ) +
  labs(shape = "Sample", color = "Sample")

my_plot <- ggplot(tidy_data, aes(x = RWC, y = value, group = Type)) +
  geom_point(aes(shape = color_group), size = 1.5) +
  geom_smooth(method = 'loess', se = FALSE, size = 0.4, aes(color = color_group)) + # Smooth line
  # geom_line(color = viridis(1)[1], size = 0.4) +  # Set a fixed viridis color for all lines
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  labs(x = "RWC", y = "") +
  scale_shape_manual(values = distinct_shapes) +  # Use distinct shapes here
  scale_color_viridis(discrete = TRUE) + # Use viridis color palette
  facet_grid(variable ~ ., scales = "free_y") +
  theme_minimal() + # Apply the minimalistic theme
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),           # Remove major grid lines
    panel.grid.minor = element_blank(),           # Remove minor grid lines
    strip.background = element_rect(fill = "white"), # Remove facet strip background
    axis.text.y = element_text(size = 12), 
    text = element_text(size = 12)
  ) +
  labs(shape = "Sample", color = "Sample")
print(my_plot)
ggsave(filename = paste0(path_to_plots
                         , "Index_RWC.png"), plot = my_plot, width = 8.3, height = 5.8, dpi = 200)

##########################################################
##########################################################
#############  RWC ~ Reflectance (FACET)   ###############
library(ggplot2)
library(patchwork)

res_index$color_var <- ifelse(res_index$Sample == "Soil", "Soil", res_index$Type)
color_levels <- unique(res_index$color_var)
res_index$color_var <- factor(res_index$color_var, levels = c("Soil", color_levels[color_levels != "Soil"]))

write.csv(res_index, file = paste0(path_to_data, "res_index_test.csv"), row.names = FALSE)


df_long <- res_index %>%
  pivot_longer(cols = c("2000", "2250", "2090", "R2220_2260",
             "R1660_1690", "R2220_2280", "R2260_2280"),
              names_to = "Wvl", values_to = "Reflect")


# df_long1 <- res_index %>%
#   dplyr::filter(Sample == "Soil") %>%
#   pivot_longer(
#     cols = c("2160", "2190", "2180"), names_to = "Wvl", values_to = "Reflect")

# df_long2 <- res_index %>%
#   dplyr::filter(Sample == "Residue") %>%
#   pivot_longer(
#     cols = c("2000", "2250", "2090"), names_to = "Wvl", values_to = "Reflect")
# 
# df_long3 <- res_index %>%
#   pivot_longer(
#     cols = c("R2220_2260", "R1660_1690", "R2220_2280", "R2260_2280"), names_to = "Wvl", values_to = "Reflect")
# 
# columns = c("Sample", "Type", "Scan", "RWC", "color_var","Wvl", "Reflect")
# 
# df_long1 <-  df_long1[, columns]
# df_long2 <-  df_long2[, columns]
# df_long3 <-  df_long3[, columns]
# 
# df_long <- rbind(df_long1, df_long2, df_long3)

df_long <- df_long %>%
  mutate(Wvl = case_when(
    Wvl == 2160 ~ "R2.0",
    Wvl == 2180 ~ "R2.1",
    Wvl == 2190 ~ "R2.2",
    # Wvl == 2000 ~ "R2.0",
    # Wvl == 2090 ~ "R2.1",
    # Wvl == 2250 ~ "R2.2",
    Wvl == "R2220_2260" ~ "SWIR6",
    Wvl == "R2260_2280" ~ "SWIR7",
    Wvl == "R1660_1690" ~ "OLI6",
    Wvl == "R2220_2280" ~ "OLI7",
    TRUE ~ "Other"
  ))

df_long$shape_value <- paste(df_long$Type, as.character(df_long$Wvl))

df_Soil <- df_long[df_long$Sample %in% "Soil", ]
df_Res <- df_long[df_long$Sample %in% "Residue", ]

for (soil in unique(df_Soil$Type)) {
  for (crp in unique(df_Res$Type)){
    # Filter dataframe based on multiple string values in the column
    values_to_include <- c(soil, crp)  # Values to filter for
    df <- df_long[df_long$Type %in% values_to_include, ]
    
    # List to hold data frames with predictions for each category
    list_of_dfs <- list()
    
    # Loop through each unique category in `shape_value`
    for (category in unique(df$shape_value)) {
      sub_df <- subset(df, shape_value == category)
      loess_fit <- loess(Reflect ~ RWC, data = sub_df)
      predictions <- predict(loess_fit)
      residuals <- sub_df$Reflect - predictions
      sub_df <- mutate(sub_df, residuals = residuals, predictions = predictions)
      
      # Filter the data by residual, for example, within +/- 0.1 of the prediction
      sub_df <- filter(sub_df, abs(residuals) <= 2)
      
      list_of_dfs[[category]] <- sub_df
    }
    
    # Combine the filtered data back into one data frame
    df_new <- bind_rows(list_of_dfs)
    
    R_values_to_include <- c("R1.6", "R2.0", "R2.2")  # Values to filter for
    df_R216 <- df_new[df_new$Wvl %in% R_values_to_include, ]
    
    
    p1 <- ggplot(df_R216, aes(x = RWC, y = Reflect, shape = shape_value)) + 
      geom_point(size=2) +
      geom_smooth(aes(color = Sample), method = "loess", se=FALSE, span = TRUE,
                  fullrange = TRUE, size = 0.4) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
      scale_y_continuous(breaks = seq(0, 100, by = 5)) +
      labs(x = "RWC", y = "Reflectance", color = "Sample", shape = "Sample") +
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),           # Remove major grid lines
            panel.grid.minor = element_blank(),           # Remove minor grid lines
            axis.line = element_line(color = "black"),
            strip.background = element_rect(fill = "white") # Remove facet strip background
      )
    p1 <- p1 + guides(color = FALSE)
    p1 <- p1 + scale_color_manual(values = c("Soil" = "black", "Residue" = "orange"))
    
    print(p1)
    
    R_values_to_include <- c("SWIR3", "SWIR6")  # Values to filter for
    df_SWIR <- df_new[df_new$Wvl %in% R_values_to_include, ]
    
    
    p2 <- ggplot(df_SWIR, aes(x = RWC, y = Reflect, shape = shape_value)) + 
      geom_point(size=2) +
      geom_smooth(aes(color = Sample), method = "loess", se=FALSE, span = TRUE,
                  fullrange = TRUE, size = 0.4) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
      scale_y_continuous(breaks = seq(0, 100, by = 5)) +
      labs(x = "RWC", y = "Reflectance", color = "Sample", shape = "Sample") +
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),           # Remove major grid lines
            panel.grid.minor = element_blank(),           # Remove minor grid lines
            axis.line = element_line(color = "black"),
            strip.background = element_rect(fill = "white") # Remove facet strip background
      )
    p2 <- p2 + guides(color = FALSE)
    p2 <- p2 + scale_color_manual(values = c("Soil" = "black", "Residue" = "orange"))
    
    print(p2)
    
    
    df_forOLI <- df_new %>%
      mutate(Wvl = case_when(
        
        Wvl == "R2.0" ~ "R2.0",
        Wvl == "R2.1" ~ "R2.1",
        Wvl == "R2.2" ~ "R2.2",
        Wvl == "SWIR6" ~ "SWIR6",
        Wvl == "SWIR7" ~ "SWIR7",
        Wvl == "OLI6" ~ "OLI6",
        Wvl == "OLI7" ~ "OLI7",
        TRUE ~ "Other"
      ))
    
    df_forOLI$shape_value <- paste(df_forOLI$Type, as.character(df_forOLI$Wvl))
    
    R_values_to_include <- c("OLI6", "OLI7")  # Values to filter for
    df_OLI <- df_forOLI[df_forOLI$Wvl %in% R_values_to_include, ]
    
    
    p3 <- ggplot(df_OLI, aes(x = RWC, y = Reflect, shape = shape_value)) + 
      geom_point(size=2) +
      geom_smooth(aes(color = Sample), method = "loess", se=FALSE, span = TRUE,
                  fullrange = TRUE, size = 0.4) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
      scale_y_continuous(breaks = seq(0, 100, by = 5)) +
      labs(x = "RWC", y = "Reflectance", color = "Sample", shape = "Sample") +
      theme(text = element_text(size = 10),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            panel.grid.major = element_blank(),           # Remove major grid lines
            panel.grid.minor = element_blank(),           # Remove minor grid lines
            axis.line = element_line(color = "black"),
            strip.background = element_rect(fill = "white") # Remove facet strip background
      )
    p3 <- p3 + guides(color = FALSE)
    p3 <- p3 + scale_color_manual(values = c("Soil" = "black", "Residue" = "orange"))
    
    print(p3)
    
    library(patchwork)
    
    p1 <- p1 + theme(legend.key.size = unit(0.6, "cm"))
    p2 <- p2 + theme(legend.key.size = unit(0.6, "cm"))
    p3 <- p3 + theme(legend.key.size = unit(0.6, "cm"))
    
    combined_plot <- (p1 + p2 + p3) + plot_layout(ncol=1)
    print(combined_plot)
    
    ggsave(filename = paste0(path_to_plots, "RWC_Reflectance_same_res_index_ref_2000_2250_2090/", soil,"_", crp, ".png"), plot = combined_plot, width = 5, height = 8, dpi = 300)
  }
}

