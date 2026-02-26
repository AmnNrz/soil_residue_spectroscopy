library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(scales)
library(broom)
library(gridExtra)
library(stringr)
library(ggpubr)


path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                       'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')
# path_to_data <- paste0('/home/amin-norouzi/OneDrive/Ph.D/Projects/',
#                        'Soil_Residue_Spectroscopy/Data/00/')


path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                        'Ph.D/Projects/Soil_Residue_Spectroscopy/Plots/final_plots/')
# path_to_plots <- paste0('/home/amin-norouzi/OneDrive/Ph.D/Projects/',
#                         'Soil_Residue_Spectroscopy/Plots/final_plots/')


df <- read.csv(paste0(path_to_data, 
                               "mixed_index_df.csv"),
                        header = TRUE, row.names = NULL)

df <- df %>%
  pivot_longer(
    cols = `CAI`:`NDTI`,      # all spectral bands
    names_to = "index_name",         # name of the new wavelength column
    values_to = "index"     # name of the new reflectance value column
  )

df <- df %>%
  dplyr::rename(crop = Crop)

df <- df %>%
  dplyr::rename(soil = Soil)

df <- df %>%
  dplyr::rename(Fraction_Residue_Cover = Fraction)


# Add fresh/weathered column 
fresh <- c("Canola", "Garbanzo Beans", "Peas", "Wheat Norwest Duet")

weathered <- c("Weathered Canola", "Weathered Wheat")
df <- df %>% 
  mutate(age = case_when(
    crop %in% fresh ~ "fresh", 
    crop %in% weathered ~ "weathered"
  ))

# Remove weathered wheat and weathered canola
fresh_df <- df %>% dplyr::filter(crop %in% fresh)

filtered_fresh <- fresh_df %>% dplyr::filter(index_name == "NDTI" & crop == "Canola")

# # Filter the DataFrame for specific RWC values
# df <- df %>%
#   filter(RWC %in% c(0, 0.2, 0.4, 0.6, 0.8, 1))
# 
# # Filter for RWC = 0
# dry_df <- df %>% dplyr::filter(RWC == 0)

dry_df <- df

# Filter for fresh crops
df_to_plot <- dry_df %>% dplyr::filter(age == "fresh")
base_size = 14

# Define custom colors (one for each crop)
custom_colors <- c("Canola" = "#fcca46", "Garbanzo Beans" = "#233d4d", "Peas" = "#fe7f2d", 
                   "Wheat" = "#619b8a", "Wheat Pritchett" = "#a1c181")  

# for (sl in unique(df_to_plot$soil)) {
#   for (idx_ in unique(df_to_plot$index_name)) {
#     filtered <- df_to_plot %>%
#       filter(soil == sl, index_name == idx_)
#     
#     filtered <- filtered %>%
#       mutate(crop = if_else(crop == "Wheat Norwest Duet", "Wheat", crop))
#     
#     
#     # Calculate slopes for each crop and rename slope to avoid conflicts
#     slopes_intercepts <- filtered %>%
#       group_by(crop) %>%
#       summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
#                 intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
#       ungroup()
#     
#     # Modify the crop labels to include the slope
#     filtered <- filtered %>%
#       left_join(slopes_intercepts, by = "crop") %>%
#       mutate(
#         crop_label = paste0(
#           crop, " (", round(slope_value, 2), ",", round(intercept_value, 2) ,")"
#           )
#         )
# 
#     
#     # Dynamically create the labels for the legend
#     local_crop_labels <- filtered %>%
#       distinct(crop, crop_label) %>%
#       pull(crop_label, crop)
#     
#     p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(crop))) +
#       geom_point(size = 2.5) + # Adds points to the plot
#       # geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
#       facet_wrap(~index_name, scales = "fixed") + # Creates a separate plot for each level of index_name
#       scale_color_manual(name = "Crop residue (slope, intercept)", values = custom_colors,
#                          labels = local_crop_labels) +
#       labs(x = idx_, y = "Fraction Residue Cover") + # Labels for axes
#       theme_minimal() + # Minimal theme for cleaner look
#       theme(legend.position = "right",
#             legend.title = element_text(size = base_size), # Legend title larger than base size
#             legend.text = element_text(size = base_size * 0.8), # Legend text at base size
#             axis.title = element_text(size = base_size), # Axis titles larger than base size
#             axis.text = element_text(size = base_size * 0.8, color = "black"), # Axis text at base size
#             panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
#             plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
#             panel.grid = element_blank(),
#             axis.ticks = element_line(color = "black", size = base_size * 0.8),
#             axis.line = element_line(color = "black"),
#             legend.key.size = unit(0.5, "cm"),
#             plot.title = element_blank(),
#             strip.text = element_blank()) +
#       scale_y_continuous(limits = c(0, 1))
#       # coord_fixed(ratio = 1)
#     
#     # # Print the plot
#     print(p)
#     # ggsave(paste0(path_to_plots, 'fr_index_dry_fits/fresh/', sl, "_", filtered$index_name[1], '.png'), 
#     #        p, width = 7, height = 3.5, dpi = 300)
#   }
# }


################################################
################################################
################ Put them in a facet
################
library(ggplot2)
library(dplyr)
library(cowplot)  # For plot_grid()

base_size = 14


# Create a global crop legend plot
global_crop_legend <- ggplot(df_to_plot %>% 
                               mutate(crop = if_else(crop == "Wheat Norwest Duet", "Wheat", crop)),
                             aes(x = 1, y = 1, color = as.factor(crop))) +
  geom_point(size = 3) +
  scale_color_manual(name = "", values = custom_colors) +  # Use the same custom colors
  theme_minimal() +
  theme(
    legend.position = "right",  # Place the legend on the right
    legend.title = element_text(size = base_size * 1.2),
    legend.text = element_text(size = base_size * 1.2, margin = margin(r = 10)),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Extract the legend from the global_crop_legend
global_legend <- cowplot::get_legend(global_crop_legend)


# Determine global limits for x and y axes
x_limits <- range(df_to_plot$index, na.rm = TRUE)
y_limits <- c(0, 1)  # Fraction Residue Cover already set to 0-1


for (sl in unique(df_to_plot$soil)) {
  
  plots_list <- list()
  extracted_legend <- NULL
  
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(soil == sl, index_name == idx_)
    
    filtered <- filtered %>%
      mutate(crop = if_else(crop == "Wheat Norwest Duet", "Wheat", crop))
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes_intercepts <- filtered %>%
      group_by(crop) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
                intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes_intercepts, by = "crop") %>%
      mutate(
        crop_label = paste0(round(slope_value, 2), ",", round(intercept_value, 2)
        )
      )
    
    # Dynamically create the labels for the legend
    local_crop_labels <- filtered %>%
      distinct(crop, crop_label) %>%
      pull(crop_label, crop)
    
    # Apply consistent theme and conditional y-axis title
    # y_axis_theme <- if (idx_ == "NDTI") theme() else theme(axis.title.y = element_blank())
    
    # Apply consistent theme and conditional y-axis settings
    y_axis_theme <- if (idx_ == unique(df_to_plot$index_name)[1]) {
      theme()  # Keep y-axis elements for the leftmost plot
    } else {
      theme(
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.line.y = element_blank()    # Remove y-axis line
      )
    }
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(crop))) +
      geom_point(size = 3.5) +
      scale_color_manual(name = "slope, intercept", values = custom_colors,
                         labels = local_crop_labels) +
      labs(x = idx_, y = if (idx_ == "NDTI") "Fraction Residue Cover" else NULL) +
      theme_minimal() +
      theme(legend.position = c(0.35, -0.02),
            legend.justification = c(0, 0),
            legend.title = element_text(size = base_size * 1.2),
            legend.text = element_text(size = base_size * 1.2, margin = margin(b = 0)),
            axis.title = element_text(size = base_size * 1.2),
            axis.text = element_text(size = base_size * 1.2, color = "black"),
            panel.background = element_rect(fill = "white", colour = "white"),
            plot.background = element_rect(fill = "white", colour = "white"),
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_blank(),
            strip.text = element_blank(),
            axis.text.x = element_text(size = base_size * 1.2, color = "black", angle = 45, hjust = 1),  # Smaller x-axis tick labels
            axis.text.y = element_text(size = base_size * 1.2, color = "black"),  # Smaller y-axis tick labels
            plot.margin = unit(c(0.02, 0.02, 0.02, 0.02), "cm")) +  # Ensure equal margins
      coord_fixed(ratio = diff(y_limits) / diff(x_limits)) +  # Ensure square plots
      y_axis_theme  # Apply the conditional y-axis theme
    
    # Store the plot in the list
    plots_list[[idx_]] <- p
  }
  
  # Arrange the plots side by side, ensuring alignment and consistent size
  combined_plot <- plot_grid(plotlist = plots_list, ncol = 3, align = 'hv')
  
  # Add the global legend to the right of the combined plot
  final_plot <- plot_grid(combined_plot, global_legend, rel_widths = c(4, 0.6))
  
  print(final_plot)
  
  # Save the combined plot
  ggsave(paste0(path_to_plots, 'fr_index_dry_fits/fresh/', sl, "_combined.png"),
         final_plot, width = 14, height = 7, dpi = 300)
}

write.csv(df_to_plot, file = paste0(path_to_data, "figure_2_data.csv"), row.names = FALSE)

################
# For AGU poster
################
################
# Define custom colors (one for each crop)
custom_colors <- c("Canola" = "#15616d", "Garbanzo Beans" = "#fcca46", "Peas" = "#ff7d00", 
                   "Wheat" = "#78290f", "Wheat Pritchett" = "#a1c181")  

# Create a global crop legend plot
global_crop_legend <- ggplot(df_to_plot %>% 
                               mutate(crop = if_else(crop == "Wheat Norwest Duet", "Wheat", crop)),
                             aes(x = 1, y = 1, color = as.factor(crop))) +
  geom_point(size = 3) +
  scale_color_manual(name = "", values = custom_colors) +  # Use the same custom colors
  theme_minimal() +
  theme(
    legend.position = "right",  # Place the legend on the right
    legend.title = element_text(size = base_size * 1.2, color="black"),
    legend.text = element_text(size = base_size * 1.2, margin = margin(r = 10), color="black"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Extract the legend from the global_crop_legend
global_legend <- cowplot::get_legend(global_crop_legend)


for (sl in unique(df_to_plot$soil)) {
  
  plots_list <- list()
  extracted_legend <- NULL
  
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(soil == sl, index_name == idx_)
    
    filtered <- filtered %>%
      mutate(crop = if_else(crop == "Wheat Norwest Duet", "Wheat", crop))
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes_intercepts <- filtered %>%
      group_by(crop) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
                intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes_intercepts, by = "crop") %>%
      mutate(
        crop_label = paste0(round(slope_value, 2), ",", round(intercept_value, 2)
        )
      )
    
    # Dynamically create the labels for the legend
    local_crop_labels <- filtered %>%
      distinct(crop, crop_label) %>%
      pull(crop_label, crop)
    
    # Apply consistent theme and conditional y-axis title
    # y_axis_theme <- if (idx_ == "NDTI") theme() else theme(axis.title.y = element_blank())
    
    # Apply consistent theme and conditional y-axis settings
    y_axis_theme <- if (idx_ == unique(df_to_plot$index_name)[1]) {
      theme()  # Keep y-axis elements for the leftmost plot
    } else {
      theme(
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.line.y = element_blank()    # Remove y-axis line
      )
    }
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(crop))) +
      geom_point(size = 3.5) +
      scale_color_manual(name = "slope, intercept", values = custom_colors,
                         labels = local_crop_labels) +
      labs(x = idx_, y = if (idx_ == "NDTI") "Fraction Residue Cover" else NULL) +
      theme_minimal() +
      theme(legend.position = c(0.35, -0.02),
            legend.justification = c(0, 0),
            legend.title = element_text(size = base_size * 1.2, color="black"),
            legend.text = element_text(size = base_size * 1.2, margin = margin(b = 0), color="black"),
            axis.title = element_text(size = base_size * 1.2, color="black"),
            axis.text = element_text(size = base_size * 1.2, color = "black"),
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_blank(),
            strip.text = element_blank(),
            axis.text.x = element_text(size = base_size * 1.2, color = "black", angle = 45, hjust = 1),  # Smaller x-axis tick labels
            axis.text.y = element_text(size = base_size * 1.2, color = "black"),  # Smaller y-axis tick labels
            plot.margin = unit(c(0.02, 0.02, 0.02, 0.02), "cm")) +  # Ensure equal margins
      coord_fixed(ratio = diff(y_limits) / diff(x_limits)) +  # Ensure square plots
      y_axis_theme  # Apply the conditional y-axis theme
    
    # Store the plot in the list
    plots_list[[idx_]] <- p
  }
  
  # Arrange the plots side by side, ensuring alignment and consistent size
  combined_plot <- plot_grid(plotlist = plots_list, ncol = 3, align = 'hv')
  
  # Add the global legend to the right of the combined plot
  final_plot <- plot_grid(combined_plot, global_legend, rel_widths = c(4, 0.6))
  
  print(final_plot)
  
  # Save the combined plot
  ggsave(paste0(path_to_plots, 'fr_index_dry_fits/fresh/agu/', sl, "_combined.png"),
         final_plot, width = 14, height = 7, dpi = 300)
}

##############################################################################
#               Plot index range vs fr bars
################
################
df_to_plot_a_soil <- df_to_plot %>% dplyr::filter(soil == "Athena") 

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

# For clarity, define short variable names
df <- df_to_plot_a_soil
df <- df %>%
  rename(
    FR = Fraction_Residue_Cover
  )
thresholds <- c(0, 0.15, 0.30, 0.75, 1)

# 3a. Nest data by (crop, index_name)
df_nested <- df %>%
  group_by(crop, index_name) %>%
  nest()

# 3b. Define a function to fit the linear model and return slope & intercept
fit_linear_model <- function(data) {
  # Fit FR ~ index
  m <- lm(FR ~ index, data = data)
  coefs <- coef(m)  # intercept = coefs[1], slope = coefs[2]
  
  tibble(
    intercept = coefs[1],
    slope     = coefs[2]
  )
}

# 3c. Apply the model fitting to each nested group
df_models <- df_nested %>%
  mutate(model_info = map(data, fit_linear_model)) %>%
  select(-data) %>%
  unnest(model_info)

# 4a. Create a helper function to compute boundary index for a single threshold
compute_boundary <- function(intercept, slope, threshold) {
  # If slope == 0, the model is degenerate. For safety, handle that edge case:
  if(abs(slope) < 1e-12) return(NA_real_)
  
  (threshold - intercept) / slope
}

# 4b. Expand df_models with all thresholds
df_boundaries <- df_models %>%
  crossing(threshold = thresholds) %>%
  rowwise() %>%
  mutate(
    boundary_index = compute_boundary(intercept, slope, threshold)
  ) %>%
  ungroup()


df_boundaries
# # This data frame has columns: crop, index_name, intercept, slope, threshold, boundary_index
# write.csv(df_boundaries, file = paste0(path_to_plots, "df_index_boundaries.csv"), row.names = FALSE)



df <- df_boundaries
df$boundary_index <- round(df$boundary_index, 3)
index_names <- c("NDTI", "CAI", "SINDRI")

# for(idx in index_names) {
#   
#   # 1) Subset and arrange
#   df_sub <- df %>%
#     filter(index_name == idx) %>%
#     arrange(crop, threshold)
#   
#   # 2) Build the base intervals
#   df_intervals <- df_sub %>%
#     group_by(crop) %>%
#     arrange(threshold, .by_group = TRUE) %>%
#     mutate(
#       xmin = boundary_index,
#       xmax = lead(boundary_index),
#       ymin = threshold,
#       ymax = lead(threshold)
#     ) %>%
#     filter(!is.na(xmax), !is.na(ymax)) %>%
#     ungroup()
#   
#   # 3) Sub-divide overlapping fraction bands
#   #    We group by (index_name + the fraction-band), i.e. (ymin, ymax).
#   #    Then within each group we count how many crops (rows) are in that band,
#   #    and assign each crop a sub_ymin..sub_ymax so they do NOT overlap.
#   df_intervals_sub <- df_intervals %>%
#     # create a label or ID for the fraction band
#     mutate(fr_band = paste0(ymin, "-", ymax)) %>%
#     group_by(index_name, fr_band) %>%
#     mutate(
#       # number of crops in this band
#       band_count = n(),
#       # index of this crop in that band
#       band_rank  = row_number(),
#       # sub-divide [ymin, ymax] into 'band_count' slices
#       sub_ymin = ymin + (band_rank - 1) * (ymax - ymin) / band_count,
#       sub_ymax = ymin + band_rank       * (ymax - ymin) / band_count
#     ) %>%
#     ungroup()
#   
#   # 4) Plot, but use sub_ymin/sub_ymax instead of ymin/ymax
#   p <- ggplot(df_intervals_sub, 
#               aes(xmin = xmin, xmax = xmax, 
#                   ymin = sub_ymin, ymax = sub_ymax, 
#                   fill = crop)) +
#     geom_rect(alpha = 0.4, color = "black") +
#     scale_y_continuous(
#       breaks = c(0, 0.15, 0.3, 0.75, 1),
#       limits = c(0, 1)
#     ) +
#     # Add custom x-axis breaks.  
#     # custom_colors <- c("Canola" = "#15616d", "Garbanzo Beans" = "#fcca46", "Peas" = "#ff7d00", 
#     # "Wheat" = "#78290f", "Wheat Pritchett" = "#a1c181") 
#     scale_x_continuous(
#       breaks = sort(unique(na.omit(c(df_intervals_sub$xmin, df_intervals_sub$xmax)))),
#       limits = range(na.omit(c(df_intervals_sub$xmin, df_intervals_sub$xmax)))
#     ) +
#     # Manually set legend key colors
#     scale_fill_manual(
#       values = c("Canola" = "#15616d", "Garbanzo Beans" = "#fcca46",
#                  "Peas" = "#ff7d00", "Wheat Norwest Duet" = "#78290f")
#       ) +
#     labs(
#       # title = paste("Index:", idx),
#       x = paste0(idx),
#       y = expression("Fractional residue cover (" * f[r] * ")")
#     ) +
#     theme_minimal() +
#     theme(
#       # panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
#       panel.grid.minor.x = element_blank(),   # Remove minor vertical grid lines
#       axis.text.x = element_blank(),
#       panel.border = element_blank(),
#       axis.line = element_line(color = "black")
#     )
#   # 5) Add text labels for left (xmin) and right (xmax) edges
#   #    We'll place them vertically in the middle of the rectangle,
#   #    and nudge them slightly inside so they’re not on the exact edge.
#   
#   # Left edge label:
#   p <- p +
#     geom_text(
#       data = df_intervals_sub,
#       aes(
#         x = xmin - 0.0004,  # small nudge so the text is just inside the rectangle
#         y = (sub_ymin + sub_ymax) / 2,
#         label = round(xmin, 4)  # or use sprintf to format
#       ),
#       inherit.aes = FALSE,  # so it doesn't try to use 'xmin'/'ymin' in the main aes()
#       size = 3,
#       hjust = 1  # anchor text at the left (inside)
#     )
#   
#   # Right edge label:
#   p <- p +
#     geom_text(
#       data = df_intervals_sub,
#       aes(
#         x = xmax + 0.0004,  # small nudge inside
#         y = (sub_ymin + sub_ymax) / 2,
#         label = round(xmax, 4)
#       ),
#       inherit.aes = FALSE,
#       size = 3,
#       hjust = 0  # anchor text at the right (inside)
#     )
#   p <- p +
#     # Add horizontal lines at specified thresholds
#     geom_hline(yintercept = c(0.15, 0.3, 0.75, 1), color = "black", linetype = "dashed", size = 0.8)
#   
#   
#   print(p)
# }
# 
# 





# library(dplyr)
# library(ggplot2)
# library(cowplot)
# library(ggpubr)
# 
# 
# 
# df <- df %>%
#   mutate(crop = recode(crop,
#                        "Wheat Norwest Duet" = "Wheat"))
# # Initialize an empty list to store individual plots
# plot_list <- list()
# 
# # Loop through each index_name and create individual plots
# for (idx in index_names) {
#   
#   # Subset, arrange, and build intervals as before
#   df_sub <- df %>%
#     filter(index_name == idx) %>%
#     arrange(crop, threshold)
#   
#   df_intervals <- df_sub %>%
#     group_by(crop) %>%
#     arrange(threshold, .by_group = TRUE) %>%
#     mutate(
#       xmin = boundary_index,
#       xmax = lead(boundary_index),
#       ymin = threshold,
#       ymax = lead(threshold)
#     ) %>%
#     filter(!is.na(xmax), !is.na(ymax)) %>%
#     ungroup()
#   
#   df_intervals_sub <- df_intervals %>%
#     mutate(fr_band = paste0(ymin, "-", ymax)) %>%
#     group_by(index_name, fr_band) %>%
#     mutate(
#       band_count = n(),
#       band_rank  = row_number(),
#       sub_ymin = ymin + (band_rank - 1) * (ymax - ymin) / band_count,
#       sub_ymax = ymin + band_rank       * (ymax - ymin) / band_count
#     ) %>%
#     ungroup()
#   
#   df_intervals_sub <- df_intervals_sub %>%
#     mutate(
#       label_xmin = xmin - 0.03 * (xmax - xmin),   # 3% of bar width left
#       label_xmax = xmax + 0.03 * (xmax - xmin)    # 3% of bar width right
#     )
#   # # Calculate the range of the x-axis for relative positioning
#   # x_range <- range(na.omit(c(df_intervals_sub$xmin, df_intervals_sub$xmax)))
#   # x_offset <- 0.01 * diff(x_range)  # 1% of the x-axis width as offset
#   # 
#   # Create the plot for the current index_name
#   p <- ggplot(df_intervals_sub, 
#               aes(xmin = xmin, xmax = xmax, 
#                   ymin = sub_ymin, ymax = sub_ymax, 
#                   fill = crop)) +
#     geom_rect(alpha = 0.4, color = "black") +
#     geom_text(
#       aes(
#         x = label_xmin,
#         y = (sub_ymin + sub_ymax) / 2,
#         label = round(xmin, 4)
#       ),
#       size = 4, 
#       hjust = 1
#     ) +
#     geom_text(
#       aes(
#         x = label_xmax,
#         y = (sub_ymin + sub_ymax) / 2,
#         label = round(xmax, 4)
#       ),
#       size = 4,
#       hjust = 0
#     ) +
#     geom_hline(yintercept = c(0.15, 0.3, 0.75, 1), color = "black", linetype = "dashed", size = 0.8) +
#     scale_y_continuous(
#       breaks = c(0, 0.15, 0.3, 0.75, 1),
#       limits = c(0, 1)
#     ) +
#     scale_x_continuous(
#       breaks = sort(unique(na.omit(c(df_intervals_sub$xmin, df_intervals_sub$xmax)))),
#       limits = range(na.omit(c(df_intervals_sub$xmin, df_intervals_sub$xmax)))
#     ) +
#     scale_fill_manual(
#       values = c("Canola" = "#15616d", "Garbanzo Beans" = "#fcca46",
#                  "Peas" = "#ff7d00", "Wheat" = "#78290f")
#     ) +
#     labs(
#       x = idx,
#       y = expression("Fractional residue cover (" * f[r] * ")"),
#       fill = ""
#     ) +
#     theme_minimal() +
#     theme(
#       panel.grid.minor.x = element_blank(),
#       axis.text.x = element_blank(),
#       panel.border = element_blank(),
#       axis.line = element_line(color = "black"),
#       legend.position = "bottom",  # Keep legend position for extraction
#       
#       
#       # Manual text size control
#       plot.title = element_text(size = 12, face = "bold"),
#       axis.title.x = element_text(size = 12),
#       axis.title.y = element_text(size = 12),
#       axis.text = element_text(size = 12),
#       legend.title = element_text(size = 12),
#       legend.text = element_text(size = 12),
#       strip.text = element_text(size = 12)  # For facet strip text if used
#       
#     )
#   
#   # Add the plot to the list
#   plot_list[[idx]] <- p
# }
# 
# # Use ggpubr::ggarrange() to arrange the plots and keep a single shared legend
# final_plot <- ggarrange(
#   plotlist = plot_list,
#   nrow = 1,
#   common.legend = TRUE,    # Ensure a single shared legend
#   legend = "bottom"        # Place the legend at the bottom
# )
# 
# # Print the final combined plot
# print(final_plot)
# 
# # Save the combined plot
# ggsave(paste0(path_to_plots, 'index_ranges/fresh/index_range_fresh.png'),
#        final_plot, width = 15, height = 6, dpi = 300)



library(dplyr)
library(ggplot2)
library(tidyr)

# 1) Recode & reorder
df2 <- df %>%
  mutate(
    crop       = recode(crop, "Wheat Norwest Duet" = "Wheat"),
    index_name = factor(index_name, levels = c("NDTI", "CAI", "SINDRI"))
  )

# 2) Build the intervals
df_intervals <- df2 %>%
  group_by(index_name, crop) %>%
  arrange(threshold, .by_group = TRUE) %>%
  mutate(
    xmin    = boundary_index,
    xmax    = lead(boundary_index),
    fr_band = paste0(threshold, "-", lead(threshold))
  ) %>%
  filter(!is.na(xmax), !is.na(fr_band)) %>%
  ungroup() %>%
  mutate(fr_band = factor(fr_band,
                          levels = c("0-0.15", "0.15-0.3", "0.3-0.75", "0.75-1")))

# 3) Define per-index offset percentages & direction
offset_lookup <- tibble(
  index_name = factor(c("NDTI", "CAI", "SINDRI"),
                      levels = c("NDTI", "CAI", "SINDRI")),
  offset_pct  = c(0.03, 0.03, 0.03),
  direction   = c(+1,    +1,    +1)
)

# 4) Build one label per unique boundary & compute its final x-position
df_labels <- df_intervals %>%
  select(index_name, crop, xmin, xmax) %>%
  pivot_longer(cols = c(xmin, xmax), values_to = "boundary_index") %>%
  distinct(index_name, crop, boundary_index) %>%
  left_join(offset_lookup, by = "index_name") %>%
  group_by(index_name, crop) %>%
  mutate(
    span   = max(boundary_index) - min(boundary_index),
    offset = span * offset_pct * direction,
    x_plot = boundary_index + offset,
    lab    = round(boundary_index, 3)
  ) %>%
  ungroup()

# 5) Identify “uncertain” spans (where ≥2 crops disagree)
uncertain <- df_intervals %>%
  group_by(index_name) %>%
  summarize(boundaries = list(sort(unique(c(xmin, xmax))))) %>%
  unnest(boundaries) %>%
  group_by(index_name) %>%
  mutate(
    start = boundaries,
    end   = lead(boundaries)
  ) %>%
  filter(!is.na(end)) %>%
  rowwise() %>%
  mutate(
    this_idx = index_name,
    n_bands  = df_intervals %>%
      filter(index_name == this_idx,
             xmin <= start, xmax >= end) %>%
      pull(fr_band) %>% n_distinct()
  ) %>%
  ungroup() %>%
  filter(n_bands > 1) %>%
  select(index_name, start, end)

# 6) Plot with horizontal “tick” segments centered on the x-axis
p <- ggplot() +
  # red horizontal line for each uncertain span
  geom_segment(
    data        = uncertain,
    inherit.aes = FALSE,
    aes(x = start, xend = end, group = index_name),
    y    = 0.4,   # adjusted to center on x-axis
    yend = 0.4,
    color = "red",
    size  = 10
  ) +
  # FR-category tiles
  geom_tile(
    data = df_intervals,
    aes(
      x      = (xmin + xmax) / 2,
      y      = crop,
      width  = xmax - xmin,
      height = 0.8,
      fill   = fr_band
    ),
    color = "black"
  ) +
  # Boundary labels
  geom_text(
    data        = df_labels,
    aes(x = x_plot, y = crop, label = lab),
    inherit.aes = FALSE,
    angle       = 90,
    hjust       = 0.5,
    vjust       = 0.5,
    size        = 6
  ) +
  facet_wrap(~ index_name, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = c(
    "0-0.15"   = "#dd6e42",
    "0.15-0.3" = "#e8dab2",
    "0.3-0.75" = "#4f6d7a",
    "0.75-1"   = "#c0d6df"
  )) +
  labs(
    x    = "Index value",
    y    = "",
    fill = expression("" * f[r] * " range")
  ) +
  coord_cartesian(clip = "off") +  # prevents clipping of red lines
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.line.x        = element_line(color = "black"),
    axis.ticks.x       = element_line(color = "black"),
    axis.text.x        = element_text(color = "black", angle = 45, hjust = 1),
    panel.border       = element_rect(color = "black", fill = NA, size = 0.5),
    strip.text         = element_text(face = "bold"),
    axis.text.y        = element_text(color = "black"),
    axis.ticks.y       = element_line(color = "black")
  )

print(p)

ggsave(paste0(path_to_plots, 'index_ranges/fresh/index_range_fresh.png'),
       p, width = 15, height = 6, dpi = 300)

write.csv(uncertain, file = paste0(path_to_plots, "uncertain_fresh.csv"), row.names = FALSE)

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

###########################
# Plot Fresh vs weathered for canola and wheat
###########################

crops <- c("Canola", "Weathered Canola", "Wheat Norwest Duet", "Weathered Wheat")
df_to_plot <- dry_df %>% dplyr::filter(crop %in% crops)

# Rename specific crop values in the 'crop' column of df_to_plot
df_to_plot <- df_to_plot %>%
  mutate(crop = recode(crop,
                       "Canola" = "Fresh canola",
                       "Weathered Canola" = "Weathered canola",
                       "Wheat Norwest Duet" = "Fresh wheat",
                       "Weathered Wheat" = "Weathered wheat"))


# Specify the desired order of crops for the legend
desired_order <- c("Fresh canola", "Weathered canola", "Fresh wheat", "Weathered wheat")

# Update df_to_plot to set the order of the factor levels for 'crop'
df_to_plot <- df_to_plot %>%
  mutate(crop = factor(crop, levels = desired_order))

custom_colors <- c("Fresh canola" = "#fe7f2d", "Weathered canola" = "#fe7f2d",
                   "Fresh wheat" = "#619b8a", "Weathered wheat" = "#619b8a"
)  
# Define the shapes: default shapes for normal crops, different shapes for weathered crops
custom_shapes <- c("Fresh canola" = 16, "Weathered canola" = 17, 
                   "Fresh wheat" = 16, "Weathered wheat" = 17)


# to_analyse <- df_to_plot %>% dplyr::filter(soil == "Athena")
# 
# write.csv(to_analyse, file = paste0(path_to_plots, "age_scatter.csv"), row.names = FALSE)

# 
# for (sl in unique(df_to_plot$soil)) {
#   for (idx_ in unique(df_to_plot$index_name)) {
#     filtered <- df_to_plot %>%
#       filter(soil == sl, index_name == idx_)
#     
#     # Calculate slopes for each crop and rename slope to avoid conflicts
#     slopes_intercepts <- filtered %>%
#       group_by(crop) %>%
#       summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
#                 intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
#       ungroup()
#     
#     # Modify the crop labels to include the slope
#     filtered <- filtered %>%
#       left_join(slopes_intercepts, by = "crop") %>%
#       mutate(crop_label = paste0(
#         crop, " (", round(slope_value, 2), ",", round(intercept_value, 2) ,")"
#       ))
#     
#     # Dynamically create the labels for the legend
#     local_crop_labels <- filtered %>%
#       distinct(crop, crop_label) %>%
#       pull(crop_label, crop)
#     
#     p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(crop), shape = as.factor(crop))) +
#       geom_point(size = 4) + # Adds points to the plot
#       # geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
#       facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
#       scale_color_manual(name = "Crop residue (slope)", values = custom_colors,
#                          labels = local_crop_labels) +
#       scale_shape_manual(name = "Crop residue (slope)", values = custom_shapes, labels = local_crop_labels) +
#       labs(x = idx_, y = "Fraction Residue Cover") + # Labels for axes
#       theme_minimal() + # Minimal theme for cleaner look
#       theme(legend.position = "right",
#             legend.title = element_text(size = base_size), # Legend title larger than base size
#             legend.text = element_text(size = base_size * 0.8), # Legend text at base size
#             axis.title = element_text(size = base_size), # Axis titles larger than base size
#             axis.text = element_text(size = base_size * 0.8, color = "black"), # Axis text at base size
#             panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
#             plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
#             panel.grid = element_blank(),
#             axis.ticks = element_line(color = "black"),
#             axis.line = element_line(color = "black"),
#             legend.key.size = unit(0.5, "cm"),
#             plot.title = element_blank(),
#             strip.text = element_blank()) +
#       scale_y_continuous(limits = c(0, 1))
#     
#     # Print the plot
#     print(p)
#     ggsave(paste0(path_to_plots, 'fr_index_dry_fits/by_age/', sl, "_", filtered$index_name[1], '.png'), 
#            p, width = 7, height = 3.5, dpi = 300)
#   }
# }



################################################
################################################
################ Put them in a facet
################
global_crop_legend <- ggplot(df_to_plot,
                             aes(x = 1, y = 1, color = as.factor(crop), shape = as.factor(crop))) +
  geom_point(size = 3) +
  scale_color_manual(name = "Crop", values = custom_colors, labels = levels(as.factor(df_to_plot$crop))) +
  scale_shape_manual(name = "Crop", values = custom_shapes, labels = levels(as.factor(df_to_plot$crop))) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Place the legend on the right
    legend.title = element_text(size = base_size * 1, color="black"),
    legend.text = element_text(size = base_size * 1, margin = margin(r = 10), color="black"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Extract the legend from the global_crop_legend
global_legend <- cowplot::get_legend(global_crop_legend)


for (sl in unique(df_to_plot$soil)) {
  plots_list <- list()
  extracted_legend <- NULL
  
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(soil == sl, index_name == idx_)
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes_intercepts <- filtered %>%
      group_by(crop) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
                intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes_intercepts, by = "crop") %>%
      mutate(crop_label = paste0(
        round(slope_value, 2), ",", round(intercept_value, 2)
      ))
    
    # Dynamically create the labels for the legend
    local_crop_labels <- filtered %>%
      distinct(crop, crop_label) %>%
      pull(crop_label, crop)
    
    # # Apply consistent theme and conditional y-axis title
    # y_axis_theme <- if (idx_ == "NDTI") theme() else theme(axis.title.y = element_blank())
    
    # Apply consistent theme and conditional y-axis settings
    y_axis_theme <- if (idx_ == unique(df_to_plot$index_name)[1]) {
      theme()  # Keep y-axis elements for the leftmost plot
    } else {
      theme(
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.line.y = element_blank()    # Remove y-axis line
      )
    }
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(crop), shape = as.factor(crop))) +
      geom_point(size = 3.5) + # Adds points to the plot
      # geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      # facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
      scale_color_manual(name = "slope, intercept", values = custom_colors, labels = local_crop_labels) +
      scale_shape_manual(name = "slope, intercept", values = custom_shapes, labels = local_crop_labels) +
      labs(x = idx_, y = "Fraction Residue Cover") + # Labels for axes
      theme_minimal() + # Minimal theme for cleaner look
      theme(legend.position = c(0.33, -0.01),
            legend.justification = c(0, 0),
            legend.key.size = unit(0.35, "cm"),
            legend.title = element_text(size = base_size * 1.1, margin = margin(b = 0)),
            legend.text = element_text(size = base_size * 1.1, margin = margin(b = 0)),
            axis.title = element_text(size = base_size * 1.1),
            axis.text = element_text(size = base_size * 1.1, color = "black"),
            panel.background = element_rect(fill = "white", colour = "white"),
            plot.background = element_rect(fill = "white", colour = "white"),
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            plot.title = element_blank(),
            strip.text = element_blank(),
            axis.text.x = element_text(size = base_size * 1.2, color = "black"),  # Smaller x-axis tick labels
            axis.text.y = element_text(size = base_size * 1.2, color = "black"),  # Smaller y-axis tick labels
            plot.margin = unit(c(0.03, 0.03, 0.03, 0.03), "cm")) +  # Ensure equal margins
      coord_fixed(ratio = diff(y_limits) / diff(x_limits)) +  # Ensure square plots
      y_axis_theme  # Apply the conditional y-axis theme
    # Store the plot in the list
    plots_list[[idx_]] <- p
  }
  
  # Arrange the plots side by side, ensuring alignment and consistent size
  combined_plot <- plot_grid(plotlist = plots_list, ncol = 3, align = 'hv')
  
  # Add the global legend to the right of the combined plot
  final_plot <- plot_grid(combined_plot, global_legend, rel_widths = c(4, 0.6))
  
  print(final_plot)
  
  ggsave(paste0(path_to_plots, 'fr_index_dry_fits/by_age/', sl, "_", "combined", '.png'), 
         final_plot, width = 14, height = 7, dpi = 300)
}


################
# For AGU poster
################
################

custom_colors <- c("Fresh canola" = "#15616d", "Weathered canola" = "#15616d",
                   "Fresh wheat" = "#78290f", "Weathered wheat" = "#78290f"
)  

global_crop_legend <- ggplot(df_to_plot,
                             aes(x = 1, y = 1, color = as.factor(crop), shape = as.factor(crop))) +
  geom_point(size = 3) +
  scale_color_manual(name = "Crop", values = custom_colors, labels = levels(as.factor(df_to_plot$crop))) +
  scale_shape_manual(name = "Crop", values = custom_shapes, labels = levels(as.factor(df_to_plot$crop))) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Place the legend on the right
    legend.title = element_text(size = base_size * 1, color="black"),
    legend.text = element_text(size = base_size * 1, margin = margin(r = 10), color="black"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Extract the legend from the global_crop_legend
global_legend <- cowplot::get_legend(global_crop_legend)


for (sl in unique(df_to_plot$soil)) {
  plots_list <- list()
  extracted_legend <- NULL
  
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(soil == sl, index_name == idx_)
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes_intercepts <- filtered %>%
      group_by(crop) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
                intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes_intercepts, by = "crop") %>%
      mutate(crop_label = paste0(
        round(slope_value, 2), ",", round(intercept_value, 2)
      ))
    
    # Dynamically create the labels for the legend
    local_crop_labels <- filtered %>%
      distinct(crop, crop_label) %>%
      pull(crop_label, crop)
    
    # # Apply consistent theme and conditional y-axis title
    # y_axis_theme <- if (idx_ == "NDTI") theme() else theme(axis.title.y = element_blank())
    
    # Apply consistent theme and conditional y-axis settings
    y_axis_theme <- if (idx_ == unique(df_to_plot$index_name)[1]) {
      theme()  # Keep y-axis elements for the leftmost plot
    } else {
      theme(
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.line.y = element_blank()    # Remove y-axis line
      )
    }
    
    x_breaks <- switch(idx_,
                       "NDTI"   = c(-0.1, 0.0, 0.1),
                       "CAI"    = c(0, 2.5, 5, 7.5, 10),
                       "SINDRI" = c(0.000, 0.025, 0.050, 0.075))
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(crop), shape = as.factor(crop))) +
      geom_point(size = 3.5) +
      scale_color_manual(name = "slope, intercept", values = custom_colors, labels = local_crop_labels) +
      scale_shape_manual(name = "slope, intercept", values = custom_shapes, labels = local_crop_labels) +
      scale_x_continuous(breaks = x_breaks) +  # <-- Set x-axis ticks manually here
      labs(x = idx_, y = "Fraction Residue Cover") +
      theme_minimal() +
      theme(legend.position = c(0.33, -0.01),
            legend.justification = c(0, 0),
            legend.key.size = unit(0.35, "cm"),
            legend.title = element_text(size = base_size * 1.1, margin = margin(b = 0), color = "black"),
            legend.text = element_text(size = base_size * 1.1, margin = margin(b = 0), color="black"),
            axis.title = element_text(size = base_size * 1.1, color = "black"),
            axis.text = element_text(size = base_size * 1.1, color = "black"),
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            plot.title = element_blank(),
            strip.text = element_blank(),
            axis.text.x = element_text(size = base_size * 1.2, color = "black"),
            axis.text.y = element_text(size = base_size * 1.2, color = "black"),
            plot.margin = unit(c(0.03, 0.03, 0.03, 0.03), "cm")) +
      coord_fixed(ratio = diff(y_limits) / diff(x_limits)) +
      y_axis_theme
    
    # Store the plot in the list
    plots_list[[idx_]] <- p
  }
  
  # Arrange the plots side by side, ensuring alignment and consistent size
  combined_plot <- plot_grid(plotlist = plots_list, ncol = 3, align = 'hv')
  
  # Add the global legend to the right of the combined plot
  final_plot <- plot_grid(combined_plot, global_legend, rel_widths = c(4, 0.6))
  
  print(final_plot)
  
  ggsave(paste0(path_to_plots, 'fr_index_dry_fits/by_age/agu/', sl, "_", "combined", '.png'), 
         final_plot, width = 14, height = 7, dpi = 300)
}



##############################################################################
#               Plot index range vs fr bars
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

df_to_plot_a_soil <- df_to_plot %>% dplyr::filter(soil == "Athena") 

df <- df_to_plot_a_soil

df <- df %>%
  rename(
    FR = Fraction_Residue_Cover
  )
thresholds <- c(0, 0.15, 0.30, 0.75, 1)

# 3a. Nest data by (crop, index_name)
df_nested <- df %>%
  group_by(crop, index_name) %>%
  nest()

# 3b. Define a function to fit the linear model and return slope & intercept
fit_linear_model <- function(data) {
  # Fit FR ~ index
  m <- lm(FR ~ index, data = data)
  coefs <- coef(m)  # intercept = coefs[1], slope = coefs[2]
  
  tibble(
    intercept = coefs[1],
    slope     = coefs[2]
  )
}

# 3c. Apply the model fitting to each nested group
df_models <- df_nested %>%
  mutate(model_info = map(data, fit_linear_model)) %>%
  select(-data) %>%
  unnest(model_info)

# 4a. Create a helper function to compute boundary index for a single threshold
compute_boundary <- function(intercept, slope, threshold) {
  # If slope == 0, the model is degenerate. For safety, handle that edge case:
  if(abs(slope) < 1e-12) return(NA_real_)
  
  (threshold - intercept) / slope
}

# 4b. Expand df_models with all thresholds
df_boundaries <- df_models %>%
  crossing(threshold = thresholds) %>%
  rowwise() %>%
  mutate(
    boundary_index = compute_boundary(intercept, slope, threshold)
  ) %>%
  ungroup()

df_boundaries
# # This data frame has columns: crop, index_name, intercept, slope, threshold, boundary_index
# write.csv(df_boundaries, file = paste0(path_to_plots, "df_index_boundaries.csv"), row.names = FALSE)



df <- df_boundaries
df$boundary_index <- round(df$boundary_index, 3)
index_names <- c("NDTI", "CAI", "SINDRI")






library(dplyr)
library(ggplot2)
library(cowplot)
library(ggpubr)

df <- df %>%
  mutate(crop = recode(crop,
                       "Canola" = "Fresh canola",
                       "Weathered Canola" = "Weathered canola",
                       "Wheat Norwest Duet" = "Fresh wheat",
                       "Weathered Wheat" = "Weathered wheat"))

# Initialize an empty list to store individual plots
plot_list <- list()

# Loop through each index_name and create individual plots
for (idx in index_names) {
  
  # Subset, arrange, and build intervals as before
  df_sub <- df %>%
    filter(index_name == idx) %>%
    arrange(crop, threshold)
  
  df_intervals <- df_sub %>%
    group_by(crop) %>%
    arrange(threshold, .by_group = TRUE) %>%
    mutate(
      xmin = boundary_index,
      xmax = lead(boundary_index),
      ymin = threshold,
      ymax = lead(threshold)
    ) %>%
    filter(!is.na(xmax), !is.na(ymax)) %>%
    ungroup()
  
  df_intervals_sub <- df_intervals %>%
    mutate(fr_band = paste0(ymin, "-", ymax)) %>%
    group_by(index_name, fr_band) %>%
    mutate(
      band_count = n(),
      band_rank  = row_number(),
      sub_ymin = ymin + (band_rank - 1) * (ymax - ymin) / band_count,
      sub_ymax = ymin + band_rank       * (ymax - ymin) / band_count
    ) %>%
    ungroup()
  
  df_intervals_sub <- df_intervals_sub %>%
    mutate(
      label_xmin = xmin - 0.03 * (xmax - xmin),   # 3% of bar width left
      label_xmax = xmax + 0.03 * (xmax - xmin)    # 3% of bar width right
    )
  # # Calculate the range of the x-axis for relative positioning
  # x_range <- range(na.omit(c(df_intervals_sub$xmin, df_intervals_sub$xmax)))
  # x_offset <- 0.01 * diff(x_range)  # 1% of the x-axis width as offset
  # 
  # Create the plot for the current index_name
  p <- ggplot(df_intervals_sub, 
              aes(xmin = xmin, xmax = xmax, 
                  ymin = sub_ymin, ymax = sub_ymax, 
                  fill = crop)) +
    geom_rect(alpha = 0.4, color = "black") +
    geom_text(
      aes(
        x = label_xmin,
        y = (sub_ymin + sub_ymax) / 2,
        label = round(xmin, 4)
      ),
      size = 4, 
      hjust = 1
    ) +
    geom_text(
      aes(
        x = label_xmax,
        y = (sub_ymin + sub_ymax) / 2,
        label = round(xmax, 4)
      ),
      size = 4,
      hjust = 0
    ) + 
    geom_hline(yintercept = c(0.15, 0.3, 0.75, 1), color = "black", linetype = "dashed", size = 0.8) +
    scale_y_continuous(
      breaks = c(0, 0.15, 0.3, 0.75, 1),
      limits = c(0, 1)
    ) + 
    scale_x_continuous(
      breaks = sort(unique(na.omit(c(df_intervals_sub$xmin, df_intervals_sub$xmax)))),
      limits = range(na.omit(c(df_intervals_sub$xmin, df_intervals_sub$xmax)))
    ) + 
    scale_fill_manual(
      values = c("Fresh canola" = "#0e9594", "Weathered canola" = "#f5dfbb",
                 "Fresh wheat" = "#562c2c", "Weathered wheat" = "#f2542d")
    ) + 
    labs(
      x = idx,
      y = expression("Fractional residue cover (" * f[r] * ")"),
      fill = ""
    ) + 
    theme_minimal() +
    theme(
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "bottom",  # Keep legend position for extraction
      # Manual text size control
      plot.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      strip.text = element_text(size = 12)  # For facet strip text if used
    )
  # Add the plot to the list
  plot_list[[idx]] <- p
}

# Use ggpubr::ggarrange() to arrange the plots and keep a single shared legend
final_plot <- ggarrange(
  plotlist = plot_list,
  nrow = 1,
  common.legend = TRUE,    # Ensure a single shared legend
  legend = "bottom"        # Place the legend at the bottom
)

# Print the final combined plot
print(final_plot)

# Save the combined plot
ggsave(paste0(path_to_plots, 'index_ranges/age/index_range_age.png'),
       final_plot, width = 15, height = 6, dpi = 300)



























# install.packages("ggh4x")   # uncomment if you haven’t installed ggh4x yet

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(tibble)
library(ggh4x)

# — assume you already have:
#    df            # your raw data frame
#    path_to_plots  # e.g. "outputs/"

# 1) Build the intervals
df_intervals <- df %>%
  group_by(index_name, crop) %>%
  arrange(threshold, .by_group = TRUE) %>%
  mutate(
    xmin    = boundary_index,
    xmax    = lead(boundary_index),
    fr_band = paste0(threshold, "-", lead(threshold))
  ) %>%
  filter(!is.na(xmax), !is.na(fr_band)) %>%
  ungroup() %>%
  mutate(
    fr_band    = factor(fr_band, levels = c("0-0.15", "0.15-0.3", "0.3-0.75", "0.75-1")),
    index_name = factor(index_name, levels = c("NDTI", "CAI", "SINDRI"))
  )

# 2) Define per-index offset percentages & direction
offset_lookup <- tibble(
  index_name = factor(c("NDTI", "CAI", "SINDRI"),
                      levels = c("NDTI", "CAI", "SINDRI")),
  offset_pct  = c(0.037, 0.037, 0.037),
  direction   = c(+1,     +1,     +1)
)

# 3) Build one label per unique boundary & compute its final x‐position
df_labels <- df_intervals %>%
  select(index_name, crop, xmin, xmax) %>%
  pivot_longer(cols = c(xmin, xmax), names_to = "which", values_to = "boundary_index") %>%
  distinct(index_name, crop, boundary_index) %>%
  left_join(offset_lookup, by = "index_name") %>%
  group_by(index_name, crop) %>%
  mutate(
    span   = max(boundary_index) - min(boundary_index),
    offset = span * offset_pct * direction,
    x_plot = boundary_index + offset,
    lab    = round(boundary_index, 3)
  ) %>%
  ungroup()

# 4) Identify truly “uncertain” spans by explicit band‐comparison
uncertain <- df_intervals %>%
  group_by(index_name) %>%
  do({
    d    <- .
    bnds <- sort(unique(c(d$xmin, d$xmax)))
    tibble(
      start = head(bnds, -1),
      end   = tail(bnds, -1)
    ) %>%
      rowwise() %>%
      mutate(
        bands      = list(d %>% filter(xmin <= start, xmax >= end) %>% pull(fr_band)),
        n_distinct = length(unique(bands))
      ) %>%
      ungroup() %>%
      filter(n_distinct > 1) %>%
      select(start, end)
  }) %>%
  ungroup()

# 5) Plot with per‐facet breaks via ggh4x::facetted_pos_scales()
p <- ggplot() +
  # red “ticks” on the x-axis
  geom_segment(
    data        = uncertain,
    aes(x = start, xend = end),
    y           = 0.4,   # centers on the x-axis line
    yend        = 0.4,
    color       = "red",
    size        = 10,
    inherit.aes = FALSE
  ) +
  # FR‐category tiles
  geom_tile(
    data = df_intervals,
    aes(
      x      = (xmin + xmax) / 2,
      y      = crop,
      width  = xmax - xmin,
      height = 0.8,
      fill   = fr_band
    ),
    color = "black"
  ) +
  # boundary labels
  geom_text(
    data        = df_labels,
    aes(x = x_plot, y = crop, label = lab),
    angle       = 90,
    hjust       = 0.5,
    vjust       = 0.5,
    size        = 5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ index_name, scales = "free_x", nrow = 1) +
  ggh4x::facetted_pos_scales(
    x = list(
      NDTI   = scale_x_continuous(breaks = c(-0.1,    0.0,   0.1)),
      CAI    = scale_x_continuous(breaks = c( 0.0,    2.5,   5.0,  7.5, 10.0)),
      SINDRI = scale_x_continuous(breaks = c( 0.000,  0.025, 0.050, 0.075))
    )
  ) +
  scale_fill_manual(values = c(
    "0-0.15"   = "#dd6e42",
    "0.15-0.3" = "#e8dab2",
    "0.3-0.75" = "#4f6d7a",
    "0.75-1"   = "#c0d6df"
  )) +
  labs(
    x    = "Index value",
    y    = NULL,
    fill = expression(paste(f[r], " category"))
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.line.x        = element_line(color = "black"),
    axis.ticks.x       = element_line(color = "black"),
    axis.text.x        = element_text(color = "black", angle = 45, hjust = 1),
    panel.border       = element_rect(color = "black", fill = NA, size = 0.5),
    strip.text         = element_text(face = "bold"),
    axis.text.y        = element_text(color = "black"),
    axis.ticks.y       = element_line(color = "black")
  )

print(p)

# 6) Save to file
ggsave(
  filename = paste0(path_to_plots, "index_ranges/age/index_range_age.png"),
  plot     = p,
  width    = 15,
  height   = 6,
  dpi      = 300
)

write.csv(uncertain, file = paste0(path_to_plots, "uncertain_age.csv"), row.names = FALSE)

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################



######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################

###########################
# Plot fr ~ index across soils
###########################

dry_df <- df 
df_to_plot <- dry_df
custom_colors <- c("Athena" = "#582f0e", "Bagdad" = "#7f4f24", "Benwy"= "#936639",
                   "Broadax"= "#a68a64", "Endicott"= "#b6ad90", "Lance"= "#c2c5aa",
                   "Mondovi 1" = "#a4ac86", "Mondovi 2" = "#656d4a", "Oxy" = "#414833",
                   "Palouse" = "#333d29", "Ritzville" = "#774936", "Shano" =  "#580c1f")  

for (crp in unique(df_to_plot$crop)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(crop == crp, index_name == idx_)
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes_intercepts <- filtered %>%
      group_by(soil) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
                intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes_intercepts, by = "soil") %>%
      mutate(soil_label = paste0(
        soil, " (", round(slope_value, 2), ",", round(intercept_value, 2) ,")")
      )
    
    # Dynamically create the labels for the legend
    local_soil_labels <- filtered %>%
      distinct(soil, soil_label) %>%
      pull(soil_label, soil)
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(soil))) +
      geom_point(size = 4) + # Adds points to the plot
      # geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
      scale_color_manual(name = "Soil (slope)", values = custom_colors,
                         labels = local_soil_labels) +
      labs(x = idx_, y = "Fraction Residue Cover") + # Labels for axes
      theme_minimal() + # Minimal theme for cleaner look
      theme(legend.position = "right",
            legend.title = element_text(size = base_size), # Legend title larger than base size
            legend.text = element_text(size = base_size * 0.8), # Legend text at base size
            axis.title = element_text(size = base_size), # Axis titles larger than base size
            axis.text = element_text(size = base_size * 0.8, color = "black"), # Axis text at base size
            panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
            plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_blank(),
            strip.text = element_blank()) +
      scale_y_continuous(limits = c(0, 1))
    
    # Print the plot
    print(p)
    ggsave(paste0(path_to_plots, 'fr_index_dry_fits/soil/', crp, "_", filtered$index_name[1], '.png'), 
           p, width = 14, height = 7, dpi = 300)
  }
}



################################################
################################################
################ Put them in a facet
################


for (crp in unique(df_to_plot$crop)) {
  plots_list <- list()
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(crop == crp, index_name == idx_)
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes_intercepts <- filtered %>%
      group_by(soil) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
                intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes_intercepts, by = "soil") %>%
      mutate(soil_label = paste0(
        soil, " (", round(slope_value, 1), ",", round(intercept_value, 1) ,")")
      )
    
    # Dynamically create the labels for the legend
    local_soil_labels <- filtered %>%
      distinct(soil, soil_label) %>%
      pull(soil_label, soil)
    
    # Apply consistent theme and conditional y-axis title
    y_axis_theme <- if (idx_ == "NDTI") theme() else theme(axis.title.y = element_blank())
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(soil))) +
      geom_point(size = 4) + # Adds points to the plot
      # geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      # facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
      # scale_x_continuous(
      #   name = idx_, 
      #   breaks = round(seq(min(filtered$index), max(filtered$index), length.out = 4), 2)  # Create 4 evenly spaced breaks
      # ) +
      scale_color_manual(name = "Soil (slope, intercept)", values = custom_colors,
                         labels = local_soil_labels) +
      # scale_x_continuous(breaks = seq(floor(min(filtered$index)), 
      #                                 ceiling(max(filtered$index)), 
      #                                 by = 2)) +  
      labs(x = idx_, y = "Fraction Residue Cover") + # Labels for axes
      theme_minimal() + # Minimal theme for cleaner look
      theme(
        legend.position = c(1, -0.02),
        legend.justification = c(0, 0),
        legend.key.size = unit(0.01, "cm"),
        legend.title = element_text(size = base_size * 0.6, margin = margin(b = 0)),
        legend.text = element_text(size = base_size * 0.6, margin = margin(b = 0)),
        axis.title = element_text(size = base_size * 0.7),
        axis.text = element_text(size = base_size * 0.8, color = "black"),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        plot.title = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_text(size = base_size * 0.7, color = "black", angle = 45, hjust = 1, vjust = 1),  # Smaller x-axis tick labels
        axis.text.y = element_text(size = base_size * 0.7, color = "black"),  # Smaller y-axis tick labels
        plot.margin = unit(c(0.5, 1.5, 0.5, 0.5), "cm")) +  # Ensure equal margins
      coord_fixed(ratio = diff(y_limits) / diff(x_limits)) +  # Ensure square plots
      y_axis_theme  # Apply the conditional y-axis theme
    # Store the plot in the list
    plots_list[[idx_]] <- p
  }
  
  # Arrange the plots side by side, ensuring alignment and reducing space between them
  combined_plot <- plot_grid(plotlist = plots_list, 
                             ncol = 3, 
                             align = 'hv', 
                             rel_widths = rep(1, length(plots_list)),  # Equal widths
                             rel_heights = rep(1, length(plots_list))) # Equal heights
  
  # Adjust plot margins to ensure legends and titles are not cut off
  combined_plot <- combined_plot + 
    theme(plot.margin = margin(t = 0.5, r = 2, b = 0.5, l = 0.5, unit = "cm"))
  
  # Print the combined plot
  print(combined_plot)
  
  # Save the plot with proper margins and DPI
  ggsave(paste0(path_to_plots, 'fr_index_dry_fits/soil/', crp, "_", "combined", '.png'), 
         combined_plot, width = 12, height = 3.5, dpi = 300, bg = "white")
  
}


################
# For AGU poster
################
################
# Create a global crop legend plot
global_crop_legend <- ggplot(df_to_plot,
                             aes(x = 1, y = 1, color = as.factor(soil))) +
  geom_point(size = 3.5) +
  scale_color_manual(name = "", values = custom_colors) +  # Use the same custom colors
  theme_minimal() +
  theme(
    legend.position = "right",  # Place the legend on the right
    legend.title = element_text(size = base_size * 1, color="black"),
    legend.text = element_text(size = base_size * 1, margin = margin(r = 10), color="black"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Extract the legend from the global_crop_legend
global_legend <- cowplot::get_legend(global_crop_legend)

theme_set(theme_minimal(base_size = 20))

for (crp in unique(df_to_plot$crop)) {
  plots_list <- list()
  extracted_legend <- NULL
  
  for (idx_ in unique(df_to_plot$index_name)) {
    filtered <- df_to_plot %>%
      filter(crop == crp, index_name == idx_)
    
    # Calculate slopes for each crop and rename slope to avoid conflicts
    slopes_intercepts <- filtered %>%
      group_by(soil) %>%
      summarise(slope_value = coef(lm(Fraction_Residue_Cover ~ index))[2],
                intercept_value = coef(lm(Fraction_Residue_Cover ~ index))[1]) %>%
      ungroup()
    
    # Modify the crop labels to include the slope
    filtered <- filtered %>%
      left_join(slopes_intercepts, by = "soil") %>%
      mutate(soil_label = paste0(round(slope_value, 1), ",", round(intercept_value, 1))
      )
    
    # Dynamically create the labels for the legend
    local_soil_labels <- filtered %>%
      distinct(soil, soil_label) %>%
      pull(soil_label, soil)
    
    # # Apply consistent theme and conditional y-axis title
    # y_axis_theme <- if (idx_ == "NDTI") theme() else theme(axis.title.y = element_blank())
    
    # Apply consistent theme and conditional y-axis settings
    y_axis_theme <- if (idx_ == unique(df_to_plot$index_name)[1]) {
      theme()  # Keep y-axis elements for the leftmost plot
    } else {
      theme(
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.line.y = element_blank()    # Remove y-axis line
      )
    }
    
    
    x_breaks <- switch(idx_,
                       "NDTI"   = c(-0.1, 0.0, 0.1),
                       "CAI"    = c(0, 2.5, 5, 7.5, 10),
                       "SINDRI" = c(0.000, 0.025, 0.050, 0.075))
    
    p <- ggplot(filtered, aes(x = index, y = Fraction_Residue_Cover, color = as.factor(soil))) +
      geom_point(size = 4) + # Adds points to the plot
      # geom_smooth(aes(group = 1), method = "lm", color = "red", se = FALSE) +
      # facet_wrap(~index_name, scales = "free") + # Creates a separate plot for each level of index_name
      # scale_x_continuous(
      #   name = idx_, 
      #   breaks = round(seq(min(filtered$index), max(filtered$index), length.out = 4), 2)  # Create 4 evenly spaced breaks
      # ) +
      scale_color_manual(name = "slope, intercept", values = custom_colors,
                         labels = local_soil_labels) +
      # scale_x_continuous(breaks = seq(floor(min(filtered$index)), 
      #                                 ceiling(max(filtered$index)), 
      #                                 by = 2)) +  
      scale_x_continuous(breaks = x_breaks) + 
      labs(x = idx_, y = "Fraction Residue Cover") + # Labels for axes
      theme_minimal() + # Minimal theme for cleaner look
      theme(
        legend.position = c(0.8, 0),
        legend.justification = c(0, 0),
        legend.key.size = unit(0.01, "cm"),
        legend.title = element_text(size = base_size * 1.2, margin = margin(b = 0), color="black"),
        legend.text = element_text(size = base_size * 1.2, margin = margin(b = 0), color = "black"),
        axis.title = element_text(size = base_size * 1.2, color="black"),
        axis.text = element_text(size = base_size * 1.2, color = "black"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        plot.title = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_text(size = base_size * 1.2, color = "black", angle = 45, hjust = 1, vjust = 1),  # Smaller x-axis tick labels
        axis.text.y = element_text(size = base_size * 1.2, color = "black"),  # Smaller y-axis tick labels
        plot.margin = unit(c(0.02, 0.02, 0.02, 0.02), "cm")) +  # Ensure equal margins
      coord_fixed(ratio = diff(y_limits) / diff(x_limits)) +  # Ensure square plots
      y_axis_theme  # Apply the conditional y-axis theme
    # Store the plot in the list
    plots_list[[idx_]] <- p
  }
  
  # Arrange the plots side by side, ensuring alignment and reducing space between them
  combined_plot <- plot_grid(plotlist = plots_list, 
                             ncol = 3, 
                             align = 'hv', 
                             rel_widths = rep(1, length(plots_list)),  # Equal widths
                             rel_heights = rep(1, length(plots_list))) # Equal heights
  
  # Adjust plot margins to ensure legends and titles are not cut off
  combined_plot <- combined_plot + 
    theme(plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"))
  
  # Arrange the plots side by side, ensuring alignment and consistent size
  combined_plot <- plot_grid(plotlist = plots_list, ncol = 3, align = 'hv')
  
  # Add the global legend to the right of the combined plot
  final_plot <- plot_grid(combined_plot, global_legend, rel_widths = c(4, 1))
  
  print(final_plot)
  
  
  # Save the plot with proper margins and DPI
  ggsave(paste0(path_to_plots, 'fr_index_dry_fits/soil/agu/', crp, "_", "combined", '.png'), 
         final_plot, width = 15, height = 6, dpi = 300)
  
}






##############################################################################
#               Plot index range vs fr bars
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

# dry_df <- df %>% dplyr::filter(RWC == 0)
df_to_plot <- dry_df
# custom_colors <- c("Athena" = "#582f0e", "Bagdad" = "#7f4f24", "Benwy"= "#936639",
#                    "Broadax"= "#a68a64", "Endicott"= "#b6ad90", "Lance"= "#c2c5aa",
#                    "Mondovi 1" = "#a4ac86", "Mondovi 2" = "#656d4a", "Oxy" = "#414833",
#                    "Palouse" = "#333d29", "Ritzville" = "#774936", "Shano" =  "#580c1f")  

df_to_plot_a_crop <- df_to_plot %>% dplyr::filter(crop == "Wheat Norwest Duet") 





# For clarity, define short variable names
df <- df_to_plot_a_crop
df <- df %>%
  rename(
    FR = Fraction_Residue_Cover
  )
thresholds <- c(0, 0.15, 0.30, 0.75, 1)

# 3a. Nest data by (crop, index_name)
df_nested <- df %>%
  group_by(soil, index_name) %>%
  nest()

# 3b. Define a function to fit the linear model and return slope & intercept
fit_linear_model <- function(data) {
  # Fit FR ~ index
  m <- lm(FR ~ index, data = data)
  coefs <- coef(m)  # intercept = coefs[1], slope = coefs[2]
  
  tibble(
    intercept = coefs[1],
    slope     = coefs[2]
  )
}

# 3c. Apply the model fitting to each nested group
df_models <- df_nested %>%
  mutate(model_info = map(data, fit_linear_model)) %>%
  select(-data) %>%
  unnest(model_info)

# 4a. Create a helper function to compute boundary index for a single threshold
compute_boundary <- function(intercept, slope, threshold) {
  # If slope == 0, the model is degenerate. For safety, handle that edge case:
  if(abs(slope) < 1e-12) return(NA_real_)
  
  (threshold - intercept) / slope
}

# 4b. Expand df_models with all thresholds
df_boundaries <- df_models %>%
  crossing(threshold = thresholds) %>%
  rowwise() %>%
  mutate(
    boundary_index = compute_boundary(intercept, slope, threshold)
  ) %>%
  ungroup()



df <- df_boundaries
df$boundary_index <- round(df$boundary_index, 3)
index_names <- c("NDTI", "CAI", "SINDRI")


library(ggplot2)

# Initialize an empty list to store individual plots
plot_list <- list()

# Loop through each index_name and create individual plots
for (idx in index_names) {
  
  # Subset, arrange, and build intervals as before
  df_sub <- df %>%
    filter(index_name == idx) %>%
    arrange(soil, threshold)
  
  df_intervals <- df_sub %>%
    group_by(soil) %>%
    arrange(threshold, .by_group = TRUE) %>%
    mutate(
      xmin = boundary_index,
      xmax = lead(boundary_index),
      ymin = threshold,
      ymax = lead(threshold)
    ) %>%
    filter(!is.na(xmax), !is.na(ymax)) %>%
    ungroup()
  
  df_intervals_sub <- df_intervals %>%
    mutate(fr_band = paste0(ymin, "-", ymax)) %>%
    group_by(index_name, fr_band) %>%
    mutate(
      band_count = n(),
      band_rank  = row_number(),
      sub_ymin = ymin + (band_rank - 1) * (ymax - ymin) / band_count,
      sub_ymax = ymin + band_rank       * (ymax - ymin) / band_count
    ) %>%
    ungroup()
  
  df_intervals_sub <- df_intervals_sub %>%
    mutate(
      label_xmin = xmin - 0.03 * (xmax - xmin),   # 3% of bar width left
      label_xmax = xmax + 0.03 * (xmax - xmin)    # 3% of bar width right
    )
  # 
  # Create the plot for the current index_name
  p <- ggplot(df_intervals_sub, 
              aes(xmin = xmin, xmax = xmax, 
                  ymin = sub_ymin, ymax = sub_ymax, 
                  fill = soil)) +
    geom_rect(alpha = 0.4, color = "black") +
    geom_text(
      aes(
        x = label_xmin,
        y = (sub_ymin + sub_ymax) / 2,
        label = round(xmin, 4)
      ),
      size = 3, 
      hjust = 1,
      # position = position_nudge(x = 0.5)
    ) +
    geom_text(
      aes(
        x = label_xmax,
        y = (sub_ymin + sub_ymax) / 2,
        label = round(xmax, 4)
      ),
      size = 3,
      hjust = 0,
      # position = position_nudge(x = 0.5)
    ) +
    geom_hline(yintercept = c(0.15, 0.3, 0.75, 1), color = "black", linetype = "dashed", size = 0.8) +
    # scale_y_continuous(
    #   breaks = c(0, 0.15, 0.3, 0.75, 1),
    #   limits = c(0, 1)
    # ) +
    scale_x_continuous(
      breaks = sort(unique(na.omit(c(df_intervals_sub$xmin, df_intervals_sub$xmax)))),
      limits = range(na.omit(c(df_intervals_sub$xmin, df_intervals_sub$xmax)))
    ) +
    scale_fill_manual(
      values = c("Athena" = "#582f0e", "Bagdad" = "#7f4f24", "Benwy"= "#936639",
                 "Broadax"= "#a68a64", "Endicott"= "#b6ad90", "Lance"= "#c2c5aa",
                 "Mondovi 1" = "#a4ac86", "Mondovi 2" = "#656d4a", "Oxy" = "#414833",
                 "Palouse" = "#333d29", "Ritzville" = "#774936", "Shano" =  "#580c1f")
    ) +
    labs(
      x = idx,
      y = expression("Fractional residue cover (" * f[r] * ")"),
      fill = ""
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "bottom",  # Keep legend position for extraction
      
      
      # Manual text size control
      plot.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      strip.text = element_text(size = 12)  # For facet strip text if used
      
    )
  # Add the plot to the list
  plot_list[[idx]] <- p
}
# Use ggpubr::ggarrange() to arrange the plots and keep a single shared legend
final_plot <- ggarrange(
  plotlist = plot_list,
  nrow = 1,
  common.legend = TRUE,    # Ensure a single shared legend
  legend = "bottom"        # Place the legend at the bottom
)

# Print the final combined plot
print(final_plot)

# Save the combined plot
ggsave(paste0(path_to_plots, 'index_ranges/soil/index_range_soil.png'),
       final_plot, width = 15, height = 6, dpi = 300)






















library(dplyr)
library(ggplot2)
library(tidyr)


# 2) Build the intervals
df_intervals <- df %>%
  group_by(index_name, soil) %>%
  arrange(threshold, .by_group = TRUE) %>%
  mutate(
    xmin    = boundary_index,
    xmax    = lead(boundary_index),
    fr_band = paste0(threshold, "-", lead(threshold))
  ) %>%
  filter(!is.na(xmax), !is.na(fr_band)) %>%
  ungroup() %>%
  mutate(fr_band = factor(fr_band,
                          levels = c("0-0.15", "0.15-0.3", "0.3-0.75", "0.75-1")))

df_intervals <- df_intervals %>%
  mutate(
    index_name = factor(index_name,
                        levels = c("NDTI", "CAI", "SINDRI"))
  )

# 3) Define per-index offset percentages & direction
#    direction = +1 pushes to the right; -1 pushes to the left
offset_lookup <- tibble(
  index_name = factor(c("NDTI", "CAI", "SINDRI"),
                      levels = c("NDTI", "CAI", "SINDRI")),
  offset_pct  = c(0.037,    0.037,    0.037),   # tweak these
  direction   = c(+1,      +1,      +1)      # NDTI → left, CAI/SINDRI → right
)

# 4) Build one label per unique boundary & compute its final x‐position
df_labels <- df_intervals %>%
  select(index_name, soil, xmin, xmax) %>%
  pivot_longer(cols = c(xmin, xmax), values_to = "boundary_index") %>%
  distinct(index_name, soil, boundary_index) %>%
  left_join(offset_lookup, by = "index_name") %>%
  group_by(index_name, soil) %>%
  mutate(
    span   = max(boundary_index) - min(boundary_index),
    offset = span * offset_pct * direction,
    x_plot = boundary_index + offset,
    lab    = round(boundary_index, 3)
  ) %>%
  ungroup()

# 5) Plot
p <- ggplot(df_intervals,
            aes(x      = (xmin + xmax) / 2,
                y      = soil,
                width  = xmax - xmin,
                height = 0.8,
                fill   = fr_band)) +
  geom_tile(color = "black") +
  geom_text(
    data        = df_labels,
    aes(x = x_plot, y = soil, label = lab),
    inherit.aes = FALSE,
    angle       = 90,
    hjust       = 0.5,
    vjust       = 0.5,
    size        = 2
  ) +
  facet_wrap(~ index_name, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = c(
    "0-0.15"   = "#dd6e42",
    "0.15-0.3" = "#e8dab2",
    "0.3-0.75" = "#4f6d7a",
    "0.75-1"   = "#c0d6df"
  )) +
  labs(
    x    = "Index value",
    y    = "",
    fill = expression(paste(f[r], " category"))
  ) +
  theme_minimal(base_size = 16) +
  theme(
    # remove the light grid under x
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    
    # draw a black line for every panel's x‐axis
    axis.line.x  = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    
    # make the tick labels themselves black
    axis.text.x  = element_text(color = "black", angle = 45, hjust = 1),
    
    # (optional) draw a full border around each panel
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    
    strip.text   = element_text(face = "bold")
  )

print(p)
# Save the combined plot
ggsave(paste0(path_to_plots, 'index_ranges/soil/index_range_soil.png'),
       p, width = 15, height = 6, dpi = 300)





















library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

# 1) Build the intervals
df_intervals <- df %>%
  group_by(index_name, soil) %>%
  arrange(threshold, .by_group = TRUE) %>%
  mutate(
    xmin    = boundary_index,
    xmax    = lead(boundary_index),
    fr_band = paste0(threshold, "-", lead(threshold))
  ) %>%
  filter(!is.na(xmax), !is.na(fr_band)) %>%
  ungroup() %>%
  mutate(
    fr_band    = factor(fr_band,
                        levels = c("0-0.15", "0.15-0.3", "0.3-0.75", "0.75-1")),
    index_name = factor(index_name,
                        levels = c("NDTI", "CAI", "SINDRI"))
  )

# 2) Define per-index offset percentages & direction
offset_lookup <- tibble(
  index_name = factor(c("NDTI", "CAI", "SINDRI"),
                      levels = c("NDTI", "CAI", "SINDRI")),
  offset_pct  = c(0.037, 0.037, 0.037),
  direction   = c(+1,     +1,     +1)
)

# 3) Build one label per unique boundary & compute its final x‐position
df_labels <- df_intervals %>%
  select(index_name, soil, xmin, xmax) %>%
  pivot_longer(cols = c(xmin, xmax), values_to = "boundary_index") %>%
  distinct(index_name, soil, boundary_index) %>%
  left_join(offset_lookup, by = "index_name") %>%
  group_by(index_name, soil) %>%
  mutate(
    span   = max(boundary_index) - min(boundary_index),
    offset = span * offset_pct * direction,
    x_plot = boundary_index + offset,
    lab    = round(boundary_index, 3)
  ) %>%
  ungroup()

# 4) Identify “uncertain” spans by explicit band‐comparison
uncertain <- df_intervals %>%
  group_by(index_name) %>%
  do({
    d    <- .
    bnds <- sort(unique(c(d$xmin, d$xmax)))
    segs <- tibble(
      start = head(bnds, -1),
      end   = tail(bnds, -1)
    )
    segs <- segs %>%
      rowwise() %>%
      mutate(
        bands     = list(d %>% filter(xmin <= start, xmax >= end) %>% pull(fr_band)),
        n_distinct = length(unique(bands))
      ) %>%
      ungroup() %>%
      filter(n_distinct > 1) %>%
      select(start, end)
    segs
  }) %>%
  ungroup()

# 5) Plot with red ticks for uncertain spans
p <- ggplot() +
  # red horizontal tick for each uncertain span
  geom_segment(
    data        = uncertain,
    inherit.aes = FALSE,
    aes(x = start, xend = end, group = index_name),
    y    = -Inf, yend = -Inf,
    color = "red",
    size  = 10
  ) +
  # the FR‐category tiles
  geom_tile(
    data = df_intervals,
    aes(
      x      = (xmin + xmax) / 2,
      y      = soil,
      width  = xmax - xmin,
      height = 0.8,
      fill   = fr_band
    ),
    color = "black"
  ) +
  # the boundary labels
  geom_text(
    data = df_labels,
    aes(x = x_plot, y = soil, label = lab),
    inherit.aes = FALSE,
    angle       = 90,
    hjust       = 0.5,
    vjust       = 0.5,
    size        = 3.5
  ) +
  facet_wrap(~ index_name, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = c(
    "0-0.15"   = "#dd6e42",
    "0.15-0.3" = "#e8dab2",
    "0.3-0.75" = "#4f6d7a",
    "0.75-1"   = "#c0d6df"
  )) +
  labs(
    x    = "Index value",
    y    = "",
    fill = expression(paste(f[r], " category"))
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.line.x        = element_line(color = "black"),
    axis.ticks.x       = element_line(color = "black"),
    axis.text.x        = element_text(color = "black", angle = 45, hjust = 1),
    panel.border       = element_rect(color = "black", fill = NA, size = 0.5),
    strip.text         = element_text(face = "bold"),
    axis.text.y  = element_text(color = "black"),        # make y-labels black
    axis.ticks.y = element_line(color = "black")
  )

print(p)
ggsave(
  paste0(path_to_plots, 'index_ranges/soil/index_range_soil.png'),
  p, width = 15, height = 6, dpi = 300
)





















library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(tibble)
library(ggh4x)

# — assume you already have:
#    df             # your raw data frame
#    path_to_plots  # e.g. "outputs/"

# 1) Build the intervals
df_intervals <- df %>%
  group_by(index_name, soil) %>%
  arrange(threshold, .by_group = TRUE) %>%
  mutate(
    xmin    = boundary_index,
    xmax    = lead(boundary_index),
    fr_band = paste0(threshold, "-", lead(threshold))
  ) %>%
  filter(!is.na(xmax), !is.na(fr_band)) %>%
  ungroup() %>%
  mutate(
    fr_band    = factor(fr_band,
                        levels = c("0-0.15", "0.15-0.3", "0.3-0.75", "0.75-1")),
    index_name = factor(index_name,
                        levels = c("NDTI", "CAI", "SINDRI"))
  )

# 2) Define per-index offset percentages & direction
offset_lookup <- tibble(
  index_name = factor(c("NDTI", "CAI", "SINDRI"),
                      levels = c("NDTI", "CAI", "SINDRI")),
  offset_pct  = c(0.037, 0.037, 0.037),
  direction   = c(+1,     +1,     +1)
)

# 3) Build one label per unique boundary & compute its final x-position
df_labels <- df_intervals %>%
  select(index_name, soil, xmin, xmax) %>%
  pivot_longer(cols = c(xmin, xmax),
               names_to   = "which",
               values_to  = "boundary_index") %>%
  distinct(index_name, soil, boundary_index) %>%
  left_join(offset_lookup, by = "index_name") %>%
  group_by(index_name, soil) %>%
  mutate(
    span   = max(boundary_index) - min(boundary_index),
    offset = span * offset_pct * direction,
    x_plot = boundary_index + offset,
    lab    = round(boundary_index, 3)
  ) %>%
  ungroup()

# 4) Identify “uncertain” spans by explicit band‐comparison
uncertain <- df_intervals %>%
  group_by(index_name) %>%
  do({
    d    <- .
    bnds <- sort(unique(c(d$xmin, d$xmax)))
    tibble(
      start = head(bnds, -1),
      end   = tail(bnds, -1)
    ) %>%
      rowwise() %>%
      mutate(
        bands      = list(d %>% filter(xmin <= start, xmax >= end) %>% pull(fr_band)),
        n_distinct = length(unique(bands))
      ) %>%
      ungroup() %>%
      filter(n_distinct > 1) %>%
      select(start, end)
  }) %>%
  ungroup()

# 5) Plot with red ticks for uncertain spans (centered on x-axis)
#    and per-facet custom x-axis breaks
p <- ggplot() +
  # red horizontal tick for each uncertain span, centered on the x-axis
  geom_segment(
    data        = uncertain,
    aes(x = start, xend = end),
    y           = 0.4,    # center on the x-axis
    yend        = 0.4,
    color       = "red",
    size        = 10,
    inherit.aes = FALSE
  ) +
  # FR-category tiles
  geom_tile(
    data = df_intervals,
    aes(
      x      = (xmin + xmax) / 2,
      y      = soil,
      width  = xmax - xmin,
      height = 0.8,
      fill   = fr_band
    ),
    color = "black"
  ) +
  # Boundary labels
  geom_text(
    data        = df_labels,
    aes(x = x_plot, y = soil, label = lab),
    angle       = 90,
    hjust       = 0.5,
    vjust       = 0.5,
    size        = 2.5,
    inherit.aes = FALSE
  ) +
  # Facet per index_name with free x scale
  facet_wrap(~ index_name, scales = "free_x", nrow = 1) +
  # Custom breaks for each facet
  ggh4x::facetted_pos_scales(
    x = list(
      NDTI   = scale_x_continuous(breaks = c(-0.1,   0.0,   0.1)),
      CAI    = scale_x_continuous(breaks = c( 0.0,   2.5,   5.0,  7.5, 10.0)),
      SINDRI = scale_x_continuous(breaks = c( 0.000, 0.025, 0.050, 0.075))
    )
  ) +
  scale_fill_manual(values = c(
    "0-0.15"   = "#dd6e42",
    "0.15-0.3" = "#e8dab2",
    "0.3-0.75" = "#4f6d7a",
    "0.75-1"   = "#c0d6df"
  )) +
  labs(
    x    = "Index value",
    y    = "",
    fill = expression(paste(f[r], " category"))
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.line.x        = element_line(color = "black"),
    axis.ticks.x       = element_line(color = "black"),
    axis.text.x        = element_text(color = "black", angle = 45, hjust = 1),
    panel.border       = element_rect(color = "black", fill = NA, size = 0.5),
    strip.text         = element_text(face = "bold"),
    axis.text.y        = element_text(color = "black"),
    axis.ticks.y       = element_line(color = "black")
  )

print(p)

ggsave(
  filename = paste0(path_to_plots, 'index_ranges/soil/index_range_soil.png'),
  plot     = p,
  width    = 15,
  height   = 6,
  dpi      = 300
)

write.csv(uncertain, file = paste0(path_to_plots, "uncertain_soil.csv"), row.names = FALSE)

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################


###########################
#       Plot bias by crops
###########################

fit_on_dry_wheat_Athena <- function(data) {
  # Find the minimum RWC in this subset
  min_rwc_data <- data %>%
    filter((crop == "Wheat Norwest Duet") & (RWC == min(RWC)) & soil == "Athena")
  
  # Fit linear model to the data with minimum RWC
  model <- lm(Fraction_Residue_Cover ~ index, data = min_rwc_data)
  
  # Predict Fraction_Residue_Cover for the entire group's data
  predictions <- predict(model, newdata = data)
  
  # Add predictions to the data
  data$Predicted_Fraction_Residue_Cover <- predictions
  
  # Calculate predictions for the minimum RWC data only
  predictions_min_rwc <- predict(model, newdata = min_rwc_data)
  
  # Since all entries with minimum RWC should have the same predicted value, take the first
  min_rwc_predicted_value <- predictions_min_rwc[1]
  
  # Calculate error ratio for all entries in the data
  data$error_ratio <- data$Predicted_Fraction_Residue_Cover / min_rwc_predicted_value
  
  # Add slope and intercept from the model
  data$Slope <- coef(model)[["index"]]
  data$Intercept <- coef(model)[["(Intercept)"]]
  
  return(data)
}


# Perform interpolation using reframe()
df_interpolated <- df %>%
  group_by(index_name, RWC, mix) %>%  # Group by index_name, RWC, and mix
  reframe(
    Fraction_Residue_Cover_interp = seq(min(Fraction_Residue_Cover), max(Fraction_Residue_Cover), length.out = 100),
    index_interp = approx(Fraction_Residue_Cover, index, xout = seq(min(Fraction_Residue_Cover), max(Fraction_Residue_Cover), length.out = 100))$y,
    crop = first(crop),
    soil = first(soil),
    age = first(age)
  )

df_interpolated <- df_interpolated %>% 
  rename(Fraction_Residue_Cover = Fraction_Residue_Cover_interp,
         index = index_interp)


# # Filter for fresh crops
# fresh_df <- df_interpolated %>% dplyr::filter(age == "fresh")

fresh_df_Athena <- df_interpolated %>% 
  dplyr::filter(soil == "Athena")
# Fit on driest wheat on Athena
results <- fresh_df_Athena %>%
  group_by(index_name) %>%
  group_modify(~ fit_on_dry_wheat_Athena(.x))


results_fr <-  results
results_fr$act_fr <- results_fr$Fraction_Residue_Cover
results_fr$pred_fr <- results_fr$Predicted_Fraction_Residue_Cover
results_fr$fr_ab <- abs(results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover)


df_to_plot <- results_fr
df_to_plot <- df_to_plot %>% dplyr::filter(RWC == 0)
# Ensure index_name is a factor with the desired ordering
df_to_plot$index_name <- factor(df_to_plot$index_name, levels = c("NDTI", "CAI", "SINDRI"))

# Define the crops to keep
crops_to_keep <- c("Canola", "Garbanzo Beans", "Peas", "Weathered Canola", 
                   "Weathered Wheat", "Wheat Norwest Duet")

# Filter the dataframe for the specified crops
df_to_plot <- df_to_plot[df_to_plot$crop %in% crops_to_keep, ]

# Rename "Wheat Norwest Duet" to "wheat"
df_to_plot$crop <- ifelse(df_to_plot$crop == "Wheat Norwest Duet", "Wheat", df_to_plot$crop)

# Reorder the levels of 'crop' as desired
df_to_plot$crop <- factor(
  df_to_plot$crop, levels = c(
    "Canola", "Weathered Canola", "Wheat",
    "Weathered Wheat", "Garbanzo Beans",
    "Peas"))


# Get unique index names
index_names <- c("NDTI", "CAI", "SINDRI")

# Initialize an empty list to store the plots
plot_list <- list()

df_to_plot_crop <- df_to_plot
for (i in seq_along(index_names)) {
  idx   <- index_names[i]
  sub_df <- df_to_plot[df_to_plot$index_name == idx, ]
  
  p <- ggplot(sub_df, aes(x = index, y = fr_ab, color = crop)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = c(
        "Canola"           = "#63ccb7", 
        "Weathered Canola" = "#fe7f2d",
        "Wheat"            = "#06293c",
        "Weathered Wheat"  = "#606c38", 
        "Garbanzo Beans"   = "#06728e",
        "Peas"             = "#ff5643"
      )
    ) +
    labs(
      x     = idx,
      y     = "Mean Absolute Bias",
      color = "Crop Types"
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      expand = c(0, 0)
    ) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(color = "gray80"),
      axis.text          = element_text(size = 16),
      axis.title         = element_text(size = 16),
      legend.position    = "bottom",
      legend.title       = element_text(size = 16),
      legend.text        = element_text(size = 16),
      plot.title         = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  
  # strip y-axis on all but the first panel
  if (i != 1) {
    p <- p + theme(
      axis.title.y  = element_blank(),
      axis.text.y   = element_blank(),
      axis.ticks.y  = element_blank()
    )
  }
  
  plot_list[[i]] <- p
}

final_plot <- ggpubr::ggarrange(
  plotlist      = plot_list,
  ncol          = length(index_names),
  common.legend = TRUE,
  legend        = "bottom",
  align         = "v"
)

print(final_plot)

ggsave(
  filename = paste0(path_to_plots, "mab/by_crops.png"),
  plot     = final_plot,
  width    = 15,
  height   = 6,
  dpi      = 300
)




#####################
#####################
# Across soils
#####################
#####################

# Perform interpolation using reframe()
df_interpolated <- df %>%
  group_by(index_name, RWC, mix) %>%  # Group by index_name, RWC, and mix
  reframe(
    Fraction_Residue_Cover_interp = seq(min(Fraction_Residue_Cover), max(Fraction_Residue_Cover), length.out = 100),
    index_interp = approx(Fraction_Residue_Cover, index, xout = seq(min(Fraction_Residue_Cover), max(Fraction_Residue_Cover), length.out = 100))$y,
    crop = first(crop),
    soil = first(soil),
    age = first(age)
  )

df_interpolated <- df_interpolated %>% 
  rename(Fraction_Residue_Cover = Fraction_Residue_Cover_interp,
         index = index_interp)

# Fit on driest wheat on Athena
results <- df_interpolated %>%
  group_by(index_name) %>%
  group_modify(~ fit_on_dry_wheat_Athena(.x))


results_fr <-  results
results_fr$act_fr <- results_fr$Fraction_Residue_Cover
results_fr$pred_fr <- results_fr$Predicted_Fraction_Residue_Cover
results_fr$fr_ab <- abs(results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover)


df_to_plot <- results_fr
df_to_plot <- df_to_plot %>% dplyr::filter(RWC == 0)
# Ensure index_name is a factor with the desired ordering
df_to_plot$index_name <- factor(df_to_plot$index_name, levels = c("NDTI", "CAI", "SINDRI"))

# Define the crops to keep
crops_to_keep <- c("Wheat Norwest Duet")

# Filter the dataframe for the specified crops
df_to_plot <- df_to_plot[df_to_plot$crop %in% crops_to_keep, ]

# Rename "Wheat Norwest Duet" to "wheat"
df_to_plot$crop <- ifelse(df_to_plot$crop == "Wheat Norwest Duet", "Wheat", filtered_df$crop)


# Get unique index names
index_names <- c("NDTI", "CAI", "SINDRI")
# Initialize an empty list to store the plots

df_to_plot_soil <- df_to_plot

library(ggplot2)
library(ggpubr)   # for ggarrange()

plot_list <- list()

for (i in seq_along(index_names)) {
  idx    <- index_names[i]
  sub_df <- df_to_plot[df_to_plot$index_name == idx, ]
  
  p <- ggplot(sub_df, aes(x = index, y = fr_ab, color = soil)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = c(
        "Athena"      = "#582f0e", "Bagdad"   = "#7f4f24",
        "Benwy"       = "#936639", "Broadax"  = "#a68a64",
        "Endicott"    = "#b6ad90", "Lance"    = "#c2c5aa",
        "Mondovi 1"   = "#a4ac86", "Mondovi 2"= "#656d4a", 
        "Oxy"         = "#414833", "Palouse"  = "#333d29",
        "Ritzville"   = "#774936", "Shano"    = "#580c1f"
      )
    ) +
    # fixed y-axis from 0 to 1, ticks at 0.0,0.2,…1.0
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      expand = c(0, 0)
    ) +
    labs(
      x     = idx,
      y     = "Mean Absolute Bias",
      color = "Soil Type"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(color = "gray80"),
      axis.text          = element_text(size = 12),
      axis.title         = element_text(size = 12),
      legend.position    = "bottom",
      legend.title       = element_text(size = 12),
      legend.text        = element_text(size = 12),
      plot.title         = element_text(size = 12, face = "bold", hjust = 0.5)
    )
  
  # strip the y-axis on all but the first panel
  if (i != 1) {
    p <- p + theme(
      axis.title.y  = element_blank(),
      axis.text.y   = element_blank(),
      axis.ticks.y  = element_blank()
    )
  }
  
  plot_list[[i]] <- p
}

# combine with one common legend & shared left y-axis
final_plot <- ggarrange(
  plotlist      = plot_list,
  ncol          = 3,
  common.legend = TRUE,
  legend        = "bottom",
  align         = "v"
)

# display & save
print(final_plot)
ggsave(
  filename = paste0(path_to_plots, 'mab/by_soils.png'),
  plot     = final_plot,
  width    = 15,
  height   = 6,
  dpi      = 300
)










#####################
#####################
# Crops and soils combined MAB
#####################
#####################

# load packages
library(ggplot2)
library(ggpubr)
library(scales)    # for number_format()

# your index order
index_names <- c("NDTI", "CAI", "SINDRI")

# the exact x-axis breaks per index
x_breaks <- list(
  NDTI   = c(-0.2, -0.1,   0.0,  0.1, 0.2),
  CAI    = c( 0.0,   2.5,  5.0,  7.5, 10),
  SINDRI = c( -0.025, 0.000, 0.025, 0.050, 0.075, 0.1)
)

# color scales
crop_cols <- c(
  "Canola"           = "#63ccb7",
  "Weathered Canola" = "#fe7f2d",
  "Wheat"            = "#06293c",
  "Weathered Wheat"  = "#606c38",
  "Garbanzo Beans"   = "#06728e",
  "Peas"             = "#ff5643"
)
soil_cols <- c(
  "Athena"    = "#582f0e", "Bagdad"   = "#7f4f24",
  "Benwy"     = "#936639", "Broadax"  = "#a68a64",
  "Endicott"  = "#b6ad90", "Lance"    = "#c2c5aa",
  "Mondovi 1" = "#a4ac86", "Mondovi 2"= "#656d4a",
  "Oxy"       = "#414833", "Palouse"  = "#333d29",
  "Ritzville" = "#774936", "Shano"    = "#580c1f"
)

# helper to build panels
make_panels <- function(df, aes_colour, colour_vals, legend_title) {
  panels <- list()
  for (i in seq_along(index_names)) {
    idx    <- index_names[i]
    
    # inside the for-loop, after defining `idx`:
    lab_fun <- if (idx == "SINDRI") {
      # three-decimal labels
      scales::label_number(accuracy = 0.001)
    } else {
      # one-decimal labels for NDTI & CAI
      scales::label_number(accuracy = 0.1)
    }
    
    sub_df <- df[df$index_name == idx, ]
    
    p <- ggplot(sub_df, aes_string(x = "index", y = "fr_ab", colour = aes_colour)) +
      geom_line(size = 1) +
      scale_color_manual(values = colour_vals) +
      
      # force axis limits to the full break range,
      # so all ticks (even if data don't reach them) appear
      scale_x_continuous(
        limits = range(x_breaks[[idx]]),
        breaks = x_breaks[[idx]],
        labels = lab_fun,
        expand = expansion(add = c(0, 0.01)) 
      ) +
      
      scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        expand = c(0, 0)
      ) +
      
      labs(
        x      = idx,
        y      = if (i == 1) "Mean Absolute Bias" else NULL,
        colour = legend_title
      ) +
      theme_minimal(base_size = 18) +
      theme(
        panel.grid.minor   = element_blank(),
        panel.grid.major   = element_line(color = "gray80"),
        axis.title.x       = element_text(size = 18),
        axis.text.x        = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y       = element_text(size = 18),
        legend.position    = "bottom",
        legend.title       = element_text(size = 18),
        legend.text        = element_text(size = 18),
        plot.title         = element_text(size = 18, face = "bold", hjust = 0.5)
      )
    
    # strip left‐axis on all but first panel
    if (i != 1) {
      p <- p + theme(
        axis.title.y  = element_blank(),
        axis.text.y   = element_blank(),
        axis.ticks.y  = element_blank()
      )
    }
    
    panels[[i]] <- p
  }
  panels
}

# make crop row
crop_panels <- make_panels(
  df             = df_to_plot_crop,
  aes_colour     = "crop",
  colour_vals    = crop_cols,
  legend_title   = "Crop Types"
)

# make soil row
soil_panels <- make_panels(
  df             = df_to_plot_soil,
  aes_colour     = "soil",
  colour_vals    = soil_cols,
  legend_title   = "Soil Types"
)

# arrange each row with common legend
row1 <- ggarrange(
  plotlist      = crop_panels,
  ncol          = 3,
  common.legend = TRUE,
  legend        = "bottom",
  align         = "v"
)
row2 <- ggarrange(
  plotlist      = soil_panels,
  ncol          = 3,
  common.legend = TRUE,
  legend        = "bottom",
  align         = "v"
)

# stack the two rows
final_plot <- ggarrange(
  row1, row2,
  ncol    = 1,
  heights = c(1, 1)
)

# show & save
print(final_plot)
ggsave(
  filename = paste0(path_to_plots, "mab/by_crops_and_soils.png"),
  plot     = final_plot,
  width    = 15,
  height   = 12,
  dpi      = 300
)

###############################################################
###############################################################
###############################################################
###############################################################
# Create the fr_range column
results_fr <- results_fr %>%
  mutate(fr_range_4groups = cut(act_fr,
                                breaks = c(0, 0.15, 0.3, 0.75, 1),
                                labels = c("0-0.15", "0.15-0.3", "0.3-0.75", "0.75-1"),
                                include.lowest = TRUE))

# fr_table_4groups <- results_fr %>% 
#   group_by(index_name, RWC, crop, fr_range_4groups) %>% 
#   summarise(mae = round(mean(fr_ae), 2), 
#             normalized_index_range = paste0(round(min(index_normalized), 2), "-", round(max(index_normalized), 2)))
# 
# view(fr_table_4groups)


error_summary <- results_fr %>%
  group_by(index_name, soil, RWC, crop, fr_range_4groups) %>%
  summarise(
    mae = mean(fr_ab)
  )

df_to_plot <- error_summary

df_to_plot$RWC <- round(as.numeric(as.character(df_to_plot$RWC)), 1)

df_to_plot$RWC <- as.character(df_to_plot$RWC)

custom_colors <- c("Canola" = "#63ccb7", "Garbanzo Beans" = "#06728e", "Peas" = "#ff5643", 
                   "Wheat Norwest Duet" = "#06293c", "Wheat Pritchett" = "#8a5a44")  
# Set the desired order as factor levels
desired_order <- c("NDTI", "SINDRI", "CAI")
df_to_plot$index_name <- factor(df_to_plot$index_name, levels = desired_order)

# to_analyse <- df_to_plot %>% dplyr::filter(soil == "Athena")

write.csv(df_to_plot, file = paste0(path_to_plots, "rwc_mae_athena_wheat.csv"), row.names = FALSE)


# library(cowplot)
# 
# for (sl in unique(df_to_plot$soil)) {
#   plots_list <- list()
#   
#   for (idx_ in levels(df_to_plot$index_name)) {
#     print(idx_)
#     
#     filtered <- df_to_plot %>%
#       filter(soil == sl, index_name == idx_)
#     
#     p <- ggplot(filtered, aes(x = RWC, y = mae, color = crop, group = crop)) +
#       geom_point(size = 2, alpha = 0.7) +
#       geom_line(size = 1, alpha = 0.7) +
#       scale_color_manual(name = "Crop residue", values = custom_colors) +
#       labs(x = "RWC", y = "MAE") +
#       ylim(0, 1) +
#       coord_fixed(ratio = 6) +
#       theme_minimal() +
#       theme(
#         legend.text = element_text(size = 18),  
#         legend.title = element_text(size = 14),
#         legend.key.size = unit(1.5, "cm"), 
#         legend.position = "none",  
#         panel.spacing = unit(0, "lines"),
#         axis.title = element_text(size = base_size * 1),
#         axis.text = element_text(size = base_size * 1, color = "black"),
#         panel.background = element_rect(fill = "white", colour = "white"),
#         plot.background = element_rect(fill = "white", colour = "white"),
#         panel.grid = element_blank(),
#         axis.ticks = element_line(color = "black"),
#         axis.line = element_line(color = "black"),
#         plot.title = element_blank(),
#         # Remove strip.text = element_blank() to show facet labels
#         axis.text.x = element_text(size = base_size * 0.8, color = "black"),
#         axis.text.y = element_text(size = base_size * 0.8, color = "black"),
#         plot.margin = unit(c(0.5, 5, 0.5, 0.5), "cm"),
#         strip.placement = "outside",               # Place strips outside the axes
#         strip.background = element_blank(),        # Optional: Remove strip background
#         strip.text.x = element_text(size = 14) ,
#         panel.spacing.y = unit(1, "lines")# Optional: Adjust strip text size
#       ) +
#       facet_wrap(~fr_range_4groups, ncol = 4, strip.position = "bottom")
#     
#     
#     plots_list[[idx_]] <- p
#   }
#   
#   # Extract the legend
#   legend <- get_legend(
#     ggplot(filtered, aes(x = RWC, y = mae, color = crop, group = crop)) +
#       geom_point(size = 3, alpha = 0.7) +
#       geom_line(size = 2, alpha = 0.7) +
#       scale_color_manual(name = "Crop residue", values = custom_colors) +
#       theme_minimal() +
#       theme(legend.position = "right")  # Keep legend visible for extraction
#   )
#   
#   # Remove legends from all individual plots
#   plots_without_legend <- lapply(plots_list, function(plot) plot + theme(legend.position = "none"))
#   
#   # Arrange the plots in a grid
#   combined_plot <- plot_grid(plotlist = plots_without_legend, ncol = 1, align = 'hv')
#   
#   
#   # # Create a label row for the fr_range_4groups
#   # label_text <- paste(unique(filtered$fr_range_4groups), collapse = " | ")  # Combine group labels
#   # labels_row <- ggdraw() +
#   #   draw_label(label_text, fontface = "bold", x = 0.5, y = 0.5, size = 14, hjust = 0.5)
#   # 
#   # Combine the grid with the legend, positioning the legend manually
#   final_plot <- ggdraw() +
#     draw_plot(combined_plot, 0, 0, 0.8, 1) +    # Place the plots in the left 80% of the canvas
#     draw_plot(legend, 0.62, 0.1, 0.2, 0.8)      # Place the legend in the remaining 20% (adjust coordinates)
#     # draw_plot(labels_row, 0, 0, 1, 0.1) # Labels at the bottom
#   
#   
#   # Print the final combined plot
#   print(final_plot)
#   
#   # Save the final plot
#   ggsave(paste0(path_to_plots, 'MAE/rwc_effect_wheat_athena/', sl, '.png'), 
#          final_plot, width = 12, height = 10, dpi = 300)
# }







##############################
##############################
######## put it in a facet

library(ggplot2)
library(cowplot)
library(dplyr)


for (sl in unique(df_to_plot$soil)) {
  filtered <- df_to_plot %>%
    filter(soil == sl)
  # c("Canola" = "#63ccb7", "Garbanzo Beans" = "#06728e", "Peas" = "#ff5643", 
  #   "Wheat Norwest Duet" = "#06293c", "Wheat Pritchett" = "#8a5a44")
  # 
  p <- ggplot(filtered, aes(x = RWC, y = mae, color = crop, group = crop)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_line(size = 1, alpha = 0.7) +
    scale_color_manual(
      name = "Crop residue",
      values = custom_colors,
      labels = c("Wheat Norwest Duet" = "Wheat",
                 "Garbanzo Beans" = "Garbanzo Beans",
                 "Peas" = "Peas",
                 "Canola" = "Canola")) +
    labs(x = "RWC", y = "Mean Absolute Bias") +
    ylim(0, 1) +
    coord_fixed(ratio = 6) +
    theme_minimal() +
    theme(
      legend.text = element_text(size = 18),  
      legend.title = element_text(size = 18),
      legend.key.size = unit(1.5, "cm"), 
      legend.position = "none",  
      panel.spacing = unit(0, "lines"),
      axis.title = element_text(size = base_size * 1.2),
      axis.text = element_text(size = base_size * 1.2, color = "black"),
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background = element_rect(fill = "white", colour = "white"),
      panel.grid = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.line = element_line(color = "black"),
      plot.title = element_blank(),
      axis.text.x = element_text(size = base_size * 1, color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = base_size * 1, color = "black"),
      plot.margin = unit(c(0.5, 5, 0.5, 0.5), "cm"),
      strip.placement = "outside",               # Place strips outside the axes
      strip.background = element_blank(),        # Remove strip background
      strip.text.x = element_text(size = 14),    # Adjust strip text size for columns
      strip.text.y = element_text(size = 14),    # Adjust strip text size for rows
      strip.switch.pad.grid = unit(0, "cm"),
      panel.spacing.y = unit(1, "lines"), # Remove padding between strips and axis
    ) +
    facet_grid(
      rows = vars(index_name),
      cols = vars(fr_range_4groups),
      switch = "both",
      switch = "y")
  
  # Extract the legend
  legend <- get_legend(
    ggplot(filtered, aes(x = RWC, y = mae, color = crop, group = crop)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_line(size = 2, alpha = 0.7) +
      scale_color_manual(name = "Crop residue", values = custom_colors, labels = c("Wheat Norwest Duet" = "Wheat",
                                                                                   "Garbanzo Beans" = "Garbanzo Beans",
                                                                                   "Peas" = "Peas",
                                                                                   "Canola" = "Canola")) +
      theme_minimal() +
      theme(legend.position = "right",
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14)  # Adjust legend title size
      )  # Keep legend visible for extraction
  )
  
  # Remove legend from the main plot
  p <- p + theme(legend.position = "none")
  
  # Combine the plot and the legend
  final_plot <- ggdraw() +
    draw_plot(p, 0, 0, 0.8, 1) +    # Place the plot in the left 80% of the canvas
    draw_plot(legend, 0.63, 0.1, 0.2, 0.8)      # Place the legend in the remaining 20% (adjust coordinates)
  
  # Print the final combined plot
  print(final_plot)
  
  # Save the final plot
  ggsave(paste0(path_to_plots, 'MAE/rwc_effect/agu/rwc_mae_wheat_', sl, '.png'), 
         final_plot, width = 14, height = 10, dpi = 300)
}

##############################
##############################
######## for AGU

library(ggplot2)
library(cowplot)
library(dplyr)

df_to_plot$fr_range_4groups <- paste0(df_to_plot$fr_range_4groups, "%")

# Assuming your dataframe is named df
df_to_plot <- df_to_plot %>%
  group_by(index_name, crop, fr_range_4groups, RWC) %>%
  filter(mae == min(mae)) %>%
  ungroup()


for (sl in unique(df_to_plot$soil)) {
  filtered <- df_to_plot %>%
    filter(soil == sl)
  # c("Canola" = "#63ccb7", "Garbanzo Beans" = "#06728e", "Peas" = "#ff5643", 
  #   "Wheat Norwest Duet" = "#06293c", "Wheat Pritchett" = "#8a5a44")
  # 
  p <- ggplot(filtered, aes(x = RWC, y = mae, color = crop, group = crop)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_line(size = 1, alpha = 0.7) +
    scale_color_manual(
      name = "Crop residue",
      values = custom_colors,
      labels = c("Wheat Norwest Duet" = "Wheat",
                 "Garbanzo Beans" = "Garbanzo Beans",
                 "Peas" = "Peas",
                 "Canola" = "Canola")) +
    labs(x = "RWC", y = "Mean Absolute Bias") +
    ylim(0, 1) +
    coord_fixed(ratio = 6) +
    theme_minimal() +
    theme(
      legend.text = element_text(size = 18, color = "black"),
      legend.title = element_text(size = 18),
      legend.key.size = unit(1.5, "cm"),
      legend.position = "right",
      panel.spacing = unit(0, "lines"),
      axis.title = element_text(size = base_size * 1.2, color = "black"),
      axis.text = element_text(size = base_size * 1.2, color = "black"),
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.line = element_line(color = "black"),
      plot.title = element_blank(),
      axis.text.x = element_text(size = base_size * 1, color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = base_size * 1, color = "black"),
      plot.margin = unit(c(0.5, 5, 0.5, 0.5), "cm"),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.position = "right",
      strip.text.x = element_text(size = 14, color = "black"),
      strip.text.y = element_text(size = 14, color = "black"),
      strip.switch.pad.grid = unit(0, "cm"),
      panel.spacing.y = unit(1, "lines")
    ) +
    facet_grid(rows = vars(index_name), cols = vars(fr_range_4groups))
  
  # Print the plot
  print(p)
  
  # Save the final plot
  ggsave(paste0(path_to_plots, 'MAE/rwc_effect/agu/rwc_mae_wheat_', sl, '.png'), 
         p, width = 14, height = 10, dpi = 300)
}

###########################
#       Plot Error by soil
###########################

fresh_df_wheat <- fresh_df %>% 
  dplyr::filter(crop == "Wheat Norwest Duet")
# Fit on driest wheat on Athena
results <- fresh_df_wheat %>%
  group_by(index_name) %>%
  group_modify(~ fit_on_dry_wheat_Athena(.x))


results_fr <-  results
results_fr$act_fr <- results_fr$Fraction_Residue_Cover
results_fr$pred_fr <- results_fr$Predicted_Fraction_Residue_Cover
results_fr$fr_ae <- abs(results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover)


# Perform min-max normalization for each index_name group
results_fr <- results_fr %>%
  group_by(index_name) %>%
  mutate(index_normalized = (index - min(index)) / (max(index) - min(index)))


# # Create the index_range column based on index_normalized values
# results_fr <- results_fr %>%
#   mutate(index_range_3groups = cut(index_normalized,
#                                    breaks = c(0, 0.15, 0.3, 1),
#                                    labels = c("0-0.15", "0.15-0.3", "0.3-1"),
#                                    include.lowest = TRUE))

# # Create the index_range column based on index_normalized values
# results_fr <- results_fr %>%
#   mutate(index_range_4groups = cut(index_normalized,
#                            breaks = c(0, 0.25, 0.5, 0.75, 1),
#                            labels = c("0-0.25", "0.25-0.5", "0.5-0.75", "0.75-1"),
#                            include.lowest = TRUE))

# Create the fr_range column
results_fr <- results_fr %>%
  mutate(fr_range_4groups = cut(act_fr,
                                breaks = c(0, 0.15, 0.3, 0.75, 1),
                                labels = c("0-0.15", "0.15-0.3", "0.3-0.75", "0.75-1"),
                                include.lowest = TRUE))

fr_table_4groups <- results_fr %>% 
  group_by(index_name, RWC, soil, fr_range_4groups) %>% 
  summarise(mae = round(mean(fr_ae), 2), 
            normalized_index_range = paste0(round(min(index_normalized), 2), "-", round(max(index_normalized), 2)))




view(fr_table_4groups)

error_summary <- results_fr %>%
  group_by(index_name, crop, RWC, soil) %>%
  summarise(
    mae = mean(fr_ae)
  )

df_to_plot <- error_summary
df_to_plot$RWC <- as.character(df_to_plot$RWC)

custom_colors <- c("Athena" = "#582f0e", "Bagdad" = "#7f4f24", "Benwy"= "#936639",
                   "Broadax"= "#a68a64", "Endicott"= "#b6ad90", "Lance"= "#c2c5aa",
                   "Mondovi 1" = "#a4ac86", "Mondovi 2" = "#656d4a", "Oxy" = "#414833",
                   "Palouse" = "#333d29", "Ritzville" = "#774936", "Shano" =  "#580c1f")  

for  (crp in unique(df_to_plot$crop)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    
    filtered <- df_to_plot %>%
      filter(crop == crp, index_name == idx_)
    p <- ggplot(filtered, aes(x = RWC, y = mae, color = soil, group = soil)) +
      geom_point(size = 2, alpha = 0.7) +       # Plot all points
      geom_line(size = 1, alpha = 0.7) +        # Add lines connecting points for the same crop
      scale_color_manual(name = "Soil", values = custom_colors) +
      labs(x = "RWC", y = "MAE") +
      theme_minimal() +   
      theme(legend.position = "right",
            legend.title = element_text(size = base_size), # Legend title larger than base size
            legend.text = element_text(size = base_size ), # Legend text at base size
            axis.title = element_text(size = base_size ), # Axis titles larger than base size
            axis.text = element_text(size = base_size , color = "black"), # Axis text at base size
            panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
            plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_text(hjust = 0.5, size = 14),
            strip.text = element_blank()) +
      ggtitle(filtered$index_name[1]) +
      coord_fixed(ratio = 7) 
    ggsave(paste0(path_to_plots, 'MAE/by_soil/', crp, "_", filtered$index_name[1], '.png'), 
           p, width = 7, height = 7, dpi = 300)
  }
}


###########################
#       Plot Error by age
###########################

fit_on_dry_fresh <- function(data) {
  # Find the minimum RWC in this subset
  min_rwc_data <- data %>% filter(!str_detect(crop, "Weathered") & (RWC == min(RWC)))
  
  # Fit linear model to the data with minimum RWC
  model <- lm(Fraction_Residue_Cover ~ index, data = min_rwc_data)
  
  # Predict Fraction_Residue_Cover for the entire group's data
  predictions <- predict(model, newdata = data)
  
  # Add predictions to the data
  data$Predicted_Fraction_Residue_Cover <- predictions
  
  # Calculate predictions for the minimum RWC data only
  predictions_min_rwc <- predict(model, newdata = min_rwc_data)
  
  # Since all entries with minimum RWC should have the same predicted value, take the first
  min_rwc_predicted_value <- predictions_min_rwc[1]
  
  # Calculate error ratio for all entries in the data
  data$error_ratio <- data$Predicted_Fraction_Residue_Cover / min_rwc_predicted_value
  
  # Add slope and intercept from the model
  data$Slope <- coef(model)[["index"]]
  data$Intercept <- coef(model)[["(Intercept)"]]
  
  return(data)
}

canola <- c("Canola", "Weathered Canola")
wheat <- c("Wheat Norwest Duet", "Weathered Wheat")

fresh_aged_wheat <- df %>% dplyr::filter(crop %in% wheat)
fresh_aged_canola <- df %>% dplyr::filter(crop %in% canola)

fresh_aged_wheat <- fresh_aged_wheat %>% dplyr::filter(soil == "Athena")
fresh_aged_canola <- fresh_aged_canola %>% dplyr::filter(soil == "Athena")



# Fit on driest fresh
results_wheat <- fresh_aged_wheat %>%
  group_by(index_name, soil) %>%
  group_modify(~ fit_on_dry_fresh(.x))

# Fit on driest 
results_canola <- fresh_aged_canola %>%
  group_by(index_name, soil) %>%
  group_modify(~ fit_on_dry_fresh(.x))



results <- results_wheat
results_fr <-  results
results_fr$act_fr <- results_fr$Fraction_Residue_Cover
results_fr$pred_fr <- results_fr$Predicted_Fraction_Residue_Cover
results_fr$fr_ae <- abs(results_fr$Predicted_Fraction_Residue_Cover - results_fr$Fraction_Residue_Cover)

# Perform min-max normalization for each index_name group
results_fr <- results_fr %>%
  group_by(index_name) %>%
  mutate(index_normalized = (index - min(index)) / (max(index) - min(index)))

# results_fr <- results_fr %>%
#   mutate(index_range_3groups = cut(index_normalized,
#                                    breaks = c(0, 0.15, 0.3, 1),
#                                    labels = c("0-0.15", "0.15-0.3", "0.3-1"),
#                                    include.lowest = TRUE))

# Create the fr_range column
results_fr <- results_fr %>%
  mutate(fr_range_4groups = cut(act_fr,
                                breaks = c(0, 0.15, 0.3, 0.75, 1),
                                labels = c("0-0.15", "0.15-0.3", "0.3-0.75", "0.75-1"),
                                include.lowest = TRUE))

fr_table_4groups <- results_fr %>% 
  group_by(index_name, RWC, crop, fr_range_4groups) %>% 
  summarise(mae = round(mean(fr_ae), 2), 
            normalized_index_range = paste0(round(min(index_normalized), 2), "-", round(max(index_normalized), 2)))

view(fr_table_4groups)

to_analyse <- df_to_plot %>% dplyr::filter(soil == "Athena")

write.csv(fr_table_4groups, file = paste0(path_to_plots, "age_mae_wheat.csv"), row.names = FALSE)

error_summary <- results_fr %>%
  group_by(index_name, soil, RWC, crop) %>%
  summarise(
    mae = mean(fr_ae)
  )


custom_colors <- c(
  "Canola" = "#fe7f2d", "Weathered Canola" = "#fe7f2d",
  "Wheat Norwest Duet" = "#619b8a", "Weathered Wheat" = "#619b8a"
)  

# Define the shapes: default shapes for normal crops, different shapes for weathered crops
custom_shapes <- c("Canola" = 16, "Weathered Canola" = 17, 
                   "Wheat Norwest Duet" = 16, "Weathered Wheat" = 17)

df_to_plot <- error_summary
for (sl in unique(df_to_plot$soil)) {
  for (idx_ in unique(df_to_plot$index_name)) {
    
    filtered <- df_to_plot %>%
      filter(soil == sl, index_name == idx_)
    p <- ggplot(filtered, aes(x = RWC, y = mae, color = as.factor(crop), shape = as.factor(crop))) +
      geom_point(size = 2, alpha = 0.7) +       # Plot all points
      geom_line(size = 1, alpha = 0.7) +        # Add lines connecting points for the same crop
      scale_color_manual(name = "Crop residue", values = custom_colors) +
      scale_shape_manual(name = "Crop residue", values = custom_shapes) +
      
      
      labs(x = "RWC", y = "MAE") +
      theme_minimal() +   
      theme(legend.position = "right",
            legend.title = element_text(size = base_size), # Legend title larger than base size
            legend.text = element_text(size = base_size ), # Legend text at base size
            axis.title = element_text(size = base_size ), # Axis titles larger than base size
            axis.text = element_text(size = base_size , color = "black"), # Axis text at base size
            panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
            plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
            panel.grid = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.line = element_line(color = "black"),
            legend.key.size = unit(0.5, "cm"),
            plot.title = element_text(hjust = 0.5, size = 14),
            strip.text = element_blank()) +
      ggtitle(filtered$index_name[1]) +
      coord_fixed(ratio = 2) 
    ggsave(paste0(path_to_plots, 'MAE/by_age/', sl, "_", filtered$index_name[1], '.png'), 
           p, width = 15, height = 7, dpi = 300)
  }
}


####################################
####################################
################## Error by fr class

# Assuming your dataframe is named df
results_fr <- results_fr %>%
  mutate(fr_class = case_when(
    Fraction_Residue_Cover >= 0 & Fraction_Residue_Cover <= 0.15 ~ "0-15%",
    Fraction_Residue_Cover > 0.15 & Fraction_Residue_Cover <= 0.3 ~ "16-30%",
    Fraction_Residue_Cover > 0.3 ~ ">30%"
  ))


# df_to_plot <- results_fr %>% 
#   group_by(soil, index_name, RWC)

df_to_plot <- results_fr %>% dplyr::filter((index_name == "NDTI" & soil == "Athena"))

# Ensure fr_class is a factor and set the desired order
df_to_plot <- df_to_plot %>%
  mutate(fr_class = factor(fr_class, levels = c("0-15%", "16-30%", ">30%")))


# Assuming df is your dataframe and the bins for 'index' are defined using cut()
df_to_plot <- df_to_plot %>%
  mutate(index_bin = cut(round(index, 1), breaks = 5)) # Adjust 'breaks' to define the number of bins for index



# Adjusted plot: grouped boxplots
ggplot(df_to_plot, aes(x = index_bin, y = fr_ae, fill = fr_class, colour = fr_class)) +
  geom_boxplot(lwd = 0.5, outlier.size = 1, outlier.shape = 16) +  # Regular boxplot (no stacking)
  scale_fill_manual(values = c("0-15%" = "red", "16-30%" = "green", ">30%" = "blue")) +  # Customize colors
  scale_colour_manual(values = c("0-15%" = "red", "16-30%" = "green", ">30%" = "blue")) +  # Customize outlier colors
  labs(x = "Index Bins", y = "Fr absolute error", fill = "") +  # Labels
  theme_minimal(base_size = 15) +  # Adjust theme
  theme(legend.position = "right",
        legend.title = element_text(size = base_size), # Legend title larger than base size
        legend.text = element_text(size = base_size), # Legend text at base size
        axis.title = element_text(size = base_size ), # Axis titles larger than base size
        axis.text = element_text(size = 7 , color = "black"), # Axis text at base size
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", colour = "white"), # Set plot background to white
        panel.grid = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(hjust = 0.5, size = 14))+
  facet_wrap(~ RWC, scales = "free_y", labeller = labeller(RWC = function(x) paste("RWC =", x))) +
  guides(colour = "none")



####################################
####################################
############# 

# Loop over each unique RWC value and generate the corresponding plot
unique_rwc_values <- unique(df_to_plot$RWC)

for (rwc_value in unique_rwc_values) {
  
  # Filter the dataframe for the current RWC level
  df_filtered <- df_to_plot %>% dplyr::filter(RWC == rwc_value)
  
  # Adjusted plot: grouped boxplots with data points colored by 'crop'
  p <- ggplot(df_filtered, aes(x = index_bin, y = fr_ae, fill = fr_class, group = interaction(index_bin, fr_class))) +
    geom_boxplot(lwd = 0.5, outlier.size = 1, outlier.shape = 16,
                 position = position_dodge(width = 0.8)) +  # Regular boxplot
    geom_jitter(aes(color = crop), size = 3,
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +  # Jittered points
    scale_fill_manual(values = c("0-15%" = "red", "16-30%" = "green", ">30%" = "blue")) +  # Customize fill colors
    scale_color_manual(values = c("Canola" = "orange", "Garbanzo Beans" = "purple", "Peas" = "brown", "Wheat Norwest Duet" = "pink")) +  # Customize point colors
    labs(x = "Index Bins", y = "Fr absolute error", fill = "Fr class", color = "Crop residue") +  # Labels
    theme_minimal(base_size = 15) +  # Adjust theme
    theme(legend.position = "right",
          legend.title = element_text(size = 15),  # Legend title size
          legend.text = element_text(size = 15),   # Legend text size
          axis.title = element_text(size = 15),    # Axis title size
          axis.text = element_text(size = 16, color = "black"),  # Axis text size
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),  # White background
          plot.background = element_rect(fill = "white", colour = "white"),
          panel.grid = element_blank(),
          axis.ticks = element_line(color = "black"),
          axis.line = element_line(color = "black"),
          legend.key.size = unit(1, "cm"),
          plot.title = element_text(hjust = 0.5, size = 14))
  
  # Define the save location and filename
  file_name <- paste0(path_to_plots, 'MAE/stackedbox_by_crop/', "_", df_filtered$index_name[1], '_RWC_', rwc_value, '.png')
  
  # Save the plot
  ggsave(file_name, p, width = 15, height = 7, dpi = 300)
  
}







################################################
################################################
############ 
############   statistical test
############


df_to_plot$crop <- factor(df_to_plot$crop)

df_to_plot$crop <- relevel(df_to_plot$crop, ref = "Wheat Norwest Duet")

df_to_plot_ <- df_to_plot %>% dplyr::filter(RWC == 0)

write.csv(df_to_plot, paste0(path_to_plots, "df_crop_statTest.csv"), row.names = FALSE)

# Fit the linear model with interaction
model_interaction <- lm(Fraction_Residue_Cover ~ index * crop, data = df_to_plot_)


# Fit the reduced model (without interaction)
model_reduced <- lm(Fraction_Residue_Cover ~ index + crop, data = df_to_plot_)


# Compare the models using ANOVA
anova(model_reduced, model_interaction)


summary(model_interaction)




## install.packages("emmeans")   # run once if you don’t have it
library(emmeans)

## 1.  Get one slope (“trend”) per crop -----------------------------
#   var = "index" tells emmeans we want the coefficient for index
slopes <- emtrends(model_interaction,
                   specs = ~ crop,        # one row per crop
                   var   = "index")

## 2.  Do *all‑pair* comparisons of those slopes -------------------
pairwise_slopes  <- pairs(slopes, adjust = "tukey")   # Tukey‑HSD p‑value adjust
pairwise_slopes
