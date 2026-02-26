library(tidyverse)
library(dplyr)
library(ggplot2)


path_to_data <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                       'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                       'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')

path_to_plots <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                        'OneDrive-WashingtonStateUniversity(email.wsu.edu)/Ph.D/',
                        'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/')

# path_to_data <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                        'Projects/Soil_Residue_Spectroscopy/Data/10nm_resolution/')
# 
# path_to_plots <- paste0('/home/amnnrz/OneDrive - a.norouzikandelati/Ph.D/',
#                         'Projects/Soil_Residue_Spectroscopy/Plots/10nm_resolution/')

Residue_Median <- read.csv(paste0(path_to_data, 
                                  "Residue.csv"),
                           header = TRUE, row.names = NULL)
Residue_Median <- Residue_Median[-c(1, 8)]
Residue_Median <- dplyr::filter(Residue_Median, Wvl >=500)

Soil_Median <- read.csv(paste0(path_to_data, 
                               "Soil.csv"),
                        header = TRUE, row.names = NULL)
Soil_Median <- Soil_Median[-c(1, 8)]
Soil_Median <- dplyr::filter(Soil_Median, Wvl >=500)

Residue_Median <- Residue_Median %>%
  dplyr::rename(Type = Crop)

Soil_Median <- Soil_Median %>%
  dplyr::rename(Type = Soil)


Residue_Median <- Residue_Median %>%
  group_by(Type) %>% 
  filter(RWC == min(RWC))

Soil_Median <- Soil_Median %>%
  group_by(Type) %>% 
  filter(RWC == min(RWC))


Residue_Median <- Residue_Median %>%
  mutate(Sample = recode(Sample, "Crop Residue" = "Residue"))

Residue <- Residue_Median
Soil <- Soil_Median

res_wide <- Residue %>%
  pivot_wider(names_from = Wvl, values_from = Reflect) 

soil_wide <- Soil %>%
  pivot_wider(names_from = Wvl, values_from = Reflect) 

###############################################################
###############################################################
#####
# mix one crop, soil and RWC range 
#####
###############################################################
###############################################################

# # Filter by one crop and one soil
# crop_filtered <- residue_df %>% 
#   filter(Type == 'Peas')
# soil_filtered <- soil_df %>% 
#   filter(Type == 'Athena')
# 
# # filter by one RWC range
# rwc_crop <- dplyr::filter(crop_filtered, RWC_range == '0-0.25')
# rwc_soil <- dplyr::filter(soil_filtered, RWC_range == '0-0.25')
# 
# cropScans <- unique(rwc_crop$Scan)
# soilScans <- unique(rwc_soil$Scan)
# 
# mixed_dataframe <- data.frame()
# 
# for (i in cropScans){
#   for (j in soilScans){
#     
#     fractions <- sort(runif(10, min = 0, max = 1))
#     
#     cropReflect <- dplyr::filter(rwc_crop, Scan == i) %>% 
#       select("500":ncol(rwc_crop))
#     
#     soilReflect <- dplyr::filter(rwc_soil, Scan == j)%>% 
#       select("500":ncol(rwc_soil))
#     
#     Rr <- lapply(fractions, function(fr) as.numeric(cropReflect) * fr)
#     Rs <- lapply(fractions, function(fr) as.numeric(soilReflect) * (1-fr))
#     
#     Rmix <- mapply(FUN = `+`, Rr, Rs, SIMPLIFY = FALSE)
#     
#     mixed_df <- as.data.frame(do.call(rbind, Rmix))
#     colnames(mixed_df) <- names(rwc_crop)[6:ncol(rwc_crop)]
#     
#     mixed_df$cropType <- rwc_crop$Type[1]
#     mixed_df$soilType <- rwc_soil$Type[1]
#     mixed_df$cropRWC <- rwc_crop$RWC[1]
#     mixed_df$soilRWC <- rwc_soil$RWC[1]
#     mixed_df$Fr <- sort(runif(10, min = 0, max = 1))
#     mixed_df$cropScan <- i
#     mixed_df$soilScan <- j
#     
#     mixed_df <- mixed_df %>% 
#       select("cropType", "soilType", "cropRWC", "soilRWC", "Fr", "cropScan", 
#       "soilScan", everything())
#     
#     mixed_dataframe <- rbind(mixed_dataframe, mixed_df)
#   }
# }

###############################################################
###############################################################
#####
# mix all crops, soils and RWC ranges 
#####
###############################################################
###############################################################
# Filter by one crop and one soil
crops = unique(res_wide$Type)
soils = unique(soil_wide$Type)

fractions <- seq(0, 1, by = 0.1)
mixed_dataframe <- data.frame()

# Filter by one crop and one soil
crp <-  'Peas'
sl <- 'Athena'

for (crp in crops){
  for (sl in soils){
    print(paste0(crp, "_", sl))
    
    crop_filtered <- res_wide %>% 
      filter(Type == crp) %>% 
      ungroup()
    
    soil_filtered <- soil_wide %>% 
      filter(Type == sl) %>% 
      ungroup()
    
    
    
    cropReflect <- crop_filtered %>%  dplyr::select("500":ncol(crop_filtered))
    
    soilReflect <- soil_filtered %>%  dplyr::select("500":ncol(soil_filtered))
    
    Rmix <- lapply(
      fractions, function(fr) {
        df <- (cropReflect * fr) + (soilReflect * (1-fr))
        df <- cbind(Fraction = fr, df)
        df <- cbind(
          Mix = paste0(crop_filtered$Type, "_", soil_filtered$Type), df)
        return(df)
      })
    
    mixed_df <- data.frame()
    mixed_df <- as.data.frame(do.call(rbind, Rmix))
    mixed_df <- cbind(Crop = crp, mixed_df)
    mixed_df <- cbind(Soil = sl, mixed_df)
    
    mixed_dataframe <- rbind(
      mixed_dataframe, mixed_df)
  }
}

# # Standard Normal Variate (SNV) Transformation
# snv_transform <- function(matrix) {
#   snv_transformed <- apply(matrix, 1, function(row) {
#     row_mean <- mean(row)
#     row_sd <- sd(row)
#     (row - row_mean) / row_sd
#   })
#   return(t(snv_transformed)) # Transpose to maintain original orientation
# }

# spectra_matrix <- mixed_dataframe %>% dplyr::select("1500":ncol(mixed_dataframe))
# spectra_matrix <- as.matrix(spectra_matrix)
# snv_transformed_matrix <- snv_transform(spectra_matrix)
# mixed_dataframe_firstCols <- mixed_dataframe %>%  dplyr::select("Soil":6)
# mixed_dataframe <- cbind(mixed_dataframe_firstCols, snv_transformed_matrix)

# Update path
path_to_data_new <- paste0('/Users/aminnorouzi/Library/CloudStorage/',
                            'OneDrive-WashingtonStateUniversity(email.wsu.edu)/',
                            'Ph.D/Projects/Soil_Residue_Spectroscopy/Data/00/')


mix_long <- mixed_dataframe %>%
  pivot_longer(
    cols = `500`:`2450`,      # all spectral bands
    names_to = "Wvl",         # name of the new wavelength column
    values_to = "Reflect"     # name of the new reflectance value column
  )

write.csv(mix_long, paste0(path_to_data_new, "mixed_spectra_dry.csv"), row.names = FALSE)




